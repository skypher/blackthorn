;;;; Blackthorn -- Lisp Game Engine
;;;;
;;;; Copyright (c) 2007-2009, Elliott Slaughter <elliottslaughter@gmail.com>
;;;;
;;;; Permission is hereby granted, free of charge, to any person
;;;; obtaining a copy of this software and associated documentation
;;;; files (the "Software"), to deal in the Software without
;;;; restriction, including without limitation the rights to use, copy,
;;;; modify, merge, publish, distribute, sublicense, and/or sell copies
;;;; of the Software, and to permit persons to whom the Software is
;;;; furnished to do so, subject to the following conditions:
;;;;
;;;; Except as contained in this notice, the name(s) of the above
;;;; copyright holders shall not be used in advertising or otherwise to
;;;; promote the sale, use or other dealings in this Software without
;;;; prior written authorization.
;;;;
;;;; The above copyright notice and this permission notice shall be
;;;; included in all copies or substantial portions of the Software.
;;;;
;;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;;; NONINFRINGEMENT.  IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;;;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;;; DEALINGS IN THE SOFTWARE.
;;;;

(in-package :blackthorn-user)

;;;
;;; System paths
;;;

(defvar *resource-directory-pathname*
  "The root directory which contains resources for the game. Normally, this
   is equivalent to *default-pathname-defaults*, but in some situations,
   e.g. Mac OS X applications, this refers to a different location.")

(defvar *database-pathname* (make-pathname :directory '(:relative "game.db"))
  "The directory to use as the game database. Considered relative to
   *resource-directory-pathname*.")

(defvar *save-file-pathname* "game.btg"
  "The save file pathname. Considered relative to
   *resource-directory-pathname*.")

(defun setup-paths ()
  (setf *resource-directory-pathname* *default-pathname-defaults*)
  #+darwin
  (let* ((executable
          (truename
           (merge-pathnames
            (make-pathname :directory
                           (pathname-directory (command-line-executable))))))
         (resources
          (merge-pathnames
           (make-pathname :directory '(:relative :up "Resources"))
           executable)))
    (when (fad:directory-exists-p resources)
      (setf *default-pathname-defaults* executable
            *resource-directory-pathname* (truename resources))))
  (setf *database-pathname*
        (merge-pathnames *database-pathname* *resource-directory-pathname*)
        *save-file-pathname*
        (merge-pathnames *save-file-pathname*
                         *resource-directory-pathname*)))

;;;
;;; Command-line option parsing
;;;

(defun command-line-executable ()
  "Returns the path to the executable being run."
  #+allegro (car (sys:command-line-arguments))
  #+clisp (aref (ext:argv) 0)
  #+clozure (car ccl:*command-line-argument-list*)
  #+ecl (car (ext:command-args))
  #+sbcl (car sb-ext:*posix-argv*)
  #-(or allegro clisp clozure ecl sbcl)
  (error "Don't know how to get command line args."))

(defun command-line-arguments ()
  "Returns the command-line arguments given to the executable, except for the
   name of the executable itself."
  #+allegro (cdr (sys:command-line-arguments))
  #+clisp ext:*args*
  #+ecl (cdr (ext:command-args))
  #+clozure (cdr ccl:*command-line-argument-list*)
  #+sbcl (cdr sb-ext:*posix-argv*)
  #-(or allegro clisp clozure ecl sbcl)
  (error "Don't know how to get command line args."))

(defun cli-options ()
  (list (make-instance 'cli-parser:cli-option
                       :abbr "s"
                       :full "save")
        (make-instance 'cli-parser:cli-option
                       :abbr "l"
                       :full "load")))

(defun cli-get-mode (options)
  (cond ((multiple-value-bind (value exists) (gethash "save" options)
           (declare (ignore value))
           exists)
         "save")
        ((multiple-value-bind (value exists) (gethash "load" options)
           (declare (ignore value))
           exists)
         "load")
        (t "save")))

;;;
;;; Video modes
;;;

(defun vector->complex (v)
  (complex (x v) (y v)))

(defun video-dimensions ()
  "Converts SDL:VIDEO-DIMENSIONS to a canonical (i.e. complex) form."
  (vector->complex (sdl:video-dimensions)))

(defun list-modes (flags)
  (let ((modes (sdl:list-modes flags)))
    (or (eql modes t)
        (mapcar #'vector->complex modes))))

(defun largest-video-dimensions (flags)
  (let ((modes (sdl:list-modes flags)))
    (if (eql modes t)
        (video-dimensions)
        (vector->complex
         (find (apply #'max (mapcar #'x modes)) modes :key #'x)))))

(defun smallest-video-dimensions (flags)
  (let ((modes (sdl:list-modes flags)))
    (if (eql modes t)
        (video-dimensions)
        (vector->complex
         (find (apply #'min (mapcar #'x modes)) modes :key #'x)))))

(defun main ()
  "Main function deals with initialization and loading of SDL dlls when running a compiled executable."
  ;; Initialization:
  (setup-paths)
  (load-dlls)

  (let* ((options
          (cli-parser:cli-parse (command-line-arguments) (cli-options)))
         (mode (cli-get-mode options))
         (file (or (gethash mode options) *save-file-pathname*)))
    ;; do more with mode
    (setf *save-file-pathname* file))

  (sdl:with-init ()
    (sdl:set-gl-attribute :sdl-gl-doublebuffer 1)
    (sdl:window 800 600 :bpp 32 :flags sdl:sdl-opengl)
    (gl:viewport 0 0 800 600)

    (gl:enable :texture-2d)
    (gl:enable :blend)
    (gl:blend-func :src-alpha :one-minus-src-alpha)
    (gl:clear-color 0 0 0 0)

    (gl:enable :depth-test)
    (gl:depth-func :lequal)

    (gl:matrix-mode :modelview)
    (gl:load-identity)

    (let ((root (make-instance 'component)))
      (loop for x from 0 to 800 by 16
         do (loop for y from 0 to 600 by 16
               do  (make-instance
                    'sprite :parent root :offset (complex x y)
                    :image (make-instance 'image :name 'tex
                                          :source "disp/texture.png"))))

      (setf (sdl:frame-rate) 100) ; uncork the frame rate and see how fast we go

      ;; Main loop:
      (sdl:with-events ()
        (:quit-event () t) ; t for quit, (return-from main) for toplevel
        (:key-down-event (:key key :mod mod :mod-key mod-key :unicode unicode)
          (declare (ignore mod mod-key unicode))
          (format t "key down ~a~%" key))
        (:key-up-event (:key key :mod mod :mod-key mod-key :unicode unicode)
          (declare (ignore mod mod-key unicode))
          (format t "key up ~a~%" key))
        (:idle ()
               (gl:clear :color-buffer-bit :depth-buffer-bit)

               (gl:with-pushed-matrix
                 (gl:ortho 0 800 600 0 -1 1)
                 (render root))

               (let ((s (format nil "fps: ~,2f" (sdl:average-fps))))
                 (sdl:set-caption s s))

               (gl:flush)
               (sdl:update-display)))))

  ;; Finalization:
  (exit))