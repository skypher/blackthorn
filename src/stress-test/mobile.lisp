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

(in-package :blackthorn-stress-test)

(defclass mobile-game (game)
  ((test-size
    :reader test-size
    :initarg :test-size
    :initform 5000)))

(defclass mobile-object (sprite mobile) ())

(defmethod init-game ((game mobile-game))
  (let ((root (make-instance 'component))
        (size #c(800 600))
        (texture-pathname
         (merge-pathnames "disp/texture.png" *resource-directory-pathname*)))
    (loop for i from 0 to (test-size game)
       do (make-instance
           'mobile-object :parent root
           :offset (complex (random (x size)) (random (y size)))
           :veloc (complex (- (random 1.0) 0.5) (- (random 1.0) 0.5))
           :image (make-instance 'image :name 'tex
                                 :source texture-pathname)))
    (labels ((report-event (object event)
               (declare (ignore object))
               (format t "~a: ~a~%" (event-type event) (event-key event))))
      (let ((keys (make-instance 'actor :parent root)))
        (bind keys :key-down #'report-event)
        (bind keys :key-up #'report-event)
        (subscribe (game-keys game) keys)))
    (setf (game-root game) root
          (game-view game)
          (make-instance 'component :offset #c(0 0) :size size))))

(defmethod init-game :after ((game mobile-game))
  ;; uncork the frame rate and see how fast we go
  (setf (sdl:frame-rate) 100))

(defmethod update :after ((game mobile-game))
  ;; report the frame reate
  (let ((s (format nil "fps: ~,2f" (sdl:average-fps))))
    (set-caption s s)))

(defmethod update :after ((object mobile-object))
  (with-slots (offset veloc) object
    (cond ((< (x offset) 0)
           (setf veloc (complex (abs (x veloc)) (y veloc))))
          ((>= (x offset) (x (size (game-view *game*))))
           (setf veloc (complex (- (abs (x veloc))) (y veloc)))))
    (cond ((< (y offset) 0)
           (setf veloc (complex (x veloc) (abs (y veloc)))))
          ((>= (y offset) (y (size (game-view *game*))))
           (setf veloc (complex (x veloc) (- (abs (y veloc)))))))))

;; For interactive use:
(defun mobile-test (&optional (n 5000))
  (let ((*game* (make-instance 'mobile-game :test-size n)))
    (main :exit-when-done nil)))

;; For non-interactive use:
(defvar *game* (make-instance 'mobile-game :test-size 5000))
