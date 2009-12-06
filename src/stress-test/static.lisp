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

(defclass static-game (game) ())

(defmethod init-game ((game static-game))
  (let ((root (make-instance 'component))
        (size #c(800 600))
        (texture-pathname
         (merge-pathnames "disp/texture.png" *resource-directory-pathname*)))
    (loop for x from 0 to (x size) by 16
       do (loop for y from 0 to (y size) by 16
             do (make-instance
                 'sprite :parent root :offset (complex x y)
                 :image (make-instance 'image :name 'tex
                                       :source texture-pathname))))
    (setf (game-root game) root
          (game-view game)
          (make-instance 'component :offset #c(0 0) :size size))))

(defmethod init-game :after ((game static-game))
  ;; uncork the frame rate and see how fast we go
  (setf (sdl:frame-rate) 100))

(defmethod update-game :after ((game static-game))
  ;; report the frame reate
  (let ((s (format nil "fps: ~,2f" (sdl:average-fps))))
    (set-caption s s)))

;; For interactive use:
(defun static-test ()
  (let ((*game* (make-instance 'static-game)))
    (main :exit-when-done nil)))

;; For non-interactive use:
;(defparameter *game* (make-instance 'static-game))