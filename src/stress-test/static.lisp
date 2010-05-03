;;;; Blackthorn -- Lisp Game Engine
;;;;
;;;; Copyright (c) 2007-2010, Elliott Slaughter <elliottslaughter@gmail.com>
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

(defclass static-game (game)
  ((test-size
    :reader test-size
    :initarg :test-size
    :initform 5000)))

(defclass static-object (sprite) ())

(defmethod report-event ((object actor) (event key-event))
  (declare (ignore object))
  (format t "~a: ~a ~a ~a~%" (event-type event) (event-key event)
          (event-mod event) (event-mod-key event)))

(defmethod initialize-instance :after ((game static-game) &key)
  (setf (game-root game) (make-instance 'component :size #c(800 600))
        (game-view game) (make-instance 'component :size #c(800 600))))

(defmethod game-init ((game static-game) &key &allow-other-keys)
  (let* ((root (game-root game))
         (size (size (game-root game))))
    (setf (game-sheet game) (load-sheet (resource "disp/sheet.png")))
    (loop for i from 0 to (test-size game)
       do (make-instance
           'static-object :parent root
           :offset (complex (random (x size)) (random (y size)))
           :image (make-image :explosion)))
    (let ((keys (make-instance 'actor)))
      (bind keys :key-down #'report-event)
      (bind keys :key-up #'report-event)
      (subscribe (game-keys game) keys))))

(defmethod game-init :after ((game static-game) &key &allow-other-keys)
  ;; uncork the frame rate and see how fast we go
  (setf (sdl:frame-rate) 100))

(defmethod game-update :after ((game static-game))
  ;; report the frame rate
  (let ((s (format nil "fps: ~,2f" (sdl:average-fps))))
    (set-caption s s)))

;; For interactive use:
(defun static-test (&optional (n 5000))
  (let ((*game* (make-instance 'static-game :test-size n)))
    (main :exit-when-done nil)))

;; For non-interactive use:
;(defparameter *game* (make-instance 'static-game :test-size 5000))
