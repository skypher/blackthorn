;;;; Blackthorn -- Lisp Game Engine
;;;;
;;;; Copyright (c) 2007-2011, Elliott Slaughter <elliottslaughter@gmail.com>
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

(defclass collidable-game (game)
  ((test-size
    :reader test-size
    :initarg :test-size
    :initform 2000)))

(defclass collidable-object (sprite mobile collidable) ())

(defmethod report-event ((object actor) (event key-event))
  (declare (ignore object))
  (format t "~a: ~a ~a ~a~%" (event-type event) (event-key event)
          (event-mod event) (event-mod-key event)))

(defmethod initialize-instance :after ((game collidable-game) &key)
  (setf (game-root game) (make-instance 'component :size #c(800 600))
        (game-view game) (make-instance 'component :size #c(800 600))))

(defmethod game-init ((game collidable-game) &key &allow-other-keys)
  (let* ((root (game-root game))
         (size (size (game-root game))))
    (setf (game-sheet game) (load-sheet "disp/sheet.png"))
    (loop for i from 0 to (test-size game)
       do (make-instance
           'collidable-object :parent root
           :offset (complex (random (x size)) (random (y size)))
           :veloc (complex (- (random 1.0) 0.5) (- (random 1.0) 0.5))
           :image (make-image :explosion)))
    (let ((keys (make-instance 'actor)))
      (bind keys :key-down #'report-event)
      (bind keys :key-up #'report-event)
      (subscribe (game-keys game) keys))))

(defmethod game-init :after ((game collidable-game) &key &allow-other-keys)
  ;; uncork the frame rate and see how fast we go
  (setf (sdl:frame-rate) 100))

(defmethod game-update :after ((game collidable-game))
  ;; report the frame rate
  (let ((s (format nil "fps: ~,2f" (sdl:average-fps))))
    (set-caption s s)))

(defmethod update :after ((object collidable-object) event)
  (declare (ignore event))
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
(defun collidable-test (&optional (n 2000))
  (let ((*game* (make-instance 'collidable-game :test-size n)))
    (main :exit-when-done nil)))

;; For non-interactive use:
;(defparameter *game* (make-instance 'collidable-game :test-size 2000))
