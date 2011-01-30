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

(in-package :blackthorn-collision-test)

(defclass blackthorn-collision-test-game (game) ())

(defclass player (sprite mobile collidable)
  ((image :initform (make-image :orange))))

(defmethod initialize-instance :after ((player player) &key)
  (bind-key-down player :sdl-key-up    #'move-north)
  (bind-key-up   player :sdl-key-up    #'stop-north)
  (bind-key-down player :sdl-key-down  #'move-south)
  (bind-key-up   player :sdl-key-down  #'stop-south)
  (bind-key-down player :sdl-key-left  #'move-west)
  (bind-key-up   player :sdl-key-left  #'stop-west)
  (bind-key-down player :sdl-key-right #'move-east)
  (bind-key-up   player :sdl-key-right #'stop-east))

(defmethod move-north ((player player) event)
  (incf (veloc player) #c(0 -1)))

(defmethod stop-north ((player player) event)
  (decf (veloc player) #c(0 -1)))

(defmethod move-south ((player player) event)
  (incf (veloc player) #c(0 1)))

(defmethod stop-south ((player player) event)
  (decf (veloc player) #c(0 1)))

(defmethod move-west ((player player) event)
  (incf (veloc player) #c(-1 0)))

(defmethod stop-west ((player player) event)
  (decf (veloc player) #c(-1 0)))

(defmethod move-east ((player player) event)
  (incf (veloc player) #c(1 0)))

(defmethod stop-east ((player player) event)
  (decf (veloc player) #c(1 0)))

(defclass toggle (sprite mobile collidable)
  ((image :initform (make-image :green))))

(defmethod collide ((toggle toggle) event)
  (setf (image toggle) (make-image :blue)))

(defmethod initialize-instance :after ((game blackthorn-collision-test-game)
                                       &key)
  (setf (game-root game) (make-instance 'component :size #c(800 600))
        (game-view game) (make-instance 'component :size #c(800 600))))

(defmethod game-init ((game blackthorn-collision-test-game)
                      &key &allow-other-keys)
  (let* ((root (game-root game))
         (size (size (game-root game))))
    (setf (game-sheet game) (load-sheet (resource "disp/collision.png")))
    (let ((player (make-instance 'player :parent root :offset (/ size 2)
                                 :image (make-image :orange))))
      (subscribe (game-keys game) player))
    (let ((image-size (size (make-image :orange))))
      (iter (for x from 0 below (x size) by (* 2 (x image-size)))
            (iter (for y from 0 below (y size) by (* 2 (y image-size)))
                  (make-instance
                   'toggle :parent root :offset (complex x y) :depth 1))))))

(defmethod game-update :after ((game blackthorn-collision-test-game))
  ;; report the frame reate
  (let ((s (format nil "fps: ~,2f" (sdl:average-fps))))
    (set-caption s s)))

;; For interactive use:
(defun blackthorn-collision-test ()
  (let ((*game* (make-instance 'blackthorn-collision-test-game)))
    (main :exit-when-done nil)))

;; For non-interactive use:
(defvar *game* (make-instance 'blackthorn-collision-test-game))
