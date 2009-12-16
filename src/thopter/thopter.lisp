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

(in-package :thopter)

(defclass thopter-game (game) ())

(defclass thopter (sprite mobile collidable) ())
(defclass bullet (sprite mobile collidable) ())
(defclass enemy (sprite mobile collidable)
  ((health
    :accessor health
    :initarg :health
    :initform 0)))
(defclass explosion (sprite mobile collidable)
  ((ttl
    :initarg :ttl
    :initform 0)))

(defmethod initialize-instance :after ((thopter thopter) &key)
  (bind-key-down thopter :sdl-key-up    #'move-north)
  (bind-key-up   thopter :sdl-key-up    #'stop-north)
  (bind-key-down thopter :sdl-key-down  #'move-south)
  (bind-key-up   thopter :sdl-key-down  #'stop-south)
  (bind-key-down thopter :sdl-key-left  #'move-west)
  (bind-key-up   thopter :sdl-key-left  #'stop-west)
  (bind-key-down thopter :sdl-key-right #'move-east)
  (bind-key-up   thopter :sdl-key-right #'stop-east)
  (bind-key-down thopter :sdl-key-space #'shoot))

(defmethod move-north ((thopter thopter) event)
  (incf (veloc thopter) #c(0 -2)))

(defmethod stop-north ((thopter thopter) event)
  (decf (veloc thopter) #c(0 -2)))

(defmethod move-south ((thopter thopter) event)
  (incf (veloc thopter) #c(0 2)))

(defmethod stop-south ((thopter thopter) event)
  (decf (veloc thopter) #c(0 2)))

(defmethod move-west ((thopter thopter) event)
  (incf (veloc thopter) #c(-2 0)))

(defmethod stop-west ((thopter thopter) event)
  (decf (veloc thopter) #c(-2 0)))

(defmethod move-east ((thopter thopter) event)
  (incf (veloc thopter) #c(2 0)))

(defmethod stop-east ((thopter thopter) event)
  (decf (veloc thopter) #c(2 0)))

(defmethod shoot ((thopter thopter) event)
  (with-slots (parent offset size veloc) thopter
    (make-instance 'bullet :parent parent 
                   :offset (+ offset (/ (x size) 2) #c(0 -4)) :depth -1
                   :veloc (+ veloc #c(0 -5))
                   :image (make-instance 'image :name :bullet))))

(defmethod update ((bullet bullet) event)
  ;; TODO: doesn't work for nested objects. Need absolute-offset.
  (with-slots (parent offset size) bullet
    (with-slots ((view-offset offset) (view-size size)) (game-view *game*)
      (let ((x1 (x offset)) (y1 (y offset))
            (x2 (x view-offset)) (y2 (y view-offset))
            (w1 (x size)) (h1 (y size))
            (w2 (x view-size)) (h2 (y view-size)))
        (when (or (<= (+ x1 w1) x2)
                  (<= (+ x2 w2) x1)
                  (<= (+ y1 h1) y2)
                  (<= (+ y2 h2) y1))
          (detach parent bullet))))))

(defmethod collide ((bullet bullet) event)
  (when (typep (event-hit event) 'enemy)
    (detach (parent bullet) bullet)))

(defmethod collide ((enemy enemy) event)
  (with-slots (parent offset depth veloc health) enemy
    (when (typep (event-hit event) 'bullet)
      (decf health)
      (incf veloc (/ (veloc (event-hit event)) 20))
      (when (<= health 0)
        (make-instance 'explosion :parent parent
                       :offset offset :depth depth :veloc veloc
                       :image (make-instance 'image :name :explosion)
                       :ttl 10)
        (detach parent enemy)))))

(defmethod update ((explosion explosion) event)
  (with-slots (ttl) explosion
    (decf ttl)
    (when (<= ttl 0)
      (detach (parent explosion) explosion))))

(defmethod game-init ((game thopter-game))
  (let ((root (make-instance 'component))
        (size #c(800 600)))
    (setf (game-root game) root
          (game-view game) (make-instance 'component :size size)
          (game-sheet game)
          (make-instance 'sheet :source (resource "disp/thopter.png")))
    (let ((thopter (make-instance
                    'thopter :parent root
                    :offset (complex (/ (x size) 2) (* (y size) 3/4))
                    :image (make-instance 'image :name :thopter))))
      (subscribe (game-keys game) thopter))
    (loop for i from -128 to 128 by 64
       do (make-instance 'enemy :parent root
                         :offset (complex (+ (/ (x size) 2) i) (/ (y size) 4))
                         :depth 1
                         :image (make-instance 'image :name :enemy)
                         :health 4))))

(defmethod game-update :after ((game thopter-game))
  ;; report the frame reate
  (let ((s (format nil "fps: ~,2f" (sdl:average-fps))))
    (set-caption s s)))

;; For interactive use:
(defun thopter ()
  (let ((*game* (make-instance 'thopter-game)))
    (main :exit-when-done nil)))

;; For non-interactive use:
(defvar *game* (make-instance 'thopter-game))