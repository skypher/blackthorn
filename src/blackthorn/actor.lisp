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

(in-package :blackthorn-physics)

;;;
;;; Actors
;;;

(defclass actor (component key-mixin)
  ())

(defmethod initialize-instance :after ((actor actor) &key)
  (bind actor :update #'update))

;;;
;;; Sprites
;;;

(defclass sprite (actor)
  ((image
    :accessor image
    :initarg :image
    :initform nil)))

(defmethod initialize-instance :after ((sprite sprite) &key image)
  (when image
    (setf (size sprite) (size image))))

(defmethod (setf image) :after (image (sprite sprite))
  (when image
    (setf (size sprite) (size image))))

(defmethod draw ((sprite sprite) xy z)
  (with-slots (image) sprite
    (draw image xy z)))

(defmethod update :before ((sprite sprite) event)
  (next-image (image sprite)))

;;;
;;; Mobiles
;;;

(defclass mobile (actor)
  ((veloc
    :accessor veloc
    :initarg :veloc
    :initform #c(0 0))
   (accel
    :accessor accel
    :initarg :accel
    :initform #c(0 0))))

(defmethod update :before ((mobile mobile) event)
  (with-slots (offset veloc accel) mobile
    (incf veloc accel)
    (incf offset veloc)))

;;;
;;; Collidables
;;;

(defclass collidable (actor)
  ())

(defgeneric collide (object event))
(defmethod collide (object event)
  (declare (ignore object event)))

(defmethod initialize-instance :after ((collidable collidable) &key)
  (bind collidable :collide #'collide))

(defgeneric non-collidable-ancestor (object xy))
(defmethod non-collidable-ancestor ((component component) xy)
  (with-slots (offset) component
    (values component (- xy offset))))

(defmethod non-collidable-ancestor ((collidable collidable) xy)
  (with-slots (parent offset) collidable
    (if parent
        (non-collidable-ancestor parent (- xy offset))
        (values collidable (- xy offset)))))

(defgeneric collide-p (o1 o2 xy1 xy2))
(defmethod collide-p ((o1 collidable) o2 xy1 xy2)
  (declare (ignore o1 o2 xy1 xy2)))

(defmethod collide-p ((o1 collidable) (o2 collidable) xy1 xy2)
  (let ((x1 (x xy1)) (y1 (y xy1))
        (x2 (x xy2)) (y2 (y xy2))
        (w1 (x (size o1))) (h1 (y (size o1)))
        (w2 (x (size o2))) (h2 (y (size o2))))
    (not (or (<= (+ x1 w1) x2)
             (<= (+ x2 w2) x1)
             (<= (+ y1 h1) y2)
             (<= (+ y2 h2) y1)))))

(defclass collision-event (event)
  ((type :initform :collide)
   (hit
    :reader event-hit
    :initarg :hit)))
