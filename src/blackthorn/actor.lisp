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
