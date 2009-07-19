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

(in-package :blackthorn-physics)

;;;
;;; Components
;;;

(defgeneric offset (object))
(defgeneric depth (object))
(defgeneric size (object))
(defgeneric parent (object))
(defgeneric children (object))

(defclass component ()
  ((offset
    :accessor offset
    :initarg :offset
    :initform #c(0 0))
   (depth
    :accessor depth
    :initarg :depth
    :initform 0)
   (size
    :accessor size
    :initarg :size
    :initform #c(0 0))
   (parent
    :reader parent
    :initform nil)
   (children
    :reader children
    :initform (make-array 10 :fill-pointer 0 :adjustable t))))

(defgeneric attach (parent child))
(defgeneric dettach (parent child))

(defmethod initialize-instance :after ((component component) &key parent)
  (when parent
    (attach parent component)))

(defmethod attach ((parent component) (child component))
  (with-slots (children) parent
    (with-slots ((childs-parent parent)) child
      (when childs-parent
        (dettach childs-parent child))
      (vector-push-extend child children)
      ;; Replace generic sort with an efficient bubble sort?
      (sort children #'> :key #'depth)
      (setf childs-parent parent))))

(defmethod dettach ((parent component) (child component))
  (with-slots (children) parent
    (with-slots ((childs-parent parent)) child
      ;; Swap the child to the end so it can pop off...
      (rotatef (aref children (position child children))
               (aref children (1- (fill-pointer children))))
      ;; Ensure that the child can't be seen in the parent's children list.
      (setf (aref children (1- (fill-pointer children))) nil)
      (vector-pop children)
      (setf childs-parent nil))))

(defmacro do-children ((var component) &body body)
  `(loop for ,var across (slot-value ,component 'children)
      do (progn ,@body)))

(defmethod render :around ((component component))
  (with-slots (offset) component
    (gl:with-pushed-matrix
      (gl:translate (x offset) (y offset) 0)
      (call-next-method))))

(defmethod render ((component component))
  (with-slots (children) component
    (let ((n (fill-pointer children)))
      (unless (zerop n)
	(gl:with-pushed-matrix
	  (gl:translate 0 0 (+ -1 (/ 1.0d0 n)))
	  (gl:scale 1 1 (/ 1.0d0 n))
	  (loop with i = (/ 2.0d0 n)
	     for child across children
	     do (gl:translate 0 0 i)
	     do (render child)))))))

(defmethod update ((component component))
  (do-children (child component)
    (update child)))

;;;
;;; Sprites
;;;

(defgeneric image (object))

(defclass sprite (component)
  ((image
    :accessor image
    :initarg :image
    :initform nil)))

(defmethod render :before ((sprite sprite))
  (with-slots (image depth) sprite
    (render image)))

;;;
;;; Mobiles
;;;

(defgeneric veloc (object))
(defgeneric accel (object))

(defclass mobile (component)
  ((veloc
    :accessor veloc
    :initarg :veloc
    :initform #c(0 0))
   (accel
    :accessor accel
    :initarg :accel
    :initform #c(0 0))))

(defmethod update :before ((mobile mobile))
  (with-slots (offset veloc accel) mobile
    (incf veloc accel)
    (incf offset veloc)))