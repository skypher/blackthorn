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
;;; Components
;;;

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
    :initform (vector))))

(defmethod initialize-instance :after ((component component) &key parent)
  (when parent
    (attach parent component)))

(defmethod attach ((parent component) (child component))
  (with-slots (children) parent
    (with-slots ((childs-parent parent)) child
      (when childs-parent
        (detach childs-parent child))
      (setf children (merge 'vector children (vector child) #'> :key #'depth))
      (setf childs-parent parent))))

(defmethod detach ((parent component) (child component))
  (with-slots (children) parent
    (with-slots ((childs-parent parent)) child
      (setf children (delete child children))
      (setf childs-parent nil))))

(defmacro do-children ((var component) &body body)
  `(loop for ,var across (slot-value ,component 'children)
      do (progn ,@body)))

(defun walk-tree (component func &optional test (xy #c(0 0)))
  (when (or (not test) (funcall test component))
    (with-slots (offset) component
      (let ((xy (when xy (+ xy offset))))
        (funcall func component xy)
        (do-children (child component)
          (walk-tree child func test xy))))))

(defun first-neg-depth (children)
  (position-if #'(lambda (x) (< (slot-value x 'depth) 0)) children))

(defmethod render ((component component) xy zmin zmax)
  (with-slots (children offset) component
    (let ((xy (+ xy offset))
          (n (array-dimension children 0)))
      (if (not (zerop n))
          (let ((median (or (first-neg-depth children) n))
                (dz (/ (- zmax zmin) 2.0d0 n)))
            (iter (for child in-vector children below median)
                  (for z initially zmax then (- z dz))
                  (render child xy (- z dz) z))
            (draw component xy (/ (+ zmax zmin) 2d0))
            (iter (for child in-vector children from median)
                  (for z initially (/ (+ zmax zmin) 2d0) then (- z dz))
                  (render child xy (- z dz) z)))
          (draw component xy (/ (+ zmax zmin) 2d0))))))

(defmethod draw ((component component) xy z)
  (declare (ignore component xy z)))

(defmethod update ((component component) event)
  (declare (ignore component event)))
