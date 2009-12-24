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
;;; Collidables
;;;

(defgeneric collision-rect (object))

(defclass collidable (actor)
  ((collision-rect
    :accessor collision-rect)))

(defgeneric collide (object event))
(defmethod collide (object event)
  (declare (ignore object event)))

(defmethod initialize-instance :after ((collidable collidable) &key)
  (bind collidable :collide #'collide))

(defun make-rtree ()
  (spatial-trees:make-spatial-tree :greene :rectfun #'collision-rect))

(defun complex->truncated-list (i)
  (list (truncate (x i)) (truncate (y i))))

(defgeneric insert-node (tree node xy))
(defmethod insert-node (tree node xy)
  (declare (ignore tree node)))
(defmethod insert-node (tree (node collidable) xy)
  (with-slots (size collision-rect) node
    (when (not (zerop size))
      (setf collision-rect
            (rectangles:make-rectangle
             :lows (complex->truncated-list xy)
             :highs (complex->truncated-list (+ xy size #c(-1 -1)))))
      (spatial-trees:insert node tree))))

(defgeneric search-node (tree node thunk))
(defmethod search-node (tree node thunk)
  (declare (ignore tree node thunk)))
(defmethod search-node (tree (node collidable) thunk)
  (iter (for other in (spatial-trees:search node tree))
        (when (not (eql node other))
          (funcall thunk node other))))

(defun find-collisions (root thunk)
  (let ((rtree (make-rtree)))
    (labels ((insert-helper (node xy)
               (insert-node rtree node xy))
             (search-helper (node xy)
               (declare (ignore xy))
               (search-node rtree node thunk)))
      (walk-tree root #'insert-helper)
      (walk-tree root #'search-helper))))

(defclass collision-event (event)
  ((type :initform :collide)
   (hit
    :reader event-hit
    :initarg :hit)))
