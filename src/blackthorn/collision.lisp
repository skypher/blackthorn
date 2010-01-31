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

(in-package :blackthorn-physics)

;;;
;;; Collidables
;;;

(defgeneric collision-rect (object))

(defclass collidable (actor)
  ((collision-squares
    :initform (make-array 10 :fill-pointer 0 :adjustable t))
   (collision-offset
    :initform #c(0 0))))

(defgeneric collide (object event))
(defmethod collide (object event)
  (declare (ignore object event)))

(defmethod initialize-instance :after ((collidable collidable) &key)
  (bind collidable :collide #'collide))

(defclass collision-grid ()
  ((collision-grid
    :initform (make-hash-table))
   (collision-square-size
    :initform 32)))

(defgeneric insert-node (grid node xy))
(defmethod insert-node (grid node xy)
  (declare (ignore grid node)))
(defmethod insert-node (grid (node collidable) xy)
  (with-slots ((grid collision-grid) (square-size collision-square-size)) grid
    (with-slots (size (squares collision-squares) collision-offset) node
      (when (not (zerop size))
        (let ((x1 (truncate (x xy) square-size))
              (y1 (truncate (y xy) square-size))
              (x2 (truncate (+ (x xy) (x size)) square-size))
              (y2 (truncate (+ (y xy) (y size)) square-size)))
          (setf collision-offset xy (fill-pointer squares) 0)
          (iter (for i from x1 to x2)
                (iter (for j from y1 to y2)
                      (push node (gethash (complex i j) grid))
                      (vector-push-extend (complex i j) squares))))))))

(defgeneric search-node (grid node thunk))
(defmethod search-node (grid node thunk)
  (declare (ignore grid node thunk)))
(defmethod search-node (grid (node collidable) thunk)
  (with-slots ((grid collision-grid)) grid
    (with-slots ((squares collision-squares)) node
      (iter (for square in-vector squares)
            (iter (for other in (gethash square grid))
                  (when (not (eql node other))
                    (with-slots ((xy1 collision-offset) (s1 size)) node
                      (with-slots ((xy2 collision-offset) (s2 size)) other
                        (let ((x1 (x xy1)) (y1 (y xy1))
                              (x2 (x xy2)) (y2 (y xy2))
                              (w1 (x s1)) (h1 (y s1))
                              (w2 (x s2)) (h2 (y s2)))
                          (unless (or (<= (+ x1 w1) x2)
                                      (<= (+ x2 w2) x1)
                                      (<= (+ y1 h1) y2)
                                      (<= (+ y2 h2) y1))
                            (funcall thunk node other)))))))))))

(defvar *collision-grid*)

(defun find-collisions (root thunk)
  (unless (boundp '*collision-grid*)
    (setf *collision-grid* (make-instance 'collision-grid)))
  (labels ((insert-helper (node xy)
             (insert-node *collision-grid* node xy))
           (search-helper (node xy)
             (declare (ignore xy))
             (search-node *collision-grid* node thunk)))
    (with-slots (collision-grid) *collision-grid*
      (clrhash  collision-grid))
    (walk-tree root #'insert-helper)
    (walk-tree root #'search-helper)))

(defclass collision-event (event)
  ((type :initform :collide)
   (hit
    :reader event-hit
    :initarg :hit)))
