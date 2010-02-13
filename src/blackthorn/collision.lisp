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
  ((collision-offset
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

(defgeneric collision-grid-insert-node (grid node xy))
(defmethod collision-grid-insert-node (grid node xy)
  (declare (ignore grid node)))
(defmethod collision-grid-insert-node (grid (node collidable) xy)
  (with-slots ((grid collision-grid) (square-size collision-square-size)) grid
    (with-slots (size (offset collision-offset)) node
      (when (not (zerop size))
        (let ((i1 (truncate (x xy) square-size))
              (j1 (truncate (y xy) square-size))
              (i2 (truncate (+ (x xy) (x size)) square-size))
              (j2 (truncate (+ (y xy) (y size)) square-size)))
          (setf offset xy)
          (iter (for i from i1 to i2)
                (iter (for j from j1 to j2)
                      (push node (gethash (complex i j) grid)))))))))

(defgeneric collision-grid-search-node (grid node thunk))
(defmethod collision-grid-search-node (grid node thunk)
  (declare (ignore grid node thunk)))
(let ((collisions (make-hash-table)))
  (defmethod collision-grid-search-node (grid (node collidable) thunk)
    (with-slots ((grid collision-grid) (square-size collision-square-size)) grid
      (with-slots ((s1 size) (xy1 collision-offset)) node
        (let ((i1 (truncate (x xy1) square-size))
              (j1 (truncate (y xy1) square-size))
              (i2 (truncate (+ (x xy1) (x s1)) square-size))
              (j2 (truncate (+ (y xy1) (y s1)) square-size))
              (x1 (x xy1)) (y1 (y xy1)) (w1 (x s1)) (h1 (y s1)))
          (iter
           (for i from i1 to i2)
           (iter
            (for j from j1 to j2)
            (iter (for other in (gethash (complex i j) grid))
                  (when (not (eql node other))
                    (with-slots ((xy2 collision-offset) (s2 size)) other
                      (let ((x2 (x xy2)) (y2 (y xy2)) (w2 (x s2)) (h2 (y s2)))
                        (unless (or (<= (+ x1 w1) x2)
                                    (<= (+ x2 w2) x1)
                                    (<= (+ y1 h1) y2)
                                    (<= (+ y2 h2) y1))
                          (setf (gethash other collisions) t)))))))))
        (iter (for (other nil) in-hashtable collisions)
              (funcall thunk node other))
        (clrhash collisions)
        nil))))

(defun collision-grid-search-nearest (grid node size &key (test (constantly t)))
  (with-slots ((grid collision-grid) (square-size collision-square-size)) grid
    (with-slots ((offset collision-offset)) node
      (let* ((xy1 (- offset (complex size size)))
             (xy2 (+ offset (complex size size)))
             (i1 (truncate (x xy1) square-size))
             (j1 (truncate (y xy1) square-size))
             (i2 (truncate (x xy2) square-size))
             (j2 (truncate (y xy2) square-size)))
        (iter
         (for i from i1 to i2)
         (let ((min
                (iter
                 (for j from j1 to j2)
                 (let ((min
                        (iter
                         (for other in (gethash (complex i j) grid))
                         (when (funcall test other)
                           (with-slots ((other-offset collision-offset)) other
                             (finding other minimizing
                                      (dist offset other-offset)))))))
                   (when min
                     (with-slots ((min-offset collision-offset)) min
                       (finding min minimizing (dist offset min-offset))))))))
           (when min
             (with-slots ((min-offset collision-offset)) min
               (finding min minimizing (dist offset min-offset))))))))))

(defvar *collision-grid*)

(defun collision-grid-update (root)
  (unless (boundp '*collision-grid*)
    (setf *collision-grid* (make-instance 'collision-grid)))
  (labels ((insert-helper (node xy)
             (collision-grid-insert-node *collision-grid* node xy)))
    (with-slots (collision-grid) *collision-grid*
      (clrhash  collision-grid))
    (walk-tree root #'insert-helper)))

(defun collision-grid-search (root thunk)
  (labels ((search-helper (node xy)
             (declare (ignore xy))
             (collision-grid-search-node *collision-grid* node thunk)))
    (walk-tree root #'search-helper)))

(defun find-collisions (root thunk)
  (collision-grid-update root)
  (collision-grid-search root thunk))

(defun find-nearest-object (node size &key (test (constantly t)))
  (collision-grid-search-nearest *collision-grid* node size :test test))

;;;
;;; Collision Events
;;;

(defclass collision-event (event)
  ((type :initform :collide)
   (hit
    :reader event-hit
    :initarg :hit)))
