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
(defgeneric reactive-collisions-only-p (object))

(defclass collidable (actor)
  ((collision-offset
    :initform #c(0 0))
   (reactive-collisions-only-p
    :accessor reactive-collisions-only-p
    :initarg :reactive-collisions-only-p
    :initform nil)))

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

(defmacro with-collision-grid-iterate ((var (grid xy1 xy2) &key outer-label)
                                       &body body)
  (with-gensyms (g sq i1 j1 i2 j2 i j)
    (once-only (grid xy1 xy2)
      `(with-slots ((,g collision-grid) (,sq collision-square-size)) ,grid
         (let ((,i1 (truncate (x ,xy1) ,sq))
               (,j1 (truncate (y ,xy1) ,sq))
               (,i2 (truncate (x ,xy2) ,sq))
               (,j2 (truncate (y ,xy2) ,sq)))
           (iter ,@(when outer-label (list outer-label))
                 (for ,i from ,i1 to ,i2)
                 (iter (for ,j from ,j1 to ,j2)
                       (symbol-macrolet
                           (,(when var `(,var (gethash (complex ,i ,j) ,g))))
                         ,@body))))))))

(defgeneric collision-grid-insert-node (grid node xy))
(defmethod collision-grid-insert-node (grid node xy)
  (declare (ignore grid node)))
(defmethod collision-grid-insert-node (grid (node collidable) xy)
  (with-slots (size (offset collision-offset)) node
    (when (not (zerop size))
      (setf offset xy)
      (with-collision-grid-iterate (nodes (grid xy (+ xy size)))
        (push node nodes)))))

(defgeneric collision-grid-search-node (grid node thunk))
(defmethod collision-grid-search-node (grid node thunk)
  (declare (ignore grid node thunk)))
(let ((collisions (make-hash-table)))
  (defmethod collision-grid-search-node (grid (node collidable) thunk)
    (with-slots ((s1 size) (xy1 collision-offset)
                 (r1-p reactive-collisions-only-p)) node
      (unless r1-p
        (let ((x1 (x xy1)) (y1 (y xy1)) (w1 (x s1)) (h1 (y s1)))
          (with-collision-grid-iterate (nodes (grid xy1 (+ xy1 s1)))
            (iter (for other in nodes)
                  (when (not (eql node other))
                    (with-slots ((xy2 collision-offset) (s2 size)) other
                      (let ((x2 (x xy2)) (y2 (y xy2))
                            (w2 (x s2)) (h2 (y s2)))
                        (unless (or (<= (+ x1 w1) x2)
                                    (<= (+ x2 w2) x1)
                                    (<= (+ y1 h1) y2)
                                    (<= (+ y2 h2) y1))
                          (setf (gethash other collisions) t))))))))
        (iter (for (other nil) in-hashtable collisions)
              (with-slots ((r2-p reactive-collisions-only-p)) other
                (funcall thunk node other)
                (when r2-p (funcall thunk other node))))
        (clrhash collisions)
        nil))))

(defun collision-grid-search-nearest (grid node radius
                                      &key (test (constantly t)))
  (with-slots ((offset collision-offset) size) node
    (let ((circle (complex radius radius)))
      (with-collision-grid-iterate (nodes (grid (- offset circle)
                                                (+ offset circle size))
                                          :outer-label outer)
        (iter (for other in nodes)
              (when (and (not (eql node other)) (funcall test other))
                (with-slots ((other-offset collision-offset)
                             (other-size size)) other
                  (in outer (finding other minimizing
                                     (dist (+ offset (/ size 2))
                                           (+ other-offset (/ other-size 2)))
                                     into min))))
              (in outer
                  (finally
                   (when min
                     (with-slots ((other-offset collision-offset)
                                  (other-size size)) min
                       (when (< (dist (+ offset (/ size 2))
                                      (+ other-offset (/ other-size 2)))
                                (+ radius (/ (+ (x size) (y size)) 2d0)))
                         (return-from outer min)))))))))))

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

(defun find-nearest-object (node radius &key (test (constantly t)))
  (collision-grid-search-nearest *collision-grid* node radius :test test))

;;;
;;; Collision Events
;;;

(defclass collision-event (event)
  ((type :initform :collide)
   (hit
    :reader event-hit
    :initarg :hit)))
