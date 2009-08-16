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

(in-package :blackthorn-test)

(def-suite blackthorn-physics :in blackthorn)

(in-suite blackthorn-physics)

(test root-with-no-children
  (let ((root (make-instance 'component)))
    (is (zerop (offset root)))
    (is (zerop (depth root)))
    (is (zerop (size root)))
    (is (not (parent root)))
    (is (arrayp (children root)))
    (is (zerop (array-dimension (children root) 0)))))

(test (root-with-one-child :depends-on root-with-no-children)
  (let* ((root (make-instance 'component))
         (child (make-instance 'component :parent root)))
    (is (eql root (parent child)))
    (is (= (array-dimension (children root) 0) 1))
    (is (eql (aref (children root) 0) child))
    (let (seen)
      (blt-phys::do-children (c root)
        (if seen
            (fail "do-children returned more than one object")
            (setf seen c)))
      (is (eql seen child)))))

(test (root-attach-detach-child :depends-on root-with-no-children)
  (let ((root (make-instance 'component))
         (child (make-instance 'component)))
    (is (not (parent child)))
    (is (zerop (array-dimension (children root) 0)))

    (attach root child)
    (is (eql root (parent child)))
    (is (= (array-dimension (children root) 0) 1))
    (is (eql (aref (children root) 0) child))

    (detach root child)
    (is (not (parent child)))
    (is (zerop (array-dimension (children root) 0)))))

(test (root-with-two-children :depends-on root-with-one-child)
  (let* ((root (make-instance 'component))
         (child1 (make-instance 'component :parent root :depth 1))
         (child2 (make-instance 'component :parent root :depth -1)))
    (is (eql root (parent child1)))
    (is (eql root (parent child2)))
    (is (= (array-dimension (children root) 0) 2))
    ;; since children are sorted by depth, we know the order of the children
    (is (eql (aref (children root) 0) child1))
    (is (eql (aref (children root) 1) child2))

    (let (seen)
      (blt-phys::do-children (c root)
        (push c seen))
      (setf seen (nreverse seen))
      (is (equal seen (list child1 child2)))))

  ;; everything should be independent of the order of object creation
  (let* ((root (make-instance 'component))
         (child2 (make-instance 'component :parent root :depth -1))
         (child1 (make-instance 'component :parent root :depth 1)))
    (is (eql root (parent child1)))
    (is (eql root (parent child2)))
    (is (= (array-dimension (children root) 0) 2))
    (is (eql (aref (children root) 0) child1))
    (is (eql (aref (children root) 1) child2))))

(test (root-with-many-children :depends-on root-with-two-children)
  (let* ((n 100)
         (root (make-instance 'component))
         (children
          (sort (loop repeat n
                   collect (make-instance 'component :parent root
                                          :depth (random 1.0)))
                #'> :key #'depth)))
    (is (= (array-dimension (children root) 0) n))
    (loop for i from 0 below n
       do (is (eql (aref (children root) i) (nth i children)))
       do (is (eql root (parent (aref (children root) i)))))))

(test (root-attach-detach-many-children :depends-on root-attach-detach-child)
  (let* ((n 100)
         (root (make-instance 'component))
         (children
          (loop repeat n
             collect (make-instance 'component :depth (random 1.0))))
         current)
    (is (= (array-dimension (children root) 0) 0))
    (loop for i from 0 below n
       for child in children
       do (attach root child)
       do (is (eql root (parent child)))
       do (push child current)
       do (setf current (sort current #'> :key #'depth))
       do (loop for j from 0 below i
             for c in current
             do (is (eql (aref (children root) j) c))))
    (loop for i from 0 below n
       for child in children
       do (detach root child)
       do (is (eql (parent child) nil))
       do (setf current (delete child current))
       do (loop for j from 0 below (- n i)
             for c in current
             do (is (eql (aref (children root) j) c))))))
