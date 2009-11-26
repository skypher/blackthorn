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

(in-package :blackthorn-test)

(def-suite blackthorn-physics :in blackthorn)

(in-suite blackthorn-physics)

;;;
;;; Check the structural properties of component trees
;;;

(test component-with-no-children
  (let ((root (make-instance 'component)))
    (is (zerop (offset root)))
    (is (zerop (depth root)))
    (is (zerop (size root)))
    (is (not (parent root)))
    (is (arrayp (children root)))
    (is (zerop (array-dimension (children root) 0)))))

(test (component-with-one-child :depends-on component-with-no-children)
  (let* ((root (make-instance 'component))
         (child (make-instance 'component :parent root)))
    (is (eql root (parent child)))
    (is (= 1 (array-dimension (children root) 0)))
    (is (eql child (aref (children root) 0)))
    (let (seen)
      (blt-phys::do-children (c root)
	(push child seen))
      (is (equal (list child) seen)))))

(test (component-attach-detach-child :depends-on component-with-no-children)
  (let ((root (make-instance 'component))
         (child (make-instance 'component)))
    (is (not (parent child)))
    (is (zerop (array-dimension (children root) 0)))

    (attach root child)
    (is (eql root (parent child)))
    (is (= 1 (array-dimension (children root) 0)))
    (is (eql child (aref (children root) 0)))

    (detach root child)
    (is (not (parent child)))
    (is (zerop (array-dimension (children root) 0)))))

(test (component-with-two-children :depends-on component-with-one-child)
  (let* ((root (make-instance 'component))
         (child1 (make-instance 'component :parent root :depth 1))
         (child2 (make-instance 'component :parent root :depth -1)))
    (is (eql root (parent child1)))
    (is (eql root (parent child2)))
    (is (= 2 (array-dimension (children root) 0)))
    ;; since children are sorted by depth, we know the order of the children
    (is (eql child1 (aref (children root) 0)))
    (is (eql child2 (aref (children root) 1)))

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
    (is (= 2 (array-dimension (children root) 0)))
    (is (eql child1 (aref (children root) 0)))
    (is (eql child2 (aref (children root) 1)))))

(test (component-with-nested-children :depends-on component-with-one-child)
  (let* ((root (make-instance 'component))
         (child (make-instance 'component :parent root))
         (grandchild (make-instance 'component :parent child)))
    (is (eql root (parent child)))
    (is (= 1 (array-dimension (children root) 0)))
    (is (eql child (aref (children root) 0)))

    (is (eql child (parent grandchild)))
    (is (= 1 (array-dimension (children child) 0)))
    (is (eql grandchild (aref (children child) 0)))))

(test (component-with-many-children :depends-on component-with-two-children)
  (let* ((n 100)
         (root (make-instance 'component))
         (children
          (sort (loop repeat n
                   collect (make-instance 'component :parent root
                                          :depth (random 1.0)))
                #'> :key #'depth)))
    (is (= n (array-dimension (children root) 0)))
    (loop for i from 0 below n
       do (is (eql (nth i children) (aref (children root) i)))
       do (is (eql root (parent (aref (children root) i)))))))

(test (component-attach-detach-many-children
       :depends-on component-attach-detach-child)
  (let* ((n 20)
         (root (make-instance 'component))
         (children
          (loop repeat n
             collect (make-instance 'component :depth (random 1.0))))
         current)
    (is (= 0 (array-dimension (children root) 0)))
    (loop for i from 0 below n
       for child in children
       do (attach root child)
       do (is (eql root (parent child)))
       do (push child current)
       do (setf current (sort current #'> :key #'depth))
       do (loop for j from 0 below i
             for c in current
             do (is (eql c (aref (children root) j)))))
    (loop for i from 0 below n
       for child in children
       do (detach root child)
       do (is (eql nil (parent child)))
       do (setf current (delete child current))
       do (loop for j from 0 below (- n i)
             for c in current
             do (is (eql c (aref (children root) j)))))))

;;;
;;; Ensure that update gets called on every node in the entire subtree
;;;

(defclass update-test (component) ())

(defvar *methods-called*)

(defmethod update :before ((object update-test))
  (push (list :before object) *methods-called*))

(defmethod update ((object update-test))
  (push (list :primary object) *methods-called*)
  (call-next-method))

(defmethod update :after ((object update-test))
  (push (list :after object) *methods-called*))

(test (component-update-no-children :depends-on component-with-no-children)
  (let ((root (make-instance 'update-test))
        *methods-called*)
    (update root)
    (is (equal `((:before ,root)
                 (:primary ,root)
                 (:after ,root))
               (reverse *methods-called*)))))

(test (component-update-two-children
       :depends-on (and component-with-two-children
                        component-update-no-children))
  (let* ((root (make-instance 'update-test))
         (child1 (make-instance 'update-test :parent root :depth 1))
         (child2 (make-instance 'update-test :parent root :depth -1))
         *methods-called*)
    (update root)
    (is (equal `((:before ,root)
                 (:primary ,root)
                 (:before ,child1)
                 (:primary ,child1)
                 (:after ,child1)
                 (:before ,child2)
                 (:primary ,child2)
                 (:after ,child2)
                 (:after ,root))
               (reverse *methods-called*))))

  ;; changing child depth should change iteration order
  (let* ((root (make-instance 'update-test))
         (child1 (make-instance 'update-test :parent root :depth -1))
         (child2 (make-instance 'update-test :parent root :depth 1))
         *methods-called*)
    (update root)
    (is (equal `((:before ,root)
                 (:primary ,root)
                 (:before ,child2)
                 (:primary ,child2)
                 (:after ,child2)
                 (:before ,child1)
                 (:primary ,child1)
                 (:after ,child1)
                 (:after ,root))
               (reverse *methods-called*)))))

(test (component-update-children
       :depends-on (and component-with-nested-children
                        component-update-no-children))
  (let* ((root (make-instance 'update-test))
         (child (make-instance 'update-test :parent root))
         (grandchild (make-instance 'update-test :parent child))
         *methods-called*)
    (update root)
    (is (equal `((:before ,root)
                 (:primary ,root)
                 (:before ,child)
                 (:primary ,child)
                 (:before ,grandchild)
                 (:primary ,grandchild)
                 (:after ,grandchild)
                 (:after ,child)
                 (:after ,root))
               (reverse *methods-called*)))))

