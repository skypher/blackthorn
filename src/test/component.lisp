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
    (is (zerop (fill-pointer (children root))))))

(test root-with-one-child
  (let* ((root (make-instance 'component))
         (child (make-instance 'component :parent root)))
    (is (eql root (parent child)))
    (is (= (fill-pointer (children root)) 1))
    (is (eql (aref (children root) 0) child))
    (let (seen)
      (blt-phys::do-children (c root)
        (if seen
            (fail "do-children returned more than one object")
            (setf seen c)))
      (is (eql seen child)))))

(test root-attach-dettach-child
  (let ((root (make-instance 'component))
         (child (make-instance 'component)))
    (is (not (parent child)))
    (is (zerop (fill-pointer (children root))))

    (attach root child)
    (is (eql root (parent child)))
    (is (= (fill-pointer (children root)) 1))
    (is (eql (aref (children root) 0) child))

    (dettach root child)
    (is (not (parent child)))
    (is (zerop (fill-pointer (children root))))))

(test root-with-two-children
  (let* ((root (make-instance 'component))
         (child1 (make-instance 'component :parent root :depth 1))
         (child2 (make-instance 'component :parent root :depth -1)))
    (is (eql root (parent child1)))
    (is (eql root (parent child2)))
    (is (= (fill-pointer (children root)) 2))
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
    (is (= (fill-pointer (children root)) 2))
    (is (eql (aref (children root) 0) child1))
    (is (eql (aref (children root) 1) child2))))
