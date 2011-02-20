;;;; Blackthorn -- Lisp Game Engine
;;;;
;;;; Copyright (c) 2007-2011, Elliott Slaughter <elliottslaughter@gmail.com>
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
;;; Complex numbers
;;;

(defun unit (x)
  (if (zerop x) x (/ x (abs x))))

(defun dot (x y)
  (+ (* (realpart x) (realpart y)) (* (imagpart x) (imagpart y))))

(defun proj (x y)
  (* (unit y) (/ (dot x y) (abs y))))

(defun norm (x y)
  (- x (proj x y)))

(defun dist (x y)
  (abs (- x y)))

(defun theta (x)
  (atan (imagpart x) (realpart x)))

(defun polar (a)
  (complex (cos a) (sin a)))

(defun rot (x a)
  (* (polar (+ (theta x) a)) (abs x)))

(defun cross (x y)
  (- (* (realpart x) (imagpart y)) (* (imagpart x) (realpart y))))

;;;
;;; Lists
;;;

(defun intersperse (x ys)
  (cons (car ys)
        (mapcan #'(lambda (y) (list x y)) (cdr ys))))

(defun ordered-subset-p (list1 list2 &key (key #'identity) (test #'eql))
  (if list2
      (if (funcall test (funcall key (car list1)) (funcall key (car list2)))
          (ordered-subset-p (cdr list1) (cdr list2) :key key :test test)
          (ordered-subset-p list1 (cdr list2) :key key :test test))
      (not list1)))

(defun alref (item alist &key key (test nil test-p))
  (cadr (apply #'assoc item alist
               (append (list :key key) (if test-p (list :test test))))))

(defun alrest (item alist &key key (test nil test-p))
  (cdr (apply #'assoc item alist
              (append (list :key key) (if test-p (list :test test))))))

(defun select (table)
  (when table
    (let ((n (random (reduce #'+ (mapcar #'car table)))))
      (reduce #'(lambda (n entry)
                  (if (>= (- n (car entry)) 0)
                      (- n (car entry))
                      (return-from select (cdr entry))))
              (cons n table)))))

(defun all-pairs (list)
  (when list
    (nconc (loop for elt in (cdr list) collect (list (car list) elt))
           (all-pairs (cdr list)))))

(defun list->table (alist)
  (let ((table (make-hash-table)))
    (loop for (k v) in alist
       do (setf (gethash k table) v))
    table))

;;;
;;; Anaphora
;;;

(defmacro aif (test-form then-form &optional else-form)
  `(let ((it ,test-form))
     (if it ,then-form ,else-form)))

(defmacro acond (&rest clauses)
  (if (null clauses)
      nil
      (let ((cl1 (car clauses))
            (sym (gensym)))
        `(let ((,sym ,(car cl1)))
           (if ,sym
               (let ((it ,sym)) ,@(cdr cl1))
               (acond ,@(cdr clauses)))))))

;;;
;;; Macros
;;;

(defmacro once-only ((&rest names) &body body)
  (let ((gensyms (loop for n in names collect (gensym))))
    `(let (,@(loop for g in gensyms collect `(,g (gensym))))
      `(let (,,@(loop for g in gensyms for n in names collect ``(,,g ,,n)))
        ,(let (,@(loop for n in names for g in gensyms collect `(,n ,g)))
           ,@body)))))

(eval-when (:compile-toplevel)
  (defmacro with-gensyms (syms &body body)
    `(let ,(mapcar #'(lambda (s)
                       `(,s (gensym)))
                   syms)
       ,@body)))

(defmacro defmemo (name params &body body)
  (with-gensyms (cache args value exists)
    `(let ((,cache (make-hash-table :test #'equal)))
       (defun ,name (&rest ,args)
         (multiple-value-bind (,value ,exists) (gethash ,args ,cache)
           (if ,exists
               ,value
               (setf (gethash ,args ,cache)
                     (apply #'(lambda ,params ,@body) ,args))))))))

(defmacro defweakmemo (name params &body body)
  (with-gensyms (cache args value exists)
    `(let ((,cache
            (tg:make-weak-hash-table :test #'equal #-(or clozure ecl) :weakness #-(or clozure ecl) :key)))
       (defun ,name (&rest ,args)
         (multiple-value-bind (,value ,exists) (gethash ,args ,cache)
           (if ,exists
               ,value
               (setf (gethash ,args ,cache)
                     (apply #'(lambda ,params ,@body) ,args))))))))

;;;
;;; Garbage Collection
;;;

(defun gc (&key full verbose)
  (trivial-garbage:gc :full full :verbose verbose))
