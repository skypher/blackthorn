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
;;; Macros
;;;

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
