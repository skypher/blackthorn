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

(in-package :blackthorn-graphics)

;;;
;;; Graphics Utilities
;;;

(defvar *font-width* 256)
(defvar *font-height* 256)
(defvar *font-char-code-min* 0)
(defvar *font-char-code-max* 255)

(defun font-to-surface (font)
  (let* ((char-width (sdl:char-width font))
         (char-height (sdl:char-height font))
         (row-length (floor *font-width* char-width))
         (row-count (ceiling (- *font-char-code-max* *font-char-code-min*)
                             row-length))
         (surface (sdl:create-surface
                   *font-width* *font-height* :bpp 32 :pixel-alpha t)))
    (assert (< (* row-count char-height) *font-height*))
    (sdl:fill-surface sdl:*black* :surface surface)
    (iter (for r from 0 below row-count)
          (for c0 from *font-char-code-min*
               below *font-char-code-max* by row-length)
          (let ((string
                 (iter (for c from c0
                            below (min (+ c0 row-length) *font-char-code-max*))
                       (collect (code-char c) result-type string))))
            (sdl:draw-string-solid-*
             string 0 (* r char-height)
             :font font :color sdl:*white* :surface surface)))
    surface))

(defun font-to-texture (font)
  (surface-to-texture (font-to-surface font)))

;;;
;;; Fonts
;;;

(defclass font ()
  ((name
    :reader name
    :initarg :name)
   (source
    :initarg :source)
   sdl-font
   char-size
   texture))

(defvar *fonts*
  (let ((table (make-hash-table)))
    (setf (gethash :font-10x20 table)
          (make-instance 'font :name :font-10x20 :source sdl:*font-10x20*)
          (gethash :font-5x7 table)
          (make-instance 'font :name :font-5x7 :source sdl:*font-5x7*)
          (gethash :font-5x8 table)
          (make-instance 'font :name :font-5x8 :source sdl:*font-5x8*)
          (gethash :font-6x10 table)
          (make-instance 'font :name :font-6x10 :source sdl:*font-6x10*)
          (gethash :font-6x12 table)
          (make-instance 'font :name :font-6x12 :source sdl:*font-6x12*)
          (gethash :font-6x13 table)
          (make-instance 'font :name :font-6x13 :source sdl:*font-6x13*)
          (gethash :font-6x13b table)
          (make-instance 'font :name :font-6x13b :source sdl:*font-6x13b*)
          (gethash :font-6x13o table)
          (make-instance 'font :name :font-6x13o :source sdl:*font-6x13o*)
          (gethash :font-6x9 table)
          (make-instance 'font :name :font-6x9 :source sdl:*font-6x9*)
          (gethash :font-7x13 table)
          (make-instance 'font :name :font-7x13 :source sdl:*font-7x13*)
          (gethash :font-7x13b table)
          (make-instance 'font :name :font-7x13b :source sdl:*font-7x13b*)
          (gethash :font-7x13o table)
          (make-instance 'font :name :font-7x13o :source sdl:*font-7x13o*)
          (gethash :font-7x14 table)
          (make-instance 'font :name :font-7x14 :source sdl:*font-7x14*)
          (gethash :font-7x14b table)
          (make-instance 'font :name :font-7x14b :source sdl:*font-7x14b*)
          (gethash :font-8x13 table)
          (make-instance 'font :name :font-8x13 :source sdl:*font-8x13*)
          (gethash :font-8x13b table)
          (make-instance 'font :name :font-8x13b :source sdl:*font-8x13b*)
          (gethash :font-8x13o table)
          (make-instance 'font :name :font-8x13o :source sdl:*font-8x13o*)
          (gethash :font-8x8 table)
          (make-instance 'font :name :font-8x8 :source sdl:*font-8x8*)
          (gethash :font-9x15 table)
          (make-instance 'font :name :font-9x15 :source sdl:*font-9x15*)
          (gethash :font-9x15b table)
          (make-instance 'font :name :font-9x15b :source sdl:*font-9x15b*)
          (gethash :font-9x18 table)
          (make-instance 'font :name :font-9x18 :source sdl:*font-9x18*)
          (gethash :font-9x18b table)
          (make-instance 'font :name :font-9x18b :source sdl:*font-9x18b*))
    table))

(defun load-font (name)
  (let ((font (gethash name *fonts*)))
    (unless (slot-boundp font 'texture)
      (with-slots (source sdl-font char-size texture) font
        (setf sdl-font (sdl:initialise-font source)
              char-size (complex (sdl:char-width sdl-font)
                                 (sdl:char-height sdl-font))
              texture (font-to-texture sdl-font))))
    font))

;;;
;;; Text
;;;

(defclass text ()
  ((source
    :initarg :source)
   (font
    :initarg :font)
   (color
    :initarg :color)
   (size
    :reader size)
   (bbox-offset
    :reader bbox-offset
    :initform #c(0 0))
   (bbox-size
    :reader bbox-size
    :initform #c (0 0))
   ))

(defmethod initialize-instance :after ((text text) &key)
  (with-slots (source font size) text
    (with-slots (char-size) font
      (setf size (complex (* (length source) (x char-size)) (y char-size))))))

(defmethod draw ((text text) xy z)
  (with-slots (source font color size) text
    (with-slots (texture char-size) font
      (gl:end)
      (gl:bind-texture :texture-2d texture)
      (gl:with-primitive :quads
        (let* ((width-f (* *font-width* 1d0)) (height-f (* *font-height* 1d0))
               (row-length (floor *font-width* (x char-size))))
          (iter (for c in-string source) (for i from 0 by (x char-size))
                (assert
                 (<= *font-char-code-min* (char-code c) *font-char-code-max*))
                (let* ((x1 (+ (truncate (x xy)) i))
                       (x2 (+ x1 (x char-size)))
                       (y1 (truncate (y xy)))
                       (y2 (+ y1 (y char-size)))
                       (tx1 (* (mod (- (char-code c) *font-char-code-min*)
                                    row-length)
                               (/ (x char-size) width-f)))
                       (tx2 (+ tx1 (/ (x char-size) width-f)))
                       (ty1 (* (floor (- (char-code c) *font-char-code-min*)
                                      row-length)
                               (/ (y char-size) height-f)))
                       (ty2 (+ ty1 (/ (y char-size) height-f))))
                  (gl:tex-coord tx1 ty1) (gl:vertex x1 y1 z)
                  (gl:tex-coord tx2 ty1) (gl:vertex x2 y1 z)
                  (gl:tex-coord tx2 ty2) (gl:vertex x2 y2 z)
                  (gl:tex-coord tx1 ty2) (gl:vertex x1 y2 z)))))
      (gl:bind-texture :texture-2d *active-texture*)
      (gl:begin :quads))))

(defmethod next-image ((text text))
  (declare (ignore text)))

;;;
;;; Creation Utilities
;;;

(defun make-font (name)
  (load-font name))

(defun make-text (string font)
  (make-instance 'text :source string :font font :color sdl:*white*))
