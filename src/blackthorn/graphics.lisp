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

(in-package :blackthorn-graphics)

;;;
;;; Graphics Utilities
;;;

(declaim (inline x y))
(defun x (n) (realpart n))
(defun y (n) (imagpart n))

(defun window (size &optional title-caption icon-caption)
  (sdl:window (x size) (y size) :bpp 32 :flags sdl:sdl-opengl
              :title-caption title-caption :icon-caption icon-caption)
  (gl:viewport 0 0 (x size) (y size)))

;;;
;;; Open GL Texture Wrapper
;;;

(defgeneric name (object))

(defclass image ()
  ((name
    :reader name
    :initarg :name)
   (source
    :initarg :source)
   texture
   size)
  (:documentation
   "Wrapper over an Open GL texture."))

(defvar *images* (make-hash-table))

(defun unload-graphics ()
  (gl:delete-textures
   (loop for image being the hash-values in *images*
      collect (slot-value image 'texture)
      do (slot-makunbound image 'texture))))

(defmethod make-instance ((class (eql (find-class 'image)))
                          &rest initargs &key name source)
  (or (when name (gethash name *images*))
      (when (not source) (error "No such image named ~a." name))
      (let ((source (enough-namestring (truename source))))
        (setf (gethash name *images*)
              (apply #'call-next-method class :source source initargs)))))

(defmethod make-instance ((class (eql 'image)) &rest initargs)
  (apply #'make-instance (find-class 'image) initargs))

(defun load-and-convert-image (source)
  (assert (probe-file source))
  (let* ((image (sdl-image:load-image source))
         (w (sdl:width image)) (h (sdl:height image))
         (surface (sdl:create-surface  w h :bpp 32 :pixel-alpha t)))
    (sdl:blit-surface image surface)))

(defun load-image-to-texture (source)
  (assert (probe-file source))
  (let* ((texture (car (gl:gen-textures 1)))
         (surface (load-and-convert-image source))
         (w (sdl:width surface)) (h (sdl:height surface)))
    (gl:bind-texture :texture-2d texture)
    (gl:tex-parameter :texture-2d :texture-min-filter :nearest)
    (gl:tex-parameter :texture-2d :texture-mag-filter :nearest)
    (gl:tex-image-2d
       :texture-2d 0 :rgba w h 0 :rgba :unsigned-byte
       (sdl-base::with-pixel (pixels (sdl:fp surface))
         (sdl-base::pixel-data pixels)))
    (values texture surface)))

(defun texture (image)
  (declare (type image image))
  (if (slot-boundp image 'texture)
      (slot-value image 'texture)
      (with-slots (source) image
        (multiple-value-bind (texture surface) (load-image-to-texture source)
          (setf (slot-value image 'texture) texture
                (slot-value image 'size)
                (complex (sdl:width surface) (sdl:height surface)))
          (values texture surface)))))

(defgeneric size (object))
(defmethod size ((image image))
  ;; Automatically load the image texture.
  (texture image)
  (slot-value image 'size))

(defgeneric draw (object xy z))
(defmethod draw ((image image) xy z)
  (let* ((size (size image))
         (x1 (x xy)) (x2 (+ x1 (x size)))
         (y1 (y xy)) (y2 (+ y1 (y size))))
    (gl:tex-coord 0 0) (gl:vertex x1 y1 z)
    (gl:tex-coord 1 0) (gl:vertex x2 y1 z)
    (gl:tex-coord 1 1) (gl:vertex x2 y2 z)
    (gl:tex-coord 0 1) (gl:vertex x1 y2 z)))
