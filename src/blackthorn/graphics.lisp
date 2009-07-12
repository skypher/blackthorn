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
;;; Open GL Texture Wrapper
;;;

(defclass image ()
  ((name
    :reader name
    :initarg :name)
   (source
    :initarg :source)
   (texture))
  (:documentation
   "Wrapper over an Open GL texture."))

(defvar *images* (make-hash-table))

(defmethod make-instance ((class (eql (find-class 'image)))
                          &rest initargs &key name source)
  (or (when name (gethash name *images*))
      (when (not source) (error "No such image named ~a." name))
      (let ((source (enough-namestring (truename source))))
        (apply #'call-next-method :source source initargs))))

(defmethod make-instance ((class (eql 'image)) &rest initargs)
  (apply #'make-instance (find-class 'image) initargs))

(defun pixel-format-slot (surface slot-name)
  (cffi:foreign-slot-value
   (sdl-base::pixel-format (sdl:fp surface))
   'sdl-cffi::sdl-pixel-format
   slot-name))

;; Two versions of load-image:
;; The first checks bytes-per-pixel and the red-mask manually to try to manually
;; decide the pixel format to give GL.
;; The second uses load-and-convert-image to try to do this automatically.
;; Only the first version works.
(defun load-image (source)
  (assert (probe-file source))
  (let ((texture (car (gl:gen-textures 1)))
        (surface (sdl-image:load-image source)))
    (gl:bind-texture :texture-2d texture)
    (gl:tex-parameter :texture-2d :texture-min-filter :nearest)
    (gl:tex-parameter :texture-2d :texture-mag-filter :nearest)

    (let* ((w (sdl:width surface)) (h (sdl:height surface))
           (bpp (pixel-format-slot surface 'sdl-cffi::bytesperpixel))
           (format (cond ((= bpp 4)
                          (if (= (pixel-format-slot surface 'sdl-cffi::rmask)
                                 #x000000ff)
                              :rgba
                              :bgra))
                         ((= bpp 3)
                          (if (= (pixel-format-slot surface 'sdl-cffi::rmask)
                                 #x000000ff)
                              :rgb
                              :bgr))
                         (t (error "Image \"~a\" is not truecolor." source)))))
      (gl:tex-image-2d
       :texture-2d 0 bpp w h 0 format :unsigned-byte
       (sdl-base::with-pixel (pixels (sdl:fp surface))
         (sdl-base::pixel-data pixels))))
    texture))

#+nil
(defun load-image (source)
  (assert (probe-file source))
  (let ((texture (car (gl:gen-textures 1)))
        (surface (sdl-image:load-and-convert-image source)))
    (gl:bind-texture :texture-2d texture)
    (gl:tex-parameter :texture-2d :texture-min-filter :nearest)
    (gl:tex-parameter :texture-2d :texture-mag-filter :nearest)

    (let ((w (sdl:width surface)) (h (sdl:height surface)))
      (gl:tex-image-2d
       :texture-2d 0 :rgba w h 0 :rgba :unsigned-byte
       (sdl-base::with-pixel (pixels (sdl:fp surface))
         (sdl-base::pixel-data pixels))))
    texture))