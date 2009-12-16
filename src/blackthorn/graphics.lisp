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

(in-package :blackthorn-graphics)

;;;
;;; Graphics Utilities
;;;

(defun window (size &optional title-caption icon-caption)
  "@arg[size]{A complex number.}
   @arg[title-caption]{A string.}
   @arg[icon-caption]{A string.}
   @short{Creates a window of the specified size. Optional strings may be
     provided to specify the caption of the window when visible, and
     minimized.}"
  (sdl:window (x size) (y size) :bpp 32 :flags sdl:sdl-opengl
              :title-caption title-caption :icon-caption icon-caption)
  (gl:viewport 0 0 (x size) (y size)))

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

;;;
;;; Sprite Sheets
;;;

(defclass sheet ()
  ((name
    :reader name
    :initarg :name)
   (source
    :initarg :source)
   texture
   (size
    :reader size)))

(defvar *sheets* (make-hash-table))

(defun unload-graphics ()
  (gl:delete-textures
   (loop for sheet being the hash-values in *sheets*
      collect (slot-value sheet 'texture)
      do (slot-makunbound sheet 'texture))))

(defmethod make-instance ((class (eql (find-class 'sheet))) &key name source)
  (or (when name (gethash name *sheets*))
      (when (not source) (error "No such sprite-sheet named ~a." name))
      (let ((sheet (call-next-method)))
        (setf (gethash (name sheet) *sheets*) sheet))))

(defmethod make-instance ((class (eql 'sheet)) &rest initargs)
  (apply #'make-instance (find-class 'sheet) initargs))

(defmethod initialize-instance :after ((sheet sheet) &key source)
  (let ((config (make-pathname :type "config" :defaults source)))
    (assert (probe-file source) (source) "Source file \"~a\" not found." source)
    (assert (probe-file config) (config) "Config file \"~a\" not found." config)
    (let ((options
           (with-open-file (s config) (with-standard-io-syntax (read s)))))
      (setf (slot-value sheet 'name) (cadr (assoc :name options)))
      (labels ((coord (key alist) (apply #'complex (cdr (assoc key alist))))
               (div (a b) (complex (/ (x a) (x b)) (/ (y a) (y b)))))
        (let ((sheet-size (coord :size options)))
          (iter (for image in (cdr (assoc :images options)))
                (let ((offset (coord :offset (cdr image)))
                      (size (coord :size (cdr image))))
                  (make-instance 'image
                                 :name (car image)
                                 :sheet sheet
                                 :size size
                                 :tex-offset (div offset sheet-size)
                                 :tex-size (div size sheet-size)))))))))

(defmethod texture ((sheet sheet))
  (if (slot-boundp sheet 'texture)
      (slot-value sheet 'texture)
      (with-slots (source) sheet
        (multiple-value-bind (texture surface) (load-image-to-texture source)
          (setf (slot-value sheet 'texture) texture)
          (values texture surface)))))

(defmethod activate ((sheet sheet))
  (gl:bind-texture :texture-2d (texture sheet)))

;;;
;;; Images
;;;

(defclass image ()
  ((name
    :reader name
    :initarg :name)
   (sheet
    :initarg :sheet
    :reader sheet)
   (size
    :initarg :size
    :reader size)
   (tex-offset
    :initarg :tex-offset
    :reader tex-offset)
   (tex-size
    :initarg :tex-size
    :reader tex-size)))

(defvar *images* (make-hash-table))

(defmethod make-instance ((class (eql (find-class 'image))) &key name sheet)
  (or (when name (gethash name *images*))
      (unless sheet (error "No such image named ~a." name))
      (setf (gethash name *images*) (call-next-method))))

(defmethod make-instance ((class (eql 'image)) &rest initargs)
  (apply #'make-instance (find-class 'image) initargs))

(defmethod draw ((image image) xy z)
  (with-slots (size tex-offset tex-size) image
    (let* ((x1 (x xy)) (x2 (+ x1 (x size)))
           (y1 (y xy)) (y2 (+ y1 (y size)))
           (tx1 (x tex-offset)) (tx2 (+ tx1 (x tex-size)))
           (ty1 (y tex-offset)) (ty2 (+ ty1 (y tex-size))))
      (gl:tex-coord tx1 ty1) (gl:vertex x1 y1 z)
      (gl:tex-coord tx2 ty1) (gl:vertex x2 y1 z)
      (gl:tex-coord tx2 ty2) (gl:vertex x2 y2 z)
      (gl:tex-coord tx1 ty2) (gl:vertex x1 y2 z))))
