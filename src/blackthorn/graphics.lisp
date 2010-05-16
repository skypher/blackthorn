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

(defun ceiling-expt (x y)
  (if (zerop x) x (expt y (ceiling (log x y)))))

(defun load-and-convert-images (sources options)
  (labels ((color (rgba)
             (when rgba
               (destructuring-bind (r g b &optional a) rgba
                 (apply #'sdl:color :r r :g g :b b (if a (list :a a))))))
           (point (xy)
             (when xy
               (destructuring-bind (x y) xy
                 (funcall #'sdl:point :x x :y y)))))
    (iter (for source in sources) (assert (probe-file source)))
    (let* ((images
            (iter (for source in sources)
                  (for option in options)
                  (collect
                   (sdl-image:load-image
                    source
                    :color-key (color (cdr (assoc :color-key option)))
                    :color-key-at (point (cdr (assoc :color-key-at option)))))))
           (total-width
            (ceiling-expt (or (iter (for image in images)
                                    (sum (sdl:width image)))
                              0)
                          2))
           (total-height
            (ceiling-expt (or (iter (for image in images)
                                    (maximize (sdl:height image)))
                              0)
                          2))
           (surface (sdl:create-surface
                     total-width total-height :bpp 32 :pixel-alpha t)))
      (iter (with x = 0)
            (for image in images)
            (sdl:draw-surface-at-* image x 0 :surface surface)
            (incf x (sdl:width image)))
      surface)))

(defun surface-to-texture (surface)
  (let ((texture (car (gl:gen-textures 1)))
        (w (sdl:width surface)) (h (sdl:height surface)))
    (gl:bind-texture :texture-2d texture)
    (gl:tex-parameter :texture-2d :texture-min-filter :nearest)
    (gl:tex-parameter :texture-2d :texture-mag-filter :nearest)
    (gl:tex-image-2d
       :texture-2d 0 :rgba w h 0 :rgba :unsigned-byte
       (sdl-base::with-pixel (pixels (sdl:fp surface))
         (sdl-base::pixel-data pixels)))
    texture))

(defun load-source-to-texture (source &optional options)
  (surface-to-texture
   (load-and-convert-images (if (listp source) source (list source)) options)))

;;;
;;; Sprite Sheets
;;;

(defclass sheet ()
  ((name
    :reader name
    :initarg :name)
   (source
    :initarg :source)
   options
   texture
   (size
    :reader size)))

(defvar *sheets* (make-hash-table))

(defun unload-graphics ()
  (gl:delete-textures
   (loop for sheet being the hash-values in *sheets*
      collect (slot-value sheet 'texture)
      do (slot-makunbound sheet 'texture)))
  (clrhash *sheets*))

(defun load-sheet (source &key name)
  (let ((sheet (make-instance 'sheet :name name :source source)))
    (setf (gethash (name sheet) *sheets*) sheet)))

(defun parse-config-file (source)
  (let ((config (make-pathname :type "config" :defaults source)))
    (assert (probe-file source) (source) "Source file \"~a\" not found." source)
    (assert (probe-file config) (config) "Config file \"~a\" not found." config)
    (let ((options (with-open-file (s config) (with-standard-io-syntax (read s)))))
      options)))

(defun process-config-file (sheet options file-offset)
  (labels ((coord (key alist &optional (default nil default-p)) 
		  (if default-p 
		      (let ((result-key (cdr (assoc key alist))))
			(if result-key (apply #'complex result-key) default))
		    (apply #'complex (cdr (assoc key alist)))))
           (div (a b) (complex (/ (x a) (x b)) (/ (y a) (y b)))))
    (with-slots ((total-size size)) sheet
      (iter (for image in (cdr (assoc :images options)))
            (let* ((offset (coord :offset image))
                   (size (coord :size image))
		   (bbox-offset (coord :bbox-offset image #c(0 0)))
		   (bbox-size (coord :bbox-size image size)))
              (init-image :name (cadr (assoc :name image))
                          :sheet sheet
                          :size size
			  :bbox-offset bbox-offset
			  :bbox-size bbox-size
                          :tex-offset (div (+ file-offset offset) total-size)
                          :tex-size (div size total-size)))))
    (iter (for anim in (cdr (assoc :anims options)))
          (init-anim
           :name (cadr (assoc :name anim))
           :timescale (or (cadr (assoc :timescale anim))
                          (cadr (assoc :timescale options))
                          1)
           :images (iter (for i in (cdr (assoc :images anim)))
                         (collect (find-image i) result-type vector))))))

(defun flip (f) #'(lambda (y x) (funcall f x y)))

(defmethod initialize-instance :after ((sheet sheet) &key source)
  (labels ((coord (key alist) (apply #'complex (cdr (assoc key alist))))
           (forever (x) (let ((l (list x))) (setf (cdr l) l) l)))
    (with-slots (name size options) sheet
      (unless (listp source) (setf source (list source)))
      (let* ((config-options (mapcar #'parse-config-file source))
             (total-size
              (iter (for o in config-options)
                    (sum (x (coord :size o)) into x)
                    (maximize (y (coord :size o)) into y)
                    (finally (return (complex (ceiling-expt x 2)
                                              (ceiling-expt y 2))))))
             (file-offsets
              (iter (with x = 0)
                    (for o in config-options)
                    (collect x)
                    (incf x (x (coord :size o))))))
        (unless (slot-boundp sheet 'name)
          (let ((config-name
                 (if (not (cdr source)) (assoc :name (car config-options)))))
            (if config-name
                (setf name config-name)
                (error "Name must be specified for composite sheet."))))
        (setf size total-size
              options (mapcar #'(lambda (o)
                                  (remove '(:name :size :images :anims) o
                                          :test (flip #'member) :key #'car))
                              config-options))
        (mapcar #'process-config-file
                (forever sheet) config-options file-offsets)))))

(defmethod texture ((sheet sheet))
  (if (slot-boundp sheet 'texture)
      (slot-value sheet 'texture)
      (with-slots (source options) sheet
        (multiple-value-bind (texture surface) (load-source-to-texture source options)
          (setf (slot-value sheet 'texture) texture)
          (values texture surface)))))

(defvar *active-texture*)

(defmethod activate ((sheet sheet))
  (gl:bind-texture :texture-2d (texture sheet))
  (setf *active-texture* (texture sheet)))

;;;
;;; Images
;;;

(defclass image ()
  ((name
    :reader name
    :initarg :name)
   (sheet
    :reader sheet
    :initarg :sheet)
   (size
    :reader size
    :initarg :size)
   (bbox-offset
    :reader bbox-offset
    :initarg :bbox-offset)
   (bbox-size
    :reader bbox-size
    :initarg :bbox-size)
   (tex-offset
    :reader tex-offset
    :initarg :tex-offset)
   (tex-size
    :reader tex-size
    :initarg :tex-size)))

(defvar *images* (make-hash-table))

(defun init-image (&rest initargs &key name &allow-other-keys)
  (setf (gethash name *images*) (apply #'make-instance 'image initargs)))

(defun find-image (name)
  (or (gethash name *images*)
      (error "No such image named ~a." name)))

(defmethod draw ((image image) xy z)
  (with-slots (size tex-offset tex-size) image
    (let* ((x1 (truncate (x xy))) (x2 (+ x1 (x size)))
           (y1 (truncate (y xy))) (y2 (+ y1 (y size)))
           (tx1 (x tex-offset)) (tx2 (+ tx1 (x tex-size)))
           (ty1 (y tex-offset)) (ty2 (+ ty1 (y tex-size))))
      (gl:tex-coord tx1 ty1) (gl:vertex x1 y1 z)
      (gl:tex-coord tx2 ty1) (gl:vertex x2 y1 z)
      (gl:tex-coord tx2 ty2) (gl:vertex x2 y2 z)
      (gl:tex-coord tx1 ty2) (gl:vertex x1 y2 z))))

(defmethod next-image ((image image))
  ;; nop so that next-image can be used on images which are not anims
  (declare (ignore image)))

;;;
;;; Animations
;;;

(defclass anim ()
  ((name
    :reader name
    :initarg :name)
   (images
    :reader images
    :initarg :images)
   (timescale
    :reader timescale
    :initarg :timescale
    :initform 1)
   (index
    :reader index
    :initform 0)
   (size
    :reader size)
   (bbox-offset
    :reader bbox-offset)
   (bbox-size
    :reader bbox-size)))

(defvar *anims* (make-hash-table))

(defun init-anim (&rest initargs &key name &allow-other-keys)
  (setf (gethash name *anims*) (apply #'make-instance 'anim initargs)))

(defun find-anim (name)
  (let ((basis (gethash name *anims*)))
    (if basis
        (make-instance 'anim :name name
                       :images (images basis) :timescale (timescale basis))
        (error "No such anim named ~a." name))))

(defmethod initialize-instance :after ((anim anim) &key)
  (with-slots (images index size bbox-offset bbox-size) anim
    (setf size (size (aref images (truncate index)))
	  bbox-offset (bbox-offset (aref images (truncate index)))
	  bbox-size (bbox-size (aref images (truncate index))))))

(defmethod draw ((anim anim) xy z)
  (with-slots (images index) anim
    (draw (aref images (truncate index)) xy z)))

(defmethod next-image ((anim anim))
  (with-slots (images timescale index size bbox-offset bbox-size) anim
    (setf index (mod (+ index (/ 1 timescale)) (array-dimension images 0))
          size (size (aref images (truncate index)))
	  bbox-offset (bbox-offset (aref images (truncate index)))
	  bbox-size (bbox-size (aref images (truncate index))))))

;;;
;;; Creation Utilities
;;;

(defun format-name (name-or-format &rest format-args)
  (if (stringp name-or-format)
      (intern (apply #'format nil name-or-format format-args) :keyword)
      name-or-format))
  
(defun make-image (&rest name-or-format-args)
  (let ((name (apply #'format-name name-or-format-args)))
    (find-image name)))

(defun make-anim (&rest name-or-format-args)
  (let ((name (apply #'format-name name-or-format-args)))
    (find-anim name)))

(defun make-anim-or-image (&rest name-or-format-args)
  (let ((name (apply #'format-name name-or-format-args)))
    (if (gethash name *anims*)
        (find-anim name)
        (find-image name))))
