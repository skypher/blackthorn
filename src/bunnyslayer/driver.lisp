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

(in-package :bunnyslayer)

(defclass bunnyslayer-game (game) ())

(defclass flagable (actor)
  ((flags
    :reader flags
    :initform (make-hash-table))))

(defmethod get-flag ((flagable flagable) flag)
  (with-slots (flags) flagable
    (gethash flag flags)))

(defun set-flag (flag value)
  #'(lambda (flagable event)
      (with-slots (flags) flagable (setf (gethash flag flags) value))))

(defclass faceable (flagable)
  ((facing
    :reader facing
    :initarg :facing)))

(defvar *cardinals* '(:north :south :west :east))

(defmethod change-facing ((faceable faceable) event)
  (with-slots (facing) faceable
    (setf facing
          (if (not (get-flag faceable facing))
              (or (iter (for d in *cardinals*)
                        (when (get-flag faceable d) (return d)))
                  facing)
              facing))))

(defclass hero (sprite mobile faceable)
  ((facing
    :initform :south)))

(defun doall (&rest handlers)
  #'(lambda (object event)
      (iter (for handler in handlers) (funcall handler object event))))

(defun incf-veloc (x)
  #'(lambda (object event) (with-slots (veloc) object (incf veloc x))))

(defun decf-veloc (x)
  #'(lambda (object event) (with-slots (veloc) object (decf veloc x))))

;; TODO: generalize this to apply to any class with facings and/or actions
(defmethod change-image ((hero hero) event)
  (with-slots (image facing) hero
    (setf image (make-instance
                 'anim :name (intern (format nil "HERO-~a-WALK" facing)
                                     :keyword)))))

(defmethod initialize-instance :after ((hero hero) &key)
  (iter (for (d k v) in '((:north :sdl-key-up #c(0 -2))
                          (:south :sdl-key-down #c(0 2))
                          (:west :sdl-key-left #c(-2 0))
                          (:east :sdl-key-right #c(2 0))))
        (bind-key-down hero k (doall (set-flag d t) (incf-veloc v)
                                     #'change-facing #'change-image))
        (bind-key-up hero k (doall (set-flag d nil) (decf-veloc v)
                                   #'change-facing #'change-image)))
  (change-image hero nil))

(defmethod game-init ((game bunnyslayer-game))
  (let ((root (make-instance 'component))
        (size #c(800 600)))
    (setf (game-root game) root
          (game-view game) (make-instance 'component :size size)
          (game-sheet game)
          (make-instance 'sheet :name :sheet
                         :source (directory (resource "disp/refmap/*.png"))))
    (let ((hero (make-instance 'hero :parent root :offset (/ size 2))))
      (subscribe (game-keys game) hero))))

(defmethod game-update :after ((game bunnyslayer-game))
  ;; report the frame reate
  (let ((s (format nil "fps: ~,2f" (sdl:average-fps))))
    (set-caption s s)))

;; For interactive use:
(defun bunnyslayer ()
  (let ((*game* (make-instance 'bunnyslayer-game)))
    (main :exit-when-done nil)))

;; For non-interactive use:
(defvar *game* (make-instance 'bunnyslayer-game))