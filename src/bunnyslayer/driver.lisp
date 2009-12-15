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

(in-package :bunnyslayer)

(defclass bunnyslayer-game (game) ())

(defclass hero (sprite mobile) ())

(defmethod initialize-instance :after ((hero hero) &key)
  (bind-key-down hero :sdl-key-up    #'move-north)
  (bind-key-up   hero :sdl-key-up    #'stop-north)
  (bind-key-down hero :sdl-key-down  #'move-south)
  (bind-key-up   hero :sdl-key-down  #'stop-south)
  (bind-key-down hero :sdl-key-left  #'move-west)
  (bind-key-up   hero :sdl-key-left  #'stop-west)
  (bind-key-down hero :sdl-key-right #'move-east)
  (bind-key-up   hero :sdl-key-right #'stop-east))

(defmethod move-north ((hero hero) event)
  (incf (veloc hero) #c(0 -2)))

(defmethod stop-north ((hero hero) event)
  (decf (veloc hero) #c(0 -2)))

(defmethod move-south ((hero hero) event)
  (incf (veloc hero) #c(0 2)))

(defmethod stop-south ((hero hero) event)
  (decf (veloc hero) #c(0 2)))

(defmethod move-west ((hero hero) event)
  (incf (veloc hero) #c(-2 0)))

(defmethod stop-west ((hero hero) event)
  (decf (veloc hero) #c(-2 0)))

(defmethod move-east ((hero hero) event)
  (incf (veloc hero) #c(2 0)))

(defmethod stop-east ((hero hero) event)
  (decf (veloc hero) #c(2 0)))

(defmethod game-init ((game bunnyslayer-game))
  (let ((root (make-instance 'component))
        (size #c(800 600))
        (sheet
         (make-instance
          'sheet
          :source
          (merge-pathnames "disp/sheet.png" *resource-pathname-defaults*))))
    (let ((hero (make-instance
                 'hero :parent root :offset (/ size 2)
                 :image (make-instance 'image :name :hero))))
      (subscribe (game-keys game) hero))
    (setf (game-root game) root
          (game-view game)
          (make-instance 'component :offset #c(0 0) :size size))))

(defmethod render :before ((game bunnyslayer-game) xy zmin zmax)
  (activate (make-instance 'sheet :name :sheet)))

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