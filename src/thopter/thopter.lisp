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

(in-package :thopter)

(defclass thopter-game (game) ())

(defclass thopter (sprite mobile) ())

(defmethod initialize-instance :after ((thopter thopter) &key)
  (bind-key-down thopter :sdl-key-up    #'move-north)
  (bind-key-up   thopter :sdl-key-up    #'stop-north)
  (bind-key-down thopter :sdl-key-down  #'move-south)
  (bind-key-up   thopter :sdl-key-down  #'stop-south)
  (bind-key-down thopter :sdl-key-left  #'move-west)
  (bind-key-up   thopter :sdl-key-left  #'stop-west)
  (bind-key-down thopter :sdl-key-right #'move-east)
  (bind-key-up   thopter :sdl-key-right #'stop-east))

(defmethod move-north ((thopter thopter) event)
  (incf (veloc thopter) #c(0 -2)))

(defmethod stop-north ((thopter thopter) event)
  (decf (veloc thopter) #c(0 -2)))

(defmethod move-south ((thopter thopter) event)
  (incf (veloc thopter) #c(0 2)))

(defmethod stop-south ((thopter thopter) event)
  (decf (veloc thopter) #c(0 2)))

(defmethod move-west ((thopter thopter) event)
  (incf (veloc thopter) #c(-2 0)))

(defmethod stop-west ((thopter thopter) event)
  (decf (veloc thopter) #c(-2 0)))

(defmethod move-east ((thopter thopter) event)
  (incf (veloc thopter) #c(2 0)))

(defmethod stop-east ((thopter thopter) event)
  (decf (veloc thopter) #c(2 0)))

(defmethod game-init ((game thopter-game))
  (let ((root (make-instance 'component))
        (size #c(800 600)))
    (setf (game-root game) root
          (game-view game) (make-instance 'component :size size)
          (game-sheet game)
          (make-instance 'sheet :source (resource "disp/thopter.png")))
    (let ((thopter (make-instance
                 'thopter :parent root :offset (/ size 2)
                 :image (make-instance 'image :name :thopter))))
      (subscribe (game-keys game) thopter))))

(defmethod game-update :after ((game thopter-game))
  ;; report the frame reate
  (let ((s (format nil "fps: ~,2f" (sdl:average-fps))))
    (set-caption s s)))

;; For interactive use:
(defun thopter ()
  (let ((*game* (make-instance 'thopter-game)))
    (main :exit-when-done nil)))

;; For non-interactive use:
(defvar *game* (make-instance 'thopter-game))