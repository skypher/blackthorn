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

(in-package :bunnyslayer)

(defclass bunnyslayer-game (game) ())

(defclass hero (sprite mobile actor) ())

(defmethod initialize-instance :after ((hero hero) &key)
  (define-event-handlers (event) hero
    (move-north (and (event-type-p event :key-down)
                     (eql (event-key event) :sdl-key-up))
      (incf (veloc hero) #c(0 -2)))
    (move-north (and (event-type-p event :key-up)
                     (eql (event-key event) :sdl-key-up))
      (decf (veloc hero) #c(0 -2)))
    (move-south (and (event-type-p event :key-down)
                     (eql (event-key event) :sdl-key-down))
      (incf (veloc hero) #c(0 2)))
    (move-south (and (event-type-p event :key-up)
                     (eql (event-key event) :sdl-key-down))
      (decf (veloc hero) #c(0 2)))
    (move-west (and (event-type-p event :key-down)
                     (eql (event-key event) :sdl-key-left))
      (incf (veloc hero) #c(-2 0)))
    (move-west (and (event-type-p event :key-up)
                     (eql (event-key event) :sdl-key-left))
      (decf (veloc hero) #c(-2 0)))
    (move-east (and (event-type-p event :key-down)
                     (eql (event-key event) :sdl-key-right))
      (incf (veloc hero) #c(2 0)))
    (move-east (and (event-type-p event :key-up)
                     (eql (event-key event) :sdl-key-right))
      (decf (veloc hero) #c(2 0)))))

(defmethod init-game ((game bunnyslayer-game))
  (let ((root (make-instance 'component))
        (size #c(800 600))
        (texture-pathname
         (merge-pathnames "disp/hero.png"
                          blt-user::*resource-directory-pathname*)))
    (let ((hero (make-instance
                 'hero :parent root :offset (/ size 2)
                 :image (make-instance 'image :name 'tex
                                       :source texture-pathname))))
      (subscribe-event (key-subscription game) hero))
    (setf (game-root game) root
          (game-view game)
          (make-instance 'component :offset #c(0 0) :size size))))

(defmethod update :after ((game bunnyslayer-game))
  ;; report the frame reate
  (let ((s (format nil "fps: ~,2f" (sdl:average-fps))))
    (set-caption s s)))

;; For interactive use:
(defun bunnyslayer ()
  (let ((*game* (make-instance 'bunnyslayer-game)))
    (main :exit-when-done nil)))

;; For non-interactive use:
(defvar *game* (make-instance 'bunnyslayer-game))