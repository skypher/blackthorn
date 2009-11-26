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

#+allegro (require :foreign)
#+allegro (require :osi)

;;;
;;; Package definitions
;;;

(in-package :cl-user)

(defpackage :blackthorn-graphics
  (:nicknames :blt-gfx)
  (:use :cl)
  (:import-from :sdl :set-caption)
  (:export

   :set-caption

   ;; graphics.lisp
   :window
   :image
   :unload-graphics
   :size
   :x
   :y
   :draw

   ))

(defpackage :blackthorn-physics
  (:nicknames :blt-phys)
  (:use :cl :iter :blt-gfx)
  (:export

   ;; component.lisp
   :component
   :offset
   :depth
   :size
   :parent
   :children
   :attach
   :detach
   :render
   :draw
   :sprite
   :image
   :mobile
   :veloc
   :accel

   ;; actor.lisp
   :event
   :event-type
   :event-mixin
   :bound-p
   :bind
   :unbind
   :event-subscription
   :subscribe
   :unsubscribe

   ;; input.lisp
   :key-event
   :event-key
   :event-mod
   :event-mod-key
   :event-unicode
   :bound-key-down-p
   :bind-key-down
   :unbind-key-down
   :bound-key-up-p
   :bind-key-up
   :unbind-key-up
   :actor

   ;; game.lisp
   :game
   :*game*
   :game-root
   :game-view
   :game-keys
   :init-game
   :load-game
   :save-game
   :update
   :send
   :event-update

   ))

(defpackage :blackthorn-user
  (:nicknames :blt-user)
  (:use :cl :blt-gfx :blt-phys)
  (:shadow :room)
  #+allegro (:import-from :cl-user :exit)
  (:export

   ;; main.lisp
   :*resource-directory-pathname*
   :*database-pathname*
   :*save-file-pathname*
   :main

   ))

#-allegro
(eval-when (:compile-toplevel :load-toplevel)
  (setf (symbol-function 'blt-user::exit) #'quit))