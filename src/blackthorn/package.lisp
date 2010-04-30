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

(in-package :cl-user)

;;;
;;; Internal Packages:
;;; Anything exported here but not exported in the public interface is
;;; intended for internal use.
;;;

(defpackage :blackthorn-net
  (:nicknames :blt-net)
  (:use :cl :iter)
  (:export

   ;; network.lisp
   :hostname
   :hostnames
   :net-init
   :net-exit
   :net-game-connect
   :net-game-start
   :net-game-update
   :net-game-quit

   ))

(defpackage :blackthorn-graphics
  (:nicknames :blt-gfx)
  (:use :cl :iter)
  (:import-from :sdl :set-caption)
  (:export

   :set-caption

   ;; graphics.lisp
   :window
   :sheet
   :activate
   :image
   :unload-graphics
   :size
   :x
   :y
   :draw
   :anim
   :next-image
   :load-sheet
   :make-image
   :make-anim
   :make-anim-or-image

   ))

(defpackage :blackthorn-mixer
  (:nicknames :blt-mixer)
  (:use :cl)
  (:export

   ;; music.lisp
   :init-mixer
   :unload-mixer
   :sample
   :play
   :stop
   
   ))

(defpackage :blackthorn-physics
  (:nicknames :blt-phys)
  (:use :cl :iter :blt-gfx)
  (:export

   ;; utils.lisp
   :unit
   :dot
   :proj
   :norm
   :dist
   :theta
   :polar
   :rot
   :cross
   :once-only
   :with-gensyms

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
   :update
   :sprite
   :image

   ;; event.lisp
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
   :key-mixin
   :event-host
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

   ;; actor.lisp
   :actor
   :mobile
   :veloc
   :accel

   ;; collision.lisp
   :collidable
   :reactive-collisions-only-p
   :collide
   :find-nearest-object
   :collision-event
   :event-hit

   ;; game.lisp
   :game
   :*game*
   :game-root
   :game-view
   :game-sheet
   :game-keys
   :game-init
   :game-load
   :game-save
   :game-update
   :send

   ))

;;;
;;; Public Interface:
;;; The generic functions and classes listed form the interface to Blackthorn.
;;;

(defpackage :blackthorn
  (:nicknames :blt)
  (:use :cl :blt-gfx :blt-mixer :blt-phys)
  (:export

   ;; utils.lisp
   :unit
   :dot
   :proj
   :norm
   :dist
   :theta
   :polar
   :rot
   :cross
   :once-only
   :with-gensyms

   ;; graphics.lisp
   :set-caption
   :window
   :sheet
   :activate
   :image
   :size
   :x
   :y
   :draw
   :anim
   :next-image
   :load-sheet
   :make-image
   :make-anim
   :make-anim-or-image

   ;; music.lisp
   :init-mixer
   :unload-mixer
   :sample
   :play
   :stop

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
   :update
   :sprite
   :image

   ;; event.lisp
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
   :key-mixin
   :event-host
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

   ;; actor.lisp
   :actor
   :mobile
   :veloc
   :accel

   ;; collision.lisp
   :collidable
   :reactive-collisions-only-p
   :collide
   :find-nearest-object
   :collision-event
   :event-hit

   ;; game.lisp
   :game
   :*game*
   :game-root
   :game-view
   :game-sheet
   :game-keys
   :game-init
   :game-load
   :game-save
   :game-update
   :send

   ))

;;;
;;; User Package:
;;;

(defpackage :blackthorn-user
  (:nicknames :blt-user)
  (:use :cl :blt :blt-net :iter)
  (:shadow :room)
  #+allegro (:import-from :cl-user :exit)
  (:import-from :blt-gfx :unload-graphics)
  (:export

   ;; main.lisp
   :*resource-pathname-defaults*
   :resource
   :main

   ))

#-allegro
(eval-when (:compile-toplevel :load-toplevel)
  (setf (symbol-function 'blt-user::exit) #'quit))
