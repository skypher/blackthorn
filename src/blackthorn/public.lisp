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

(in-package :blackthorn)

;;;
;;; Utilities
;;;

(declaim (inline x y))
(defun x (n)
  "@arg[n]{A complex number.}
   @return{The real part of n.}
   @short{Complex numbers are used to represent 2D coordinates in space.} The
     x and y coordinates are represented by the real and imaginary portions
     of the complex number, respectively.
   @see{y}"
  (realpart n))

(defun y (n)
  "@arg[n]{A complex number.}
   @return{The imaginary part of n.}
   @short{Complex numbers are used to represent 2D coordinates in space.} The
     x and y coordinates are represented by the real and imaginary portions
     of the complex number, respectively.
   @see{x}"
  (imagpart n))

;;;
;;; Games
;;;

(defgeneric game-root (game)
  (:documentation
   "@arg[game]{A @class{game}.}
    @return{A @class{component}.}
    @short{Returns the root component of the game simulation tree.}"))

(defgeneric game-view (game)
  (:documentation
   "@arg[game]{A @class{game}.}
    @return{A @class{component}.}
    @short{Returns the view component of the game simulation.}"))

(defgeneric game-view (game)
  (:documentation
   "@arg[game]{A @class{game}.}
    @return{A @class{sheet}.}
    @short{Returns the active sheet of the game.}"))

(defgeneric game-keys (game)
  (:documentation
   "@arg[game]{A @class{game}.}
    @return{A @class{key-event} @class{event-subscription}.}
    @short{Returns a subcription which can be used to subscribe to
      global key events.}"))

(defgeneric game-init (game)
  (:documentation
   "@arg[game]{A @class{game}.}
    @short{Used to initialize a new @class{game} prior to starting the main
      loop. Users are expected to define a primary method for this generic
      function.}"))

(defgeneric game-load (game))
(defgeneric game-save (game))

(defgeneric game-update (game)
  (:documentation
   "@arg[game]{A @class{game}.}
    @short{Called during the main game loop to update the game every frame.
      A default method is supplied}"))

;;;
;;; Graphics
;;;

(defgeneric name (object)
  (:documentation
   "@arg[object]{An object.}
    @return{A symbol.}
    @short{Returns the name of an object.} Used to uniquely indentify
      objects in a human readable form."))

(defgeneric size (object)
  (:documentation
   "@arg[object]{An object.}
    @return{A complex number.}
    @short{Returns the size of an object as a complex number.}
    @see{x} @see{y}"))

(defgeneric draw (object xy z)
  (:documentation
   "@arg[object]{An object.}
    @arg[xy]{A complex number.}
    @arg[z]{A real number.}
    @short{Draws the object at the given position in absolute coordinates.}
    @see{x} @see{y}"))

;;;
;;; Components
;;;

(defgeneric offset (object)
  (:documentation
   "@arg[object]{A @class{component}.}
    @return{A complex number.}
    @short{Returns the coordinate offset of an object relative to its
    @fun{parent}.}
    @see{x} @see{y}"))

(defgeneric depth (object)
  (:documentation
   "@arg[object]{A @class{component}.}
    @return{A real number.}
    @short{Returns the depth of an object.} Depth is assessed relative to
      siblings, according to parent. Negative depth is above the parent,
      positive depth is below the parent."))

(defgeneric parent (object)
  (:documentation
   "@arg[object]{A @class{component}.}
    @return{A @class{component}.}
    @short{Returns the parent of an object within the game simulation tree.}
    @see{attach} @see{detach}"))

(defgeneric children (object)
  (:documentation
   "@arg[object]{A @class{component}.}
    @return{A vector of @class{component}.}
    @short{Returns the children of an object within the game simulation tree.}
    @see{attach} @see{detach}"))

(defgeneric image (object)
  (:documentation
   "@arg[object]{A @class{blt-phys:sprite}.}
    @return{An @class{image}.}
    @short{Returns the image of an object.}"))

(defgeneric attach (parent child)
  (:documentation
   "@arg[parent]{A @class{component}.}
    @arg[child]{A @class{component}.}
    @short{Sets child's @fun{parent} to parent and adds child to
      parent's @fun{children}.}
    @see{detach}"))

(defgeneric detach (parent child)
  (:documentation
   "@arg[parent]{A @class{component}.}
    @arg[child]{A @class{component}.}
    @short{Sets child's @fun{parent} to nil and removes child from
      parent's @fun{children}.}
    @see{attach}"))

(defgeneric render (object xy zmin zmax)
  (:documentation
   "@arg[object]{A @class{component}.}
    @arg[xy]{A complex number.}
    @arg[zmin]{A real number.}
    @arg[zmax]{A real number.}
    @short{Draws an object at an absolute coordinate within the specified
      depth space.} Responsible for rendering all @fun{children} of an
      object."))

;;;
;;; Events
;;;

(defgeneric event-type (event)
  (:documentation
   "@arg[event]{An @class{event}.}
    @return{A symbol.}
    @short{Returns an event's type.}"))

(defgeneric bound-p (object event)
  (:documentation
   "@arg[object]{An @class{event-mixin}.}
    @arg[event]{A type of @class{event}.}
    @return{A generalized boolean.}
    @short{Returns whether an object has an event handler bound for a given
      event type.}
    @see{bind} @see{unbind}"))

(defgeneric bind (object event thunk)
  (:documentation
   "@arg[object]{An @class{event-mixin}.}
    @arg[event]{A type of @class{event}.}
    @arg[thunk]{A function of two arguments: object, an @class{event-mixin},
      and event, an @class{event}.}
    @short{Binds an event handler for the given event type.}
    @see{bound-p} @see{unbind}"))

(defgeneric unbind (object event)
  (:documentation
   "@arg[object]{An @class{event-mixin}.}
    @arg[event]{A type of @class{event}.}
    @short{Uninds the event handler for the given event type.}
    @see{bound-p} @see{bind}"))

(defgeneric subscribe (subscription subscriber)
  (:documentation
   "@arg[subscription]{An @class{event-subscription}.}
    @arg[subscriber]{An @class{event-mixin}.}
    @short{Adds an object to the list of subscribers to a subscription.}
    @see{unsubscribe}"))

(defgeneric unsubscribe (subscription subscriber)
  (:documentation
   "@arg[subscription]{An @class{event-subscription}.}
    @arg[subscriber]{An @class{event-mixin}.}
    @short{Removes an object from the list of subscribers to a subscription.}
    @see{subscribe}"))

;;;
;;; Keyboard Input
;;;

(defgeneric event-key (event)
  (:documentation
   "@arg[subscription]{A @class{key-event}.}
    @return{A symbol.}
    @short{Returns the key name associated with a key event.}
    @see{event-mod} @see{event-mod-key} @see{event-unicode}"))

(defgeneric event-mod (event)
  (:documentation
   "@arg[subscription]{A @class{key-event}.}
    @return{An integer.}
    @short{Returns the bitwise OR of all modifiers associated with a key event.}
    @see{event-key} @see{event-mod-key} @see{event-unicode}"))

(defgeneric event-mod-key (event)
  (:documentation
   "@arg[subscription]{A @class{key-event}.}
    @return{A list of symbols.}
    @short{Returns a list of modifiers associated with a key event.}
    @see{event-key} @see{event-mod} @see{event-unicode}"))

(defgeneric event-unicode (event)
  (:documentation
   "@arg[subscription]{A @class{key-event}.}
    @return{A character.}
    @short{Returns the character associated with a key event.}
    @see{event-key} @see{event-mod} @see{event-mod-key}"))

(defgeneric bound-key-down-p (object key)
  (:documentation
   "@arg[object]{A @class{key-mixin}.}
    @arg[event]{A type of @class{key-event}.}
    @return{A generalized boolean.}
    @short{Returns whether an object has a key-down event handler bound for a
      given key.}
    @see{bind-key-down} @see{unbind-key-down}"))

(defgeneric bind-key-down (object key thunk)
  (:documentation
   "@arg[object]{A @class{key-mixin}.}
    @arg[event]{A type of @class{key-event}.}
    @arg[thunk]{A function of two arguments: object, a @class{key-mixin},
      and event, a @class{key-event}.}
    @short{Binds a key-down event handler for the given key.}
    @see{bound-key-down-p} @see{unbind-key-down}"))

(defgeneric unbind-key-down (object key)
  (:documentation
   "@arg[object]{A @class{key-mixin}.}
    @arg[event]{A type of @class{key-event}.}
    @short{Uninds the key-down event handler for the given key.}
    @see{bound-key-down-p} @see{bind-key-down}"))

(defgeneric bound-key-up-p (object key)
  (:documentation
   "@arg[object]{A @class{key-mixin}.}
    @arg[event]{A type of @class{key-event}.}
    @return{A generalized boolean.}
    @short{Returns whether an object has a key-up event handler bound for a
      given key.}
    @see{bind-key-up} @see{unbind-key-up}"))

(defgeneric bind-key-up (object key thunk)
  (:documentation
   "@arg[object]{A @class{key-mixin}.}
    @arg[event]{A type of @class{key-event}.}
    @arg[thunk]{A function of two arguments: object, a @class{key-mixin},
      and event, a @class{key-event}.}
    @short{Binds a key-up event handler for the given key.}
    @see{bound-key-up-p} @see{unbind-key-up}"))

(defgeneric unbind-key-up (object key)
  (:documentation
   "@arg[object]{A @class{key-mixin}.}
    @arg[event]{A type of @class{key-event}.}
    @short{Uninds the key-up event handler for the given key.}
    @see{bound-key-up-p} @see{bind-key-up}"))

;;;
;;; Actors
;;;

(defgeneric veloc (object)
  (:documentation
   "@arg[object]{A @class{mobile}.}
    @return{A complex number.}
    @short{Returns the object's velocity.}
    @see{accel} @see{x} @see{y}"))

(defgeneric accel (object)
  (:documentation
   "@arg[object]{A @class{mobile}.}
    @return{A complex number.}
    @short{Returns the object's acceleration.}
    @see{veloc} @see{x} @see{y}"))

(defgeneric update (object event)
  (:documentation
   "@arg[object]{An @class{actor}.}
    @arg[object]{An update @class{event}.}
    @short{Event handler for the update event.} This event handler is defined
      automatically by the @class{actor} class. The default method does
      nothing."))
