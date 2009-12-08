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

(in-package :blackthorn)

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

(defgeneric game-keys (game)
  (:documentation
   "@arg[game]{A @class{game}.}
    @return{A @class{key-event} @class{subcription}.}
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
    depth space.} Responsible for rendering all @fun{children} of an object."))

;;;
;;; Events
;;;

(defgeneric bound-p (object event))
(defgeneric bind (object event thunk))
(defgeneric unbind (object event))
(defgeneric dispatch-event (object event))
(defgeneric subscribe (subscription subscriber))
(defgeneric unsubscribe (subscription subscriber))

;;;
;;; Keyboard Input
;;;

(defgeneric event-key (object))
(defgeneric event-mod (object))
(defgeneric event-mod-key (object))
(defgeneric event-unicode (object))
(defgeneric bound-key-down-p (object key))
(defgeneric bind-key-down (object key thunk))
(defgeneric unbind-key-down (object key))
(defgeneric bound-key-up-p (object key))
(defgeneric bind-key-up (object key thunk))
(defgeneric unbind-key-up (object key))
(defgeneric dispatch-key-down (object event))
(defgeneric dispatch-key-up (object event))

;;;
;;; Actors
;;;

(defgeneric veloc (object))
(defgeneric accel (object))
(defgeneric update (object event))
