;;;; Blackthorn -- Lisp Game Engine
;;;;
;;;; Copyright (c) 2007-2011, Elliott Slaughter <elliottslaughter@gmail.com>
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

(in-package :blackthorn-physics)

(defclass game (event-subscription)
  ((game-screen
    :reader game-screen
    :initform nil)
   (game-screen-next
    :accessor game-screen-next
    :initform nil)
   (game-root
    :accessor game-root
    :initform nil)
   (game-view
    :accessor game-view
    :initform nil)
   (game-sheet
    :accessor game-sheet
    :initform nil)
   (game-keys
    :accessor game-keys
    :initform (make-instance 'event-subscription :types '(:key-down :key-up)))
   (event-queue
    :reader event-queue
    :initform (make-instance 'containers:basic-queue))))

(defmacro delegate (class &rest accessors)
  (with-gensyms (game screen value)
    `(progn
       ,@(iter (for accessor in accessors)
               (collect
                `(defmethod ,accessor ((,game ,class))
                   (let ((,screen (game-screen ,game)))
                     (if ,screen
                         (,accessor ,screen)
                         (slot-value ,game ',accessor)))))
               (collect
                `(defmethod (setf ,accessor) (,value (,game ,class))
                   (let ((,screen (game-screen ,game)))
                     (if ,screen
                         (setf (,accessor ,screen) ,value)
                         (setf (slot-value ,game ',accessor) ,value)))))))))

(delegate game game-root game-view game-sheet)

(defun apply-screen-next (game)
  (with-slots (game-screen game-screen-next) game
    (when game-screen (unsubscribe game game-screen))
    (setf game-screen game-screen-next)
    (when game-screen (subscribe game game-screen))))

(defmethod initialize-instance :after ((game game) &key)
  (subscribe game (game-keys game)))

(defvar *game*)

(defmethod game-init :before ((game game) &key &allow-other-keys)
  (apply-screen-next game)
  (if (game-view game)
      (window (size (game-view game)))
      (warn "No view for game ~a: Unable to initialize window.~%" game)))

(defmethod render ((game game) xy zmin zmax)
  (apply-screen-next game)
  (activate (game-sheet game))
  (with-slots (offset size) (game-view game)
    (gl:with-pushed-matrix
      (gl:ortho 0 (x size) (y size) 0 -1 1)
      (gl:with-primitive :quads
        (render (game-root game) (+ xy offset) zmin zmax)))))

;;;
;;; Screens
;;;

(defclass screen (event-subscription)
  ((game
    :reader game
    :initarg :game)
   (game-root
    :accessor game-root
    :initform nil)
   (game-view
    :accessor game-view
    :initform nil)
   (game-sheet
    :accessor game-sheet
    :initform nil)
   (game-keys
    :accessor game-keys
    :initform (make-instance 'event-subscription :types '(:key-down :key-up)))))

(defmethod initialize-instance :after ((screen screen) &key)
  (subscribe screen (game-keys screen)))

(defmethod activate ((screen screen))
  (setf (game-screen-next (game screen)) screen))

;;;
;;; Game Event Queue
;;;

(defgeneric send-event (game target event))

(defmethod send-event ((game game) target (event event))
  (declare (ignore game target event)))

(defmethod send-event ((game game) (target event-mixin) (event event))
  (containers:enqueue (event-queue game) (list target event)))

(defun send (target event)
  "@arg[target]{An @class{event-mixin}.}
   @arg[event]{An @class{event}.}
   @short{Schedules an event for dispatch at a future time.} The event will be
     delivered to the target object during the update portion of the game loop."
  (send-event *game* target event))

(defun game-update-events (game-root event-queue)
  (labels ((event-update (actor xy)
             (declare (ignore xy))
             (send actor (make-instance 'event :type :update)))
           (event-collide (node other)
             (send node (make-instance 'collision-event :hit other)))
           (apply-dispatch-event (args) (apply #'dispatch-event args)))
    (collision-update game-root)
    (walk-tree game-root #'event-update)
    (containers:iterate-elements event-queue #'apply-dispatch-event)
    (containers:empty! event-queue)
    (collision-update game-root)
    (collision-search game-root #'event-collide)
    (containers:iterate-elements event-queue #'apply-dispatch-event)
    (containers:empty! event-queue)))

(defmethod game-update ((game game))
  (apply-screen-next game)
  (let ((screen (game-screen game)))
    (if screen
        (game-update screen)
        (game-update-events (game-root game) (event-queue game)))))

(defmethod game-update ((screen screen))
  (game-update-events (game-root screen) (event-queue (game screen))))

;;;
;;; Game Quit Event
;;;

(defclass quit-event (event)
  ((type :initform :quit)
   (quit
    :accessor event-quit
    :initform t)))

(defmethod dispatch-event :after ((game game) (event quit-event))
  (when (event-quit event)
    (sdl:push-quit-event)))

(defun quit ()
  (send *game* (make-instance 'quit-event)))
