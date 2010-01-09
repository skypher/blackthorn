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

(in-package :blackthorn-physics)

(defclass game (event-subscription)
  ((root
    :accessor game-root
    :initform nil)
   (view
    :accessor game-view
    :initform nil)
   (sheet
    :accessor game-sheet
    :initform nil)
   (keys
    :accessor game-keys
    :initform (make-instance 'event-subscription :types '(:key-down :key-up)))
   (event-queue
    :reader event-queue
    :initform (make-instance 'containers:basic-queue))))

(defmethod initialize-instance :after ((game game) &key)
  (subscribe game (game-keys game)))

(defvar *game*)

(defmethod game-init :after ((game game))
  (if (game-view game)
      (window (size (game-view game)))
      (warn "No view object for game ~a: Unable to initialize window.~%" game)))

(defmethod render ((game game) xy zmin zmax)
  (activate (game-sheet game))
  (with-slots (offset size) (game-view game)
    (gl:with-pushed-matrix
      (gl:ortho 0 (x size) (y size) 0 -1 1)
      (gl:with-primitive :quads
        (render (game-root game) (+ xy offset) zmin zmax)))))

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

(defmethod game-update ((game game))
  (labels ((event-update (actor xy)
             (declare (ignore xy))
             (send actor (make-instance 'event :type :update)))
           (event-collide (node other)
             (send node (make-instance 'collision-event :hit other)))
           (apply-dispatch-event (args) (apply #'dispatch-event args)))
    (walk-tree (game-root game) #'event-update)
    (find-collisions (game-root game) #'event-collide)
    (containers:iterate-elements (event-queue game) #'apply-dispatch-event)
    (containers:empty! (event-queue game))))
