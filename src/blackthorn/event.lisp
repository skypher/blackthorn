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

(in-package :blackthorn-physics)

;;;
;;; Events
;;;

(defclass event ()
  ((type
    :reader event-type
    :initarg :type)))

;;;
;;; Event Mixin
;;;

(defclass event-mixin ()
  ((handlers
    :accessor event-handlers
    :initform (make-hash-table))))

(defmethod bound-p ((object event-mixin) event)
  (multiple-value-bind (value exists) (gethash event (event-handlers object))
    (declare (ignore value))
    exists))

(defmethod bind ((object event-mixin) event thunk)
  (with-slots (handlers) object
    (setf (gethash event handlers) thunk)))

(defmethod unbind ((object event-mixin) event)
  (with-slots (handlers) object
    (remhash event handlers)))

(defgeneric dispatch-event (object event))
(defmethod dispatch-event ((object event-mixin) (event event))
  (with-slots (handlers) object
    (let ((handler (gethash (event-type event) handlers)))
      (when handler
        (funcall handler object event)))))

;;;
;;; Event Subscriptions
;;;

(defclass event-subscription (event-mixin)
  ((subscribers
    :accessor event-subscribers
    :initform nil)))

(defmethod initialize-instance :after
    ((subscription event-subscription) &key types)
  (iter (for type in types)
        (unless (bound-p subscription type)
          (bind subscription type #'dispatch-event))))

(defmethod subscribe
    ((subscription event-subscription) (subscriber event-mixin))
  (pushnew subscriber (event-subscribers subscription)))

(defmethod unsubscribe
    ((subscription event-subscription) (subscriber event-mixin))
  (setf (event-subscribers subscription)
        (delete subscriber (event-subscribers subscription))))

(defmethod dispatch-event ((subscription event-subscription) (event event))
  (iter (for subscriber in (event-subscribers subscription))
        (dispatch-event subscriber event)))
