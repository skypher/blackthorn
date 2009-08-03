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

(in-package :blackthorn-physics)

;;;
;;; Events
;;;

(defclass event ()
  ((type
    :reader event-type
    :initarg :type)))

(defmethod event-type-p ((event event) type)
  (eql (event-type event) type))

;;;
;;; Event Handlers
;;;

(defclass event-handler ()
  ((name
    :reader name
    :initarg :name)
   (test
    :reader event-test
    :initarg :test)
   (body
    :reader event-body
    :initarg :body)))

(defmethod handle-event-p ((event-handler event-handler) (event event))
  (funcall (event-test event-handler) event))

(defmethod handle-event ((event-handler event-handler) (event event))
  (funcall (event-body event-handler) event))

;;;
;;; Event Mixin
;;;

(defclass event-mixin ()
  ((event-queue
    :reader event-queue
    :initform (make-instance 'containers:basic-queue))
   (event-handlers
    :accessor event-handlers
    :initform nil)))

(defmacro define-event-handlers ((event) object &body clauses)
  (labels ((expand-clause (name test &rest body)
             `(make-instance 'event-handler
               :name ',name
               :test (lambda (,event) ,test)
               :body (lambda (,event) ,@body))))
    (once-only (object)
      `(setf (event-handlers ,object)
             (append (event-handlers ,object)
                     (list ,@(mapcar #'(lambda (clause)
                                         (apply #'expand-clause clause))
                                     clauses)))))))

(defmethod push-event ((object event-mixin) (event event))
  (containers:enqueue (event-queue object) event))

(defmethod dispatch-event ((object event-mixin) (event event))
  (let ((handler (find-if #'(lambda (handler) (handle-event-p handler event))
                          (event-handlers object))))
    (when handler
      (handle-event handler event))))

(defmethod dispatch-queued-events ((object event-mixin))
  (containers:iterate-elements
   (event-queue object)
   #'(lambda (event) (dispatch-event object event)))
  (containers:empty! (event-queue object)))

;;;
;;; Event Subscriptions
;;;

(defclass event-subscription (event-mixin)
  ((event-types
    :accessor event-types
    :initarg :types
    :initform t)
   (subscribers
    :accessor event-subscribers
    :initform nil)))

(defmethod initialize-instance :after ((subscription event-subscription) &key)
  (define-event-handlers (event) subscription
    (dispatch-event (with-slots (event-types) subscription
                      (or (eql event-types t)
                          (member (event-type event) event-types)))
      (loop for subscriber in (event-subscribers subscription)
         do (push-event subscriber event)))))

(defmethod subscribe-event
    ((subscription event-subscription) (subscriber event-mixin))
  (pushnew subscriber (event-subscribers subscription)))

(defmethod unsubscribe-event
    ((subscription event-subscription) (subscriber event-mixin))
  (setf (event-subscribers subscription)
        (delete subscriber (event-subscribers subscription))))

(defmethod push-event ((subscription event-subscription) (event event))
  (dispatch-event subscription event))

;;;
;;; Actors
;;;

(defclass actor (component event-mixin)
  ())

(defmethod event-update :before ((actor actor))
  (dispatch-queued-events actor))
