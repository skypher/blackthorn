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

;;;
;;; Actors
;;;

(defclass actor (component)
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
      `(progn
        ,@(mapcar #'(lambda (clause)
                      `(pushnew ,(apply #'expand-clause clause)
                        (event-handlers ,object)
                        :key #'name))
                  clauses)))))

(defmethod push-event ((actor actor) (event event))
  (containers:enqueue (event-queue actor) event))

(defmethod dispatch-event ((actor actor) (event event))
  (let ((handler
         (find-if #'(lambda (handler) (funcall (event-test handler) event))
                  (event-handlers actor))))
    (when handler
      (funcall (event-body handler) event))))

(defmethod event-update :before ((actor actor))
  (containers:iterate-elements
   (event-queue actor)
   #'(lambda (event) (dispatch-event actor event)))
  (containers:empty! (event-queue actor)))

;;;
;;; Event Subscriptions
;;;

(defclass event-subscription ()
  ((subscribers
    :accessor event-subscribers
    :initform nil)))

(defmethod subscribe-event ((subscription event-subscription) (actor actor))
  (pushnew actor (event-subscribers subscription)))

(defmethod unsubscribe-event ((subscription event-subscription) (actor actor))
  (setf (event-subscribers subscription)
        (delete actor (event-subscribers subscription))))

(defmethod push-event ((subscription event-subscription) (event event))
  (loop for subscriber in (event-subscribers subscription)
        do (push-event subscriber event)))