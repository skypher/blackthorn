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
;;; Key Events
;;;

(defclass key-event (event)
  ((host
    :reader event-host
    :initarg :host
    :initform nil)
   (key
    :reader event-key
    :initarg :key)
   (mod
    :reader event-mod
    :initarg :mod)
   (mod-key
    :reader event-mod-key
    :initarg :mod-key)
   (unicode
    :reader event-unicode
    :initarg :unicode)))

;;;
;;; Key Mixin
;;;

(defclass key-mixin (event-mixin)
  ((host
    :reader event-host
    :initarg :host
    :initform nil)
   (key-down-handlers
    :reader key-down-handlers
    :initform (make-hash-table))
   (key-up-handlers
    :reader key-up-handlers
    :initform (make-hash-table))))

(defmethod initialize-instance :after ((object key-mixin) &key)
  (unless (bound-p object :key-down)
    (bind object :key-down #'dispatch-key-down))
  (unless (bound-p object :key-up)
    (bind object :key-up #'dispatch-key-up)))

(defmethod bound-key-down-p ((object event-mixin) key)
  (multiple-value-bind (value exists) (gethash key (key-down-handlers object))
    (declare (ignore value))
    exists))

(defmethod bind-key-down ((object key-mixin) key thunk)
  (with-slots (key-down-handlers) object
    (setf (gethash key key-down-handlers) thunk)))

(defmethod unbind-key-down ((object event-mixin) key)
  (with-slots (key-down-handlers) object
    (remhash key key-down-handlers)))

(defmethod bound-key-up-p ((object event-mixin) key)
  (multiple-value-bind (value exists) (gethash key (key-up-handlers object))
    (declare (ignore value))
    exists))

(defmethod bind-key-up ((object key-mixin) key thunk)
  (with-slots (key-up-handlers) object
    (setf (gethash key key-up-handlers) thunk)))

(defmethod unbind-key-up ((object event-mixin) key)
  (with-slots (key-up-handlers) object
    (remhash key key-up-handlers)))

(defgeneric dispatch-key-down (object event))
(defmethod dispatch-key-down ((object key-mixin) (event key-event))
  (with-slots (host key-down-handlers) object
    (with-slots ((event-host host) (event-key key)) event
      (let ((handler (gethash event-key key-down-handlers)))
        (when (and (or (not host) (not event-host) (eql host event-host))
                   handler)
          (funcall handler object event))))))

(defgeneric dispatch-key-up (object event))
(defmethod dispatch-key-up ((object key-mixin) (event key-event))
  (with-slots (host key-up-handlers) object
    (with-slots ((event-host host) (event-key key)) event
      (let ((handler (gethash event-key key-up-handlers)))
        (when (and (or (not host) (not event-host) (eql host event-host))
                   handler)
          (funcall handler object event))))))
