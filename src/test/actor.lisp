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

(in-package :blackthorn-test)

(in-suite blackthorn-physics)

(test event
  (let ((event (make-instance 'event :type :my-type)))
    (is (event-type-p event :my-type))
    (is (not (event-type-p event :some-other-type)))))

(test (event-handler :depends-on (and event))
  (let* ((event1 (make-instance 'event :type :my-type1))
         (event2 (make-instance 'event :type :my-type2))
         (handler (make-instance 'event-handler
                                 :name 'my-handler
                                 :test (lambda (e) (event-type-p e :my-type1))
                                 :body (lambda (e) (is (eql e event1))))))
    (is (handle-event-p handler event1))
    (is (not (handle-event-p handler event2)))
    (handle-event handler event1)))

(test (event-mixin :depends-on (and event event-handler))
  (let ((event1 (make-instance 'event :type :my-type1))
        (event2 (make-instance 'event :type :my-type2))
        (object (make-instance 'event-mixin)))
    (define-event-handlers (event) object
      (handle-event1 (event-type-p event :my-type1)
        (is (eql event event1) "Must match expected event1"))
      (handle-event2 (event-type-p event :my-type2)
        (is (eql event event2) "Must match expected event2"))
      (otherwise t (fail "Unable to match event")))
    (is (listp (event-handlers object))
        "Event handlers was not a list")
    (is (= (length (event-handlers object)) 3)
        "Number of handlers not equal to 3")
    (is (eql (name (first (event-handlers object))) 'handle-event1)
        "First event handlet not equal to handle-event1")
    (is (eql (name (second (event-handlers object))) 'handle-event2)
        "Second event handler not equal to handle-event2")
    (is (eql (name (third (event-handlers object))) 'otherwise)
        "Third event handler not equal to otherwise")
    (dispatch-event object event1)
    (dispatch-event object event2)
    (is (containers:empty-p (event-queue object)) "Queue must be empty")
    (push-event object event1)
    (push-event object event2)
    (dispatch-queued-events object)
    (is (containers:empty-p (event-queue object)) "Queue must be empty")))
