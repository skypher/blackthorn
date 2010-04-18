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

(in-package :blackthorn-net)

;;;
;;; Network - Sockets and Serialization
;;;

(defconstant +max-buffer-size+ 65536)

(defvar *mode*)
(defvar *local-port*)
(defvar *remote-host*)
(defvar *remote-port*)
(defvar *server-socket*)
(defvar *socket*)
(defvar *buffer*
  (make-array +max-buffer-size+ :element-type '(unsigned-byte 8)))

(defun default-abort-handler ()
  (error "Failed to connect.~%"))

(defun net-init (mode host port)
  (assert (not (boundp '*mode*)))
  (ecase mode
    ((:server)
     (let ((socket (usocket:socket-listen usocket:*wildcard-host* port
                                          :element-type '(unsigned-byte 8))))
       (setf *server-socket* socket
             *local-port* (usocket:get-local-port socket))))
    ((:client)
     (when (eql mode :client)
       (setf *remote-host* host *remote-port* port)))
    ((:normal)))
  (setf *mode* mode))

(defun net-connect (&optional (abort-handler #'default-abort-handler))
  (ecase *mode*
    ((:server)
     (usocket:wait-for-input *server-socket* :timeout nil :ready-only t)
     (setf *socket* (usocket:socket-accept *server-socket*))
     (let ((request (net-receive)))
       (unless (equal (assoc :request request) '(:request :init))
         (funcall abort-handler))
       (net-send '((:response :init)))))
    ((:client)
     (setf *socket* (usocket:socket-connect *remote-host* *remote-port*
                                            :element-type '(unsigned-byte 8)))
     (net-send `((:request :init)))
     (unless (equal (net-receive :timeout 10) '((:response :init)))
       (funcall abort-handler)))
    ((:normal))))

(defun net-exit ()
  (assert (boundp '*mode*))
  (ecase *mode*
    ((:server :client)
     (usocket:socket-close *socket*)
     (when (eql *mode* :server)
       (usocket:socket-close *server-socket*)))
    ((:normal)))
  (makunbound '*socket*)
  (makunbound '*mode*))

(defun net-receive (&key timeout)
  (assert (or (eql *mode* :server) (eql *mode* :client)))
  (cl-store:restore (usocket:socket-stream *socket*)))

(defun net-send (message)
  (assert (or (eql *mode* :server) (eql *mode* :client)))
  (cl-store:store message (usocket:socket-stream *socket*))
  (force-output (usocket:socket-stream *socket*)))

(defmacro with-serve-request ((request &key timeout) &body body)
  `(let ((,request (net-receive :timeout ,timeout)))
     (net-send (progn ,@body))))

(defun net-send-request (request &key timeout)
  (net-send request)
  (net-receive :timeout timeout))
 
;;;
;;; Network - Game Protocol
;;;

(defun net-game-connect (&optional (abort-handler #'default-abort-handler))
  (when (eql *mode* :server)
    (format t "Waiting for a connection on port ~a. Please start client.~%"
            *local-port*))
  (net-connect abort-handler)
  (ecase *mode*
    ((:server)
     (with-serve-request (request :timeout nil)
       (if (equal (assoc :request request) '(:request :connect))
           `((:response :connect) (:random-state ,mt19937:*random-state*))
           (funcall abort-handler)))
     (format t "Connected.~%"))
    ((:client)
     (handler-case
         (let ((response (net-send-request '((:request :connect)))))
           (unless (equal (assoc :response response) '(:response :connect))
             (error "Client didn't understand response.~%"))
           (setf mt19937:*random-state* (cadr (assoc :random-state response))))
       (usocket:connection-refused-error ()
         (funcall abort-handler)))
     (format t "Connected.~%"))
    ((:normal))))

(defun net-game-start (&optional (abort-handler #'default-abort-handler))
  (ecase *mode*
    ((:server)
     (format t "Server waiting for client to start.~%")
     (with-serve-request (request :timeout nil)
       (if (equal (assoc :request request) '(:request :start))
           `((:response :start))
           (error "Server didn't understand request.~%")))
     (format t "Starting.~%"))
    ((:client)
     (format t "Attempting to start...~%")
     (handler-case
         (let ((response (net-send-request '((:request :start)))))
           (unless (equal (assoc :response response) '(:response :start))
             (error "Client didn't understand response.~%")))
       (usocket:connection-refused-error ()
         (funcall abort-handler)))
     (format t "Starting.~%"))
    ((:normal))))

(defun net-game-update (input-queue process-event
                        &optional (abort-handler #'default-abort-handler))
  (ecase *mode*
    ((:server)
     (with-serve-request (request :timeout nil)
       (cond ((equal (assoc :request request) '(:request :update))
              (let ((server-events
                     (containers:collect-elements input-queue))
                    (client-events (cadr (assoc :events request))))
                (iter (for e in server-events) (funcall process-event e))
                (iter (for e in client-events) (funcall process-event e))
                (containers:empty! input-queue)
                `((:response :update) (:events ,server-events))))
             ((equal (assoc :request request) '(:request :quit))
              (funcall abort-handler))
             (t
              (error "Server didn't understand request.~%")))))
    ((:client)
     (let* ((client-events (containers:collect-elements input-queue))
            (response
             (net-send-request
              `((:request :update) (:events ,client-events)))))
       (cond ((equal (assoc :response response) '(:response :update))
              (let ((server-events (cadr (assoc :events response))))
                (iter (for e in server-events) (funcall process-event e))
                (iter (for e in client-events) (funcall process-event e))
                (containers:empty! input-queue)))
             ((equal (assoc :response response) '(:response :quit))
              (funcall abort-handler))
             (t
              (error "Client didn't understand response.~%")))))
    ((:normal)
     (let ((events (containers:collect-elements input-queue)))
       (iter (for e in events) (funcall process-event e))
       (containers:empty! input-queue)))))

(defun net-game-quit (&optional (abort-handler #'default-abort-handler))
  (ecase *mode*
    ((:server)
     (format t "Server disconnecting from client.~%")
     (with-serve-request (request :timeout nil)
       `((:response :quit)))
     (format t "Disconnected.~%"))
    ((:client)
     (format t "Attempting to disconnect...~%")
     (net-send `((:request :quit)))
     (format t "Disconnected.~%"))
    ((:normal))))
