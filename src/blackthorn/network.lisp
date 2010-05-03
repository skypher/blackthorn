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
(defvar *host*)
(defvar *hosts*)
(defvar *local-port*)
(defvar *remote-host*)
(defvar *remote-port*)
(defvar *server-socket*)
(defvar *socket*)
(defvar *sockets*)
(defvar *players*)

(defun hostname ()
  *host*)

(defun hostnames ()
  *hosts*)

(defun default-abort-handler ()
  (error "Failed to connect.~%"))

(defun net-init (mode host port players)
  (assert (not (boundp '*mode*)))
  (ecase mode
    ((:server)
     (let ((socket (usocket:socket-listen usocket:*wildcard-host* port
                                          :element-type '(unsigned-byte 8))))
       (setf *server-socket* socket
             *local-port*
             ;; FIXME: This seems to blow up in CLISP.
             #-clisp (usocket:get-local-port socket)
             #+clisp port)))
    ((:client)
     (when (eql mode :client)
       (setf *remote-host* host *remote-port* port)))
    ((:normal)))
  (setf *mode* mode *players* (if players (max players 2) 2)))

(defun net-connect (&optional (abort-handler #'default-abort-handler))
  (ecase *mode*
    ((:server)
     (setf *sockets* (iter (repeat (1- *players*))
                           (collect (usocket:socket-accept *server-socket*))))
     (iter (for socket in *sockets*)
           (let ((request (net-receive socket)))
             (unless (equal (assoc :request request) '(:request :init))
               (funcall abort-handler))
             (net-send '((:response :init)) socket))))
    ((:client)
     (setf *socket* (usocket:socket-connect *remote-host* *remote-port*
                                             :element-type '(unsigned-byte 8)))
     (net-send `((:request :init)) *socket*)
     (unless (equal (net-receive *socket* :timeout 10) '((:response :init)))
       (funcall abort-handler)))
    ((:normal))))

(defun net-exit ()
  (assert (boundp '*mode*))
  (ecase *mode*
    ((:client)
     (usocket:socket-close *socket*))
    ((:server)
     (iter (for socket in *sockets*) (usocket:socket-close socket))
     (usocket:socket-close *server-socket*))
    ((:normal)))
  (makunbound '*socket*)
  (makunbound '*mode*))

(defun net-receive (socket &key timeout)
  (assert (or (eql *mode* :server) (eql *mode* :client)))
  (cl-store:restore (usocket:socket-stream socket)))

(defun net-send (message socket)
  (assert (or (eql *mode* :server) (eql *mode* :client)))
  (cl-store:store message (usocket:socket-stream socket))
  (force-output (usocket:socket-stream socket)))

(defmacro with-serve-request ((request socket &key timeout) &body body)
  (let ((s (gensym)))
    `(let* ((,s ,socket)
            (,request (net-receive ,s :timeout ,timeout)))
       (net-send (progn ,@body) ,s))))

(defmacro with-serve-requests ((requests sockets &key timeout) &body body)
  (let ((ss (gensym)) (s (gensym)) (reply (gensym)))
    `(let* ((,ss ,sockets)
            (,requests (iter (for ,s in ,ss)
                             (collect (net-receive ,s :timeout ,timeout))))
            (,reply (progn ,@body)))
       (iter (for ,s in ,ss)
             (net-send ,reply ,s)))))

(defun net-send-request (request socket &key timeout)
  (net-send request socket)
  (net-receive socket :timeout timeout))
 
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
     (let ((hosts (iter (for i from 0 below *players*) (collect i))))
       (setf *host* 0 *hosts* hosts)
       (iter (for socket in *sockets*) (for i from 1)
             (with-serve-request (request socket :timeout nil)
               (if (equal (assoc :request request) '(:request :connect))
                   `((:response :connect)
                     (:host ,i) (:hosts ,hosts)
                     (:random-state
                      ,(mt19937::random-state-state mt19937:*random-state*)))
                   (funcall abort-handler)))))
     (format t "Connected.~%"))
    ((:client)
     (handler-case
         (let ((response (net-send-request '((:request :connect)) *socket*)))
           (unless (equal (assoc :response response) '(:response :connect))
             (error "Client didn't understand response.~%"))
           (setf *host* (cadr (assoc :host response))
                 *hosts* (cadr (assoc :hosts response))
                 mt19937:*random-state*
                 (mt19937::make-random-object
                  :state (cadr (assoc :random-state response)))))
       (usocket:connection-refused-error ()
         (funcall abort-handler)))
     (format t "Connected.~%"))
    ((:normal) (setf *host* 0 *hosts* '(0)))))

(defun net-game-start (&optional (abort-handler #'default-abort-handler))
  (ecase *mode*
    ((:server)
     (format t "Server waiting for client to start.~%")
     (iter (for socket in *sockets*)
           (with-serve-request (request socket :timeout nil)
             (if (equal (assoc :request request) '(:request :start))
                 `((:response :start))
                 (error "Server didn't understand request.~%"))))
     (format t "Starting.~%"))
    ((:client)
     (format t "Attempting to start...~%")
     (handler-case
         (let ((response (net-send-request '((:request :start)) *socket*)))
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
     (with-serve-requests (requests *sockets* :timeout nil)
       (let ((events
              (nconc
               (containers:collect-elements input-queue)
               (iter (for request in requests)
                     (cond ((equal (assoc :request request) '(:request :update))
                            (nconcing (cadr (assoc :events request))))
                           ((equal (assoc :request request) '(:request :quit))
                            (funcall abort-handler))
                           (t
                            (error "Server didn't understand request.~%")))))))
         (iter (for e in events) (funcall process-event e))
         (containers:empty! input-queue)
         `((:response :update) (:events ,events)))))
    ((:client)
     (let ((response
            (net-send-request
             `((:request :update)
               (:events ,(containers:collect-elements input-queue)))
             *socket*)))
       (cond ((equal (assoc :response response) '(:response :update))
              (let ((events (cadr (assoc :events response))))
                (iter (for e in events) (funcall process-event e))
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
     (iter (for socket in *sockets*)
           (net-send `((:response :quit)) socket))
     (format t "Disconnected.~%"))
    ((:client)
     (format t "Attempting to disconnect...~%")
     (net-send `((:request :quit)) *socket*)
     (format t "Disconnected.~%"))
    ((:normal))))
