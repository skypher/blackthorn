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

(in-package :blackthorn-user)

;;;
;;; System paths
;;;

(defvar *resource-pathname-defaults*
  "The root directory which contains resources for the game. Normally, this
   is equivalent to *default-pathname-defaults*, but in some situations,
   e.g. Mac OS X applications, this refers to a different location.")

(defun setup-paths ()
  #-darwin
  (setf *resource-pathname-defaults* (truename *default-pathname-defaults*))
  #+darwin
  (let* ((root (truename *default-pathname-defaults*))
         (exe (truename (directory-namestring (command-line-executable))))
         (resources (merge-pathnames #p"../Resources/" exe)))
    (if (fad:directory-exists-p resources)
        (setf *default-pathname-defaults* exe
              *resource-pathname-defaults* resources)
        (setf *resource-pathname-defaults* root))))

(defun resource (pathname)
  (merge-pathnames pathname *resource-pathname-defaults*))

;;;
;;; Command-line option parsing
;;;

(defun command-line-executable ()
  "Returns the path to the executable being run."
  #+allegro (car (sys:command-line-arguments))
  #+clisp (aref (ext:argv) 0)
  #+clozure (car ccl:*command-line-argument-list*)
  #+ecl (car (ext:command-args))
  #+sbcl (car sb-ext:*posix-argv*)
  #-(or allegro clisp clozure ecl sbcl)
  (error "Don't know how to get command line args."))

(defun command-line-arguments ()
  "Returns the command-line arguments given to the executable, except for the
   name of the executable itself."
  #+allegro (cdr (sys:command-line-arguments))
  #+clisp ext:*args*
  #+ecl (cdr (ext:command-args))
  #+clozure (cdr ccl:*command-line-argument-list*)
  #+sbcl (cdr sb-ext:*posix-argv*)
  #-(or allegro clisp clozure ecl sbcl)
  (error "Don't know how to get command line args."))

(defun cli-options ()
  (list (make-instance 'cli-parser:cli-option
                       :abbr "s"
                       :full "server"
                       :requires-arguments :optional)
        (make-instance 'cli-parser:cli-option
                       :abbr "c"
                       :full "connect"
                       :requires-arguments t)
        (make-instance 'cli-parser:cli-option
                       :abbr "P"
                       :full "port"
                       :requires-arguments t)))

(defun cli-get-mode (options)
  (let ((port (car (gethash "port" options))))
    (or
     (multiple-value-bind (value exists) (gethash "server" options)
       (when exists
         (list :server (car value) (when port (parse-integer port)))))
     (multiple-value-bind (value exists) (gethash "connect" options)
       (when exists
         (list :client (car value) (when port (parse-integer port)))))
     (list :normal nil nil))))

;;;
;;; Video modes
;;;

(defun vector->complex (v)
  (complex (x v) (y v)))

(defun video-dimensions ()
  "Converts SDL:VIDEO-DIMENSIONS to a canonical (i.e. complex) form."
  (vector->complex (sdl:video-dimensions)))

(defun list-modes (flags)
  (let ((modes (sdl:list-modes flags)))
    (or (eql modes t)
        (mapcar #'vector->complex modes))))

(defun largest-video-dimensions (flags)
  (let ((modes (sdl:list-modes flags)))
    (if (eql modes t)
        (video-dimensions)
        (vector->complex
         (find (apply #'max (mapcar #'x modes)) modes :key #'x)))))

(defun smallest-video-dimensions (flags)
  (let ((modes (sdl:list-modes flags)))
    (if (eql modes t)
        (video-dimensions)
        (vector->complex
         (find (apply #'min (mapcar #'x modes)) modes :key #'x)))))

;;;
;;; Network - Sockets and Serialization
;;;

(defvar *mode*)
(defvar *host*)
(defvar *port*)
(defvar *server-socket*)

(defun net-init (mode host port)
  (assert (not (boundp '*mode*)))
  (ecase mode
    ((:server)
     (let* ((host (or host usocket:*wildcard-host*))
            (port (or port usocket:*auto-port*))
            (socket (usocket:socket-listen
                     host port :element-type '(unsigned-byte 8))))
       (setf *mode* :server
             *host* host *port* (usocket:get-local-port socket)
             *server-socket* socket)))
    (:client
     (setf *mode* :client *host* host *port* port))
    (:normal
     (setf *mode* :normal))))

(defun net-exit ()
  (assert (boundp '*mode*))
  (when (eql *mode* :server)
    (usocket:socket-close *server-socket*))
  (makunbound '*mode*))

(defmacro with-serve-request ((request &key (timeout 0) max) &body body)
  (let ((i (gensym)) (m (gensym)) (time (gensym)))
    ` (let ((,m ,max) (,time ,timeout))
        (assert (eql *mode* :server))
        (loop for ,i from 0 while (or (not ,m) (< ,i ,m))
           while
           #+(not (and sbcl windows))
           (usocket:wait-for-input
            *server-socket* :timeout ,time :ready-only t)
           #+(and sbcl windows) t
           do (usocket:with-connected-socket
                  (connection (usocket:socket-accept *server-socket*))
                (let ((,request
                       (cl-store:restore (usocket:socket-stream connection))))
                  (cl-store:store
                   (progn ,@body)
                   (usocket:socket-stream connection))))))))

(defun net-send (message)
  (assert (eql *mode* :client))
  (usocket:with-client-socket
      (socket stream *host* *port* :element-type '(unsigned-byte 8))
    (cl-store:store message stream)))

(defun net-send-request (message)
  (assert (eql *mode* :client))
  (usocket:with-client-socket
      (socket stream *host* *port* :element-type '(unsigned-byte 8))
    (cl-store:store message stream)
    (force-output stream)
    (cl-store:restore stream)))

;;;
;;; Network - Game Protocol
;;;

(defun net-game-connect ()
  (ecase *mode*
    ((:server)
     (format t "Waiting for a connection on port ~a. Please start client.~%"
             *port*)
     (with-serve-request (request :timeout nil :max 1)
       (if (equal (assoc :request request) '(:request :connect))
           `((:response :connect) (:random-state ,mt19937:*random-state*))
           (error "Server didn't understand request.~%")))
     (format t "Connected.~%"))
    ((:client)
     (format t "Attempting to connect...~%")
     (handler-case
         (let ((response (net-send-request '((:request :connect)))))
           (unless (equal (assoc :response response) '(:response :connect))
             (error "Client didn't understand response.~%"))
           (setf mt19937:*random-state* (cadr (assoc :random-state response))))
       (usocket:connection-refused-error ()
         (format t "Unable to connect to server. Quitting.~%")
         (throw 'main-init nil)))
     (format t "Connected.~%"))
    ((:normal))))

(defun net-game-start ()
  (ecase *mode*
    ((:server)
     (format t "Server waiting for client to start.~%")
     (with-serve-request (request :timeout nil :max 1)
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
         (format t "Disconnected from server. Quitting.~%")
         (throw 'main-loop nil)))
     (format t "Starting.~%"))
    ((:normal))))

(defun net-game-update (input-queue)
  (ecase *mode*
    ((:server)
     (with-serve-request (request :timeout nil :max 1)
       (cond ((equal (assoc :request request) '(:request :update))
              (let ((server-events
                     (containers:collect-elements input-queue))
                    (client-events (cadr (assoc :events request))))
                (iter (for e in server-events) (send *game* e))
                (iter (for e in client-events) (send *game* e))
                (containers:empty! input-queue)
                `((:response :update) (:events ,server-events))))
             ((equal (assoc :request request) '(:request :quit))
              (format t "Disconnected from client. Quitting.~%")
              (throw 'main-loop nil))
             (t
              (error "Server didn't understand request.~%")))))
    ((:client)
     (let* ((client-events (containers:collect-elements input-queue))
            (response
             (net-send-request
              `((:request :update) (:events ,client-events)))))
       (cond ((equal (assoc :response response) '(:response :update))
              (let ((server-events (cadr (assoc :events response))))
                (iter (for e in server-events) (send *game* e))
                (iter (for e in client-events) (send *game* e))
                (containers:empty! input-queue)))
             ((equal (assoc :response response) '(:response :quit))
              (format t "Disconnected from server. Quitting.~%")
              (throw 'main-loop nil))
             (t
              (error "Client didn't understand response.~%")))))
    ((:normal)
     (let ((events (containers:collect-elements input-queue)))
       (iter (for e in events) (send *game* e))
       (containers:empty! input-queue)))))

(defun net-game-quit ()
  (ecase *mode*
    ((:server)
     (format t "Server disconnecting from client.~%")
     (with-serve-request (request :timeout nil :max 1)
       `((:response :quit)))
     (format t "Disconnected.~%"))
    ((:client)
     (format t "Attempting to disconnect...~%")
     (net-send `((:request :quit)))
     (format t "Disconnected.~%"))
    ((:normal))))

;;;
;;; Main Game Driver
;;;

(defun main (&key (exit-when-done t))
  "Main entry point for the game. Deals with initialization, finalization, and the main game loop."
  ;; Initialization:
  (setup-paths)
  (load-dlls)

  (unless *game* (error "No game specified.~%"))

  (let ((options (cli-parser:cli-parse (command-line-arguments) (cli-options))))
    (apply #'net-init (cli-get-mode options)))

  (setf mt19937:*random-state* (mt19937:make-random-state t))

  (catch 'main-init
    (net-game-connect)

    (sdl:with-init ()
      (init-mixer)
      (game-init *game*)

      (gl:enable :texture-2d)
      (gl:enable :blend)
      (gl:blend-func :src-alpha :one-minus-src-alpha)
      (gl:clear-color 0 0 0 0)
      (gl:enable :depth-test)
      (gl:depth-func :lequal)
      (gl:matrix-mode :modelview)
      (gl:load-identity)

      ;; Main loop:
      (let ((input-queue (make-instance 'containers:basic-queue)))
        (catch 'main-loop
          (net-game-start)

          (sdl:with-events ()
            (:quit-event () (net-game-quit) t)
            (:key-down-event (:key k :mod m :mod-key m-k :unicode u)
              (containers:enqueue
               input-queue
               (make-instance 'key-event :host *mode* :type :key-down :key k
                              :mod m :mod-key m-k :unicode u)))
            (:key-up-event (:key k :mod m :mod-key m-k :unicode u)
              (containers:enqueue
               input-queue
               (make-instance 'key-event :host *mode* :type :key-up :key k
                              :mod m :mod-key m-k :unicode u)))
            (:idle ()
              (gl:clear :color-buffer-bit :depth-buffer-bit)
              (render *game* #c(0 0) 1d0 -1d0)
              (gl:flush)
              (sdl:update-display)

              #+blt-debug
              (let ((connection (or swank::*emacs-connection*
                                    (swank::default-connection))))
                (when (and connection
                           (not (eql swank:*communication-style* :spawn)))
                  (swank::handle-requests connection t)))

              (net-game-update input-queue)
              (game-update *game*))))))
    #-clozure ;; FIXME: This causes a crash on Clozure builds on Windows.
    (unload-graphics)
    (unload-mixer))

  ;; Finalization:
  (net-exit)
  (when exit-when-done
    (exit)))
