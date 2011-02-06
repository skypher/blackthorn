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

(defpackage :blackthorn-build
  (:nicknames :blt-build)
  (:use :cl)
  (:import-from :cl-user :*driver-system*)
  #+allegro (:import-from :excl :exit :generate-application :run-shell-command)
  #+sbcl (:import-from :sb-ext :save-lisp-and-die)
  #+clisp (:import-from :ext :quit :saveinitmem)
  #+clozure (:import-from :ccl :quit :save-application))

(in-package :blt-build)

(defvar *driver-system* :blackthorn)

;;;
;;; Compile the system and associated driver.
;;;

#+quicklisp
(ql:quickload *driver-system*)

#-quicklisp
(require :asdf)
#-quicklisp
(asdf:oos 'asdf:load-op *driver-system*)

#+allegro (asdf:oos 'asdf:load-op :com.gigamonkeys.asdf-extensions)
#+allegro (com.gigamonkeys.asdf-extensions:build-one-fasl *driver-system*)

#+allegro (defvar *driver-fasl*
            (make-pathname :name (symbol-name *driver-system*) :type "fasl"))

;;;
;;; Get trivial-features for convienience.
;;;

(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:oos 'asdf:load-op :trivial-features))

;;;
;;; Some utilities.
;;;

(defun cwd ()
  (truename (make-pathname)))

(defun append-directory (default-pathname &rest directories)
  (merge-pathnames
   (make-pathname :directory (cons :relative directories))
   default-pathname))

;;;
;;; Setup directories for build.
;;;

(defconstant +working-dir+ (cwd))

(defconstant +build-dir+ (append-directory +working-dir+ "bin"))

;;;
;;; Ensure the build directory is empty.
;;;

(ensure-directories-exist +build-dir+)

;;;
;;; Specify executable location.
;;;

(defconstant +build-name+ "main")

(defconstant +build-exe+
  (make-pathname :directory (pathname-directory +build-dir+)
		 :name +build-name+
                 #+windows :type #+windows "exe"))

;;;
;;; Make main executable, Allegro.
;;;

#+allegro (defvar *debug* nil)
#+allegro (defvar *console-app* nil)

#+allegro
(generate-application
 +build-name+
 +build-dir+
 (apply #'list :foreign :osi :sock "allegro-fixes.cl" *driver-fasl*
        (when *debug* '(:inspect :trace)))
 :allow-existing-directory t
 #+windows :icon-file
 #+windows
 (make-pathname :directory '(:relative "windows") :name "thopter" :type "ico")
 :restart-init-function 'blt-user:main
 #-windows
 :application-administration
 #-windows ;; Quiet startup (See below for Windows version of this.)
 '(:resource-command-line "-Q")
 :read-init-files nil			; don't read ACL init files
 :print-startup-message nil		; don't print ACL startup messages
 :ignore-command-line-arguments t	; ignore ACL (not app) cmd line options
 :suppress-allegro-cl-banner t

 ;; Change the following to `t', if:
 ;; - the program (vs. data) is large
 ;; - you'll have lots of users of the app (so sharing the code is important)
 :purify nil

 ;; don't give autoload warning, but you should still be aware that
 ;; autoloads.out will contain a list of autoloadable names.
 :autoload-warning nil

 :include-debugger *debug*
 :include-tpl *debug*
 :include-ide nil
 :include-devel-env nil
 :include-compiler nil
 :discard-arglists (not *debug*)
 :discard-local-name-info (not *debug*)
 :discard-source-file-info (not *debug*)
 :discard-xref-info (not *debug*)
 
 ;; for debugging:
 :verbose nil
 :build-input "build.in"
 :build-output "build.out"
 
 :runtime :standard
 )

#+(and allegro mswindows) ;; Quiet startup:
(when (not *console-app*)
  (run-shell-command
   ;; Replace +cm with +cn to see the window, but have it not be in the
   ;; foreground.
   (format nil "\"~a\" -o \"~a\" +B +M +cm -Q"
	   (translate-logical-pathname "sys:bin;setcmd.exe")
	   +build-exe+)
   :show-window :hide))

#+(and allegro mswindows)
(when *console-app*
  (delete-file +build-exe+)
  (sys:copy-file "sys:buildi.exe" +build-exe+))

#+allegro (exit)

;;;
;;; Make main executable, SBCL.
;;;

#+sbcl (save-lisp-and-die +build-exe+ :toplevel #'blt-user:main :executable t)

;;;
;;; Make main executable, CLISP.
;;;

#+clisp
(saveinitmem +build-exe+ :init-function #'blt-user:main :executable t :norc t)
#+clisp (quit)

;;;
;;; Make main executable, CCL.
;;;

#+clozure
(save-application +build-exe+ :toplevel-function #'blt-user:main
                  :prepend-kernel t)
#+clozure (quit)
