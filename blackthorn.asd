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

(defpackage :blackthorn-asd
  (:use :cl :asdf))

(in-package :blackthorn-asd)

(defsystem blackthorn
  :name "blackthorn"
  :author "Elliott Slaughter <elliottslaughter@gmail.com>"
  :version "0.2"
  :components ((:module src
                        :components
                        ((:module blackthorn
                                  :components
                                  ((:file "package")
                                   (:file "public")
                                   (:file "utils")
                                   (:file "graphics")
                                   (:file "fonts")
                                   (:file "music")
                                   (:file "component")
                                   (:file "event")
                                   (:file "input")
                                   (:file "actor")
                                   (:file "collision")
                                   (:file "game")
                                   (:file "network")
                                   (:file "library")
                                   (:file "main"))
                                  :serial t))))
  :depends-on (;; Utilities
               :trivial-features
               :command-line-arguments
               :cl-fad
               :iterate
               :cl-containers
               :mt19937

               ;; Networking and Serialization
               :usocket
               :cl-store

               ;; Graphics and Sound:
               :lispbuilder-sdl
               :lispbuilder-sdl-image
               :lispbuilder-sdl-mixer
               :cl-opengl))
