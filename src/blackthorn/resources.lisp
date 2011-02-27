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

(in-package :blackthorn-utils)

;;;
;;; Resource search paths
;;;

(defvar *resource-search-paths* nil
  "List of paths to be searched for resources like image and sound files.")

(defun directory-of (pathname)
  (make-pathname
   :host (pathname-host pathname)
   :directory (pathname-directory pathname)))

(defun add-resource-path (pathname)
  (aif (and pathname (fad:file-exists-p (directory-of pathname)))
       (pushnew it *resource-search-paths* :test #'equal)))

(defun resource (pathname)
  (aif (iter (for dir in *resource-search-paths*)
             (thereis (fad:file-exists-p (merge-pathnames pathname dir))))
       it
       (error "Unable to find resource ~s in search path ~s."
              pathname
              *resource-search-paths*)))

(defun resource-wild (pathname)
  (iter (for dir in *resource-search-paths*)
        (appending (directory (merge-pathnames pathname dir)))))

(defun resolve-resource (pathname &key (allow-wild nil))
  (if (and allow-wild (wild-pathname-p pathname))
      (resource-wild pathname)
      (resource pathname)))
