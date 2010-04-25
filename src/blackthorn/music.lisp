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

(in-package :blackthorn-mixer)

;;;
;;; Sprite Sheets
;;;

(defvar *samples* (make-hash-table))

(defclass sample ()
  ((name
    :reader name
    :initarg :name)
   (source
    :initarg :source)
   (type
    :initarg :type
    :initform :sample)
   raw-sample))

(defun init-mixer ()
  (sdl-mixer:open-audio))

(defun unload-mixer ()
  (sdl-mixer:close-audio)
  (clrhash *samples*))

(defmethod make-instance ((class (eql (find-class 'sample))) &key name source)
  (or (when name (gethash name *samples*))
      (when (not source) (error "No such sample named ~a." name))
      (let ((sample (call-next-method)))
        (setf (gethash (name sample) *samples*) sample))))

(defmethod make-instance ((class (eql 'sample)) &rest initargs)
  (apply #'make-instance (find-class 'sample) initargs))

(defun load-sample (sample)
  (with-slots (source type) sample
    (assert (probe-file source) (source) "Source file \"~a\" not found." source)
    (ecase type
      ((:music)
       (sdl-mixer:load-music source))
      ((:sample)
       (sdl-mixer:load-sample source)))))

(defmethod play ((sample sample) &key loop fade)
  (with-slots (type raw-sample) sample
    (unless (slot-boundp sample 'raw-sample)
      (setf raw-sample (load-sample sample)))
    (ecase type
      ((:music)
       (sdl-mixer:play-music raw-sample :loop loop :fade fade))
      ((:sample)
       (sdl-mixer:play-sample raw-sample :loop loop :fade fade)))))

(defmethod stop (&key channel)
  (if channel
      (sdl-mixer:halt-sample channel)
      (sdl-mixer:halt-music)))
