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

(in-package :blt-user)

;;;
;;; Runtime dependencies which must be loaded into the lisp executable prior
;;; to calling any SDL functionality. Normally this is done when loading the
;;; system definiton, but must be done manually for distributed executables.
;;;

; from http://lispbuilder.svn.sourceforge.net/viewvc/lispbuilder/trunk/lispbuilder-sdl/cffi/library.lisp
(defun load-sdl-dlls ()
  "Loads dlls needed to run SDL."
  #+darwin
  (let ((frameworks
         (merge-pathnames
          (make-pathname :directory '(:relative :up "Frameworks")))))
    (if (fad:directory-exists-p frameworks)
        (pushnew frameworks cffi:*darwin-framework-directories*)))
  #+darwin
  (cffi:define-foreign-library cocoahelper
    (:darwin (:framework "cocoahelper")))
  #+darwin
  (cffi:use-foreign-library cocoahelper)
  (cffi:define-foreign-library sdl
    (:darwin (:framework "SDL"))
    (:windows "SDL.dll")
    (:unix (:or "libSDL-1.2.so.0.7.2"
                "libSDL-1.2.so.0"
                "libSDL-1.2.so"
                "libSDL.so"
                "libSDL")))
  (cffi:use-foreign-library sdl)
  #+darwin (lispbuilder-sdl-cocoahelper::cocoahelper-init))

; from http://lispbuilder.svn.sourceforge.net/viewvc/lispbuilder/trunk/lispbuilder-sdl-image/cffi/library.lisp
(defun load-sdl-image-dlls ()
  "Loads dlls needed to run SDL_image."
  (cffi:define-foreign-library sdl-image
    (:darwin (:framework "SDL_image"))
    (:windows (:or "SDL_image.dll" "SDL_image1.2.dll"))
    (:unix (:or "libSDL_image-1.2.so.0"
                "libSDL_image1.2"
                "libSDL_image.so")))
  (cffi:use-foreign-library sdl-image))

(defun load-dlls ()
  "Loads dlls needed to run SDL, SDL_image, and SDL_gfx."
  (load-sdl-dlls)
  (load-sdl-image-dlls))