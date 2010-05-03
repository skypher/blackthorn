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

(in-package :bunnyslayer)

(defclass bunnyslayer-game (game) ())

(defclass flag-mixin (actor)
  ((flags
    :reader flags
    :initform (make-hash-table))))

(defmethod get-flag ((object flag-mixin) flag)
  (with-slots (flags) object
    (gethash flag flags)))

(defun set-flag (flag value)
  #'(lambda (object event)
      (with-slots (flags) object (setf (gethash flag flags) value))))

(defclass direction-mixin (flag-mixin)
  ((speed
    :accessor speed
    :initarg :speed)))

(defvar *dir-names* '(:north :south :west :east))
(defvar *dir-vectors* '(#c(0 -1) #c(0 1) #c(-1 0) #c(1 0)))

(defmethod change-veloc ((object direction-mixin) event)
  (with-slots (veloc speed) object
    (setf veloc
          (* speed
             (unit (iter (for d in *dir-names*) (for v in *dir-vectors*)
                         (when (get-flag object d) (sum v))))))))

(defclass facing-mixin (direction-mixin)
  ((facing
    :reader facing
    :initarg :facing)))

(defmethod change-facing ((object facing-mixin) event)
  (with-slots (facing) object
    (setf facing
          (if (get-flag object facing)
              facing
              (or (iter (for d in *dir-names*)
                        (when (get-flag object d) (return d)))
                  facing)))))

(defclass action-mixin (actor)
  ((action-state
    :reader action
    :initarg :action)
   action-inputs
   action-states
   action-state-table))

(defclass action-event (event)
  ((old-action
    :reader old-action
    :initarg :old-action)
   (new-action
    :reader new-action
    :initarg :new-action)
   (input
    :reader input
    :initarg :input)))

(defmacro with-action-state-table ((object) inputs &body table)
  (assert (listp inputs))
  (assert (iter (for row in table) (always (listp row))))
  (let ((states
         (iter (for row in table) (collect (car row))))
        (transitions
         (iter (with state-set = (make-hash-table))
               (for (state . result-states) in table)
               (setf (gethash state state-set)
                     (iter (with result-set = (make-hash-table))
                           (for input in inputs)
                           (for result in result-states)
                           (setf (gethash input result-set) result)
                           (finally (return result-set))))
               (finally (return state-set)))))
    (with-gensyms (a-i a-s a-s-t)
      `(with-slots ((,a-i action-inputs)
                    (,a-s action-states)
                    (,a-s-t action-state-table)) ,object
         (setf ,a-i ',inputs
               ,a-s ',states
               ,a-s-t ',transitions)))))

(defmethod dispatch-action-input ((object action-mixin) input)
  (with-slots (action action-inputs action-states action-state-table) object
    (assert (member action action-states))
    (assert (member input action-inputs))
    (let* ((old-action action)
           (new-action (gethash input (gethash action action-state-table)))
           (action-event (make-instance 'action-event
                                        :old-action old-action
                                        :new-action new-action
                                        :input input)))
      (assert (member new-action action-states))
      (setf action new-action)
      (action-transition object action-event))))

(defmethod action-transition ((object action-mixin) event)
  (declare (ignore object event)))

(defun action-input (input)
  #'(lambda (object event) (dispatch-action-input object input)))

(defclass hero (sprite mobile facing-mixin action-mixin)
  ((facing
    :initform :south)
   (action
    :initform :none)
   (speed
    :initform 3)))

(defun doall (&rest handlers)
  #'(lambda (object event)
      (iter (for handler in handlers) (funcall handler object event))))

;; TODO: generalize this to apply to any class with facings and/or actions
(defmethod change-image ((hero hero) event)
  (with-slots (image facing action) hero
    (let ((action-name (if (eql action :none)
                           (if (zerop (iter (for d in *dir-names*)
                                            (for v in *dir-vectors*)
                                            (when (get-flag hero d) (sum v))))
                               :stand
                               :walk)
                           action)))
      (setf image (make-anim-or-image "HERO-~a-~a" facing action-name)))))

(defmethod initialize-instance :after ((hero hero) &key)
  (iter (for (d k) in '((:north :sdl-key-up) (:south :sdl-key-down)
                        (:west :sdl-key-left) (:east :sdl-key-right)))
        (bind-key-down hero k (doall (set-flag d t) #'change-veloc
                                     #'change-facing #'change-image))
        (bind-key-up hero k (doall (set-flag d nil) #'change-veloc
                                   #'change-facing #'change-image)))
  (with-action-state-table (hero)
            (:attack :finish)
    (:none   :attack :none)
    (:attack :attack :none))
  (change-image hero nil))

(defmethod initialize-instance :after ((game bunnyslayer-game) &key)
  (setf (game-root game) (make-instance 'component :size #c(800 600))
        (game-view game) (make-instance 'component :size #c(800 600))))

(defmethod game-init ((game bunnyslayer-game) &key &allow-other-keys)
  (let* ((root (game-root game))
         (size (size (game-root game))))
    (setf (game-sheet game)
          (load-sheet (directory (resource "disp/refmap/*.png")) :name :sheet))
    (let ((hero (make-instance 'hero :parent root :offset (/ size 2))))
      (subscribe (game-keys game) hero))))

(defmethod game-update :after ((game bunnyslayer-game))
  ;; report the frame reate
  (let ((s (format nil "fps: ~,2f" (sdl:average-fps))))
    (set-caption s s)))

;; For interactive use:
(defun bunnyslayer ()
  (let ((*game* (make-instance 'bunnyslayer-game)))
    (main :exit-when-done nil)))

;; For non-interactive use:
(defvar *game* (make-instance 'bunnyslayer-game))