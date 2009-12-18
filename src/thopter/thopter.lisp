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

(in-package :thopter)

(defclass alarm (actor)
  ((timer
    :accessor timer
    :initarg :timer
    :initform nil)))

(defgeneric alarm (object event))

(defmethod initialize-instance :after ((alarm alarm) &key)
  (bind alarm :alarm #'alarm))

(defmethod update :before ((alarm alarm) event)
  (with-slots (timer) alarm
    (when timer
      (decf timer)
      (when (< timer 0)
        (setf timer nil)
        (send alarm (make-instance 'event :type :alarm))))))

(defclass thopter-game (game)
  ((wave
    :accessor game-wave)))

(defclass wave-controller (alarm)
  ((level
    :accessor level
    :initform 0)))

(defclass thopter (sprite mobile collidable)
  ((health
    :accessor health
    :initarg :health
    :initform 0)))
(defclass bullet (sprite mobile collidable) ())
(defclass enemy (sprite mobile collidable alarm)
  ((timer :initform 10)
   (health
    :accessor health
    :initarg :health
    :initform 0)))
(defclass enemy-bullet (sprite mobile collidable) ())
(defclass explosion (sprite mobile collidable alarm) ())
(defclass health-pack (sprite mobile collidable) ())

(defmethod initialize-instance :after ((thopter thopter) &key)
  (bind-key-down thopter :sdl-key-up    #'move-north)
  (bind-key-up   thopter :sdl-key-up    #'stop-north)
  (bind-key-down thopter :sdl-key-down  #'move-south)
  (bind-key-up   thopter :sdl-key-down  #'stop-south)
  (bind-key-down thopter :sdl-key-left  #'move-west)
  (bind-key-up   thopter :sdl-key-left  #'stop-west)
  (bind-key-down thopter :sdl-key-right #'move-east)
  (bind-key-up   thopter :sdl-key-right #'stop-east)
  (bind-key-down thopter :sdl-key-space #'shoot))

(defmethod move-north ((thopter thopter) event)
  (incf (veloc thopter) #c(0 -4)))

(defmethod stop-north ((thopter thopter) event)
  (decf (veloc thopter) #c(0 -4)))

(defmethod move-south ((thopter thopter) event)
  (incf (veloc thopter) #c(0 4)))

(defmethod stop-south ((thopter thopter) event)
  (decf (veloc thopter) #c(0 4)))

(defmethod move-west ((thopter thopter) event)
  (incf (veloc thopter) #c(-4 0)))

(defmethod stop-west ((thopter thopter) event)
  (decf (veloc thopter) #c(-4 0)))

(defmethod move-east ((thopter thopter) event)
  (incf (veloc thopter) #c(4 0)))

(defmethod stop-east ((thopter thopter) event)
  (decf (veloc thopter) #c(4 0)))

(defmethod collide ((thopter thopter) event)
  (with-slots (parent offset depth veloc health) thopter
    (typecase (event-hit event)
      (enemy-bullet (decf health))
      (enemy        (decf health 4))
      (explosion    (decf health))
      (health-pack  (incf health 2)))
    (when (and parent (<= health 0))
      (make-instance 'explosion :parent parent
                     :offset offset :depth depth :veloc veloc
                     :image (make-instance 'image :name :explosion)
                     :timer 10)
      (detach parent thopter))))

(defmethod shoot ((thopter thopter) event)
  (with-slots (parent offset size veloc) thopter
    (make-instance 'bullet :parent parent 
                   :offset (+ offset (/ (x size) 2) #c(0 -4)) :depth -1
                   :veloc (+ veloc #c(0 -8))
                   :image (make-instance 'image :name :bullet))
    (make-instance 'bullet :parent parent 
                   :offset (+ offset (/ (x size) 2) #c(0 -4)) :depth -1
                   :veloc (+ veloc (rot #c(0 -8) (* pi 0.15d0)))
                   :image (make-instance 'image :name :bullet))
    (make-instance 'bullet :parent parent 
                   :offset (+ offset (/ (x size) 2) #c(0 -4)) :depth -1
                   :veloc (+ veloc (rot #c(0 -8) (* pi -0.15d0)))
                   :image (make-instance 'image :name :bullet))))

(defmethod update ((bullet bullet) event)
  ;; TODO: doesn't work for nested objects. Need absolute-offset.
  (with-slots (parent offset size) bullet
    (with-slots ((view-offset offset) (view-size size)) (game-view *game*)
      (let ((x1 (x offset)) (y1 (y offset))
            (x2 (x view-offset)) (y2 (y view-offset))
            (w1 (x size)) (h1 (y size))
            (w2 (x view-size)) (h2 (y view-size)))
        (when (or (<= (+ x1 w1) x2)
                  (<= (+ x2 w2) x1)
                  (<= (+ y1 h1) y2)
                  (<= (+ y2 h2) y1))
          (detach parent bullet))))))

(defmethod collide ((bullet bullet) event)
  (when (and (parent bullet) (typep (event-hit event) 'enemy))
    (detach (parent bullet) bullet)))

(defmethod update ((bullet enemy-bullet) event)
  ;; TODO: doesn't work for nested objects. Need absolute-offset.
  (with-slots (parent offset size) bullet
    (with-slots ((view-offset offset) (view-size size)) (game-view *game*)
      (let ((x1 (x offset)) (y1 (y offset))
            (x2 (x view-offset)) (y2 (y view-offset))
            (w1 (x size)) (h1 (y size))
            (w2 (x view-size)) (h2 (y view-size)))
        (when (or (<= (+ x1 w1) x2)
                  (<= (+ x2 w2) x1)
                  (<= (+ y1 h1) y2)
                  (<= (+ y2 h2) y1))
          (detach parent bullet))))))

(defmethod collide ((bullet enemy-bullet) event)
  (when (and (parent bullet) (typep (event-hit event) 'thopter))
    (detach (parent bullet) bullet)))

(defmethod update ((enemy enemy) event)
  (with-slots (parent (xy offset) (v veloc) accel) enemy
    (labels
        ((circular (r)
           (+ ;; circular motion
            (* (unit (- r)) (min 4 (/ (dot v v) (abs r))))
            ;; correction for radius
            (* (unit (- r)) (- (abs r) 250) 0.01d0)
            ;; correction for parallel veloc
            (* (- (proj v r)) 0.1d0)
            ;; correction for tangential veloc
            (* (unit (norm v r)) (- 2 (abs (norm v r))) 0.1d0)))
         (away (xy2 v2)
           (/ (rot v2 (* pi 0.25d0 (signum (cross v2 (- xy xy2))))) 30d0))
         (toward (xy2 v2)
           (unit (- xy2 xy))))
      (let ((nearest (iter (for x in-vector (children parent))
                           (when (or (typep x 'bullet) (typep x 'thopter))
                             (finding x minimizing (dist xy (offset x))))))
            (r (- xy (/ (size (game-root *game*)) 2))))
        (setf accel
              (if nearest
                  (with-slots ((xy2 offset) (v2 veloc)) nearest
                    (cond ((and (typep nearest 'bullet) (< (dist xy xy2) 120))
                           (away xy2 v2))
                          ((and (typep nearest 'thopter) (< (dist xy xy2) 180))
                           (toward xy2 v2))
                          (t (circular r))))
                  (circular r)))))))

(defmethod alarm ((enemy enemy) event)
  (with-slots (parent offset size veloc timer) enemy
    (setf timer (+ 2 (random 25)))
    (make-instance 'enemy-bullet :parent parent 
                   :offset (+ offset (complex (/ (x size) 2) (y size)) #c(0 4))
                   :depth -1
                   :veloc (+ veloc #c(0 8))
                   :image (make-instance 'image :name :enemy-bullet))))

(defmethod collide ((enemy enemy) event)
  (with-slots (parent offset depth veloc health) enemy
    (typecase (event-hit event)
      (bullet      (decf health))
      (thopter     (decf health 4))
      (explosion   (decf health))
      (health-pack (incf health 2)))
    (when (and parent (<= health 0))
      (make-instance 'explosion :parent parent
                     :offset offset :depth depth :veloc veloc
                     :image (make-instance 'image :name :explosion)
                     :timer 10)
      (detach parent enemy))))

(defmethod alarm ((explosion explosion) event)
  (with-slots (parent offset depth veloc) explosion
    (make-instance 'health-pack :parent parent
                   :offset (+ offset #c(8 8)) :depth depth :veloc (/ veloc 2)
                   :image (make-instance 'image :name :health))
    (detach parent explosion)))

(defmethod update ((health health-pack) event)
  ;; TODO: doesn't work for nested objects. Need absolute-offset.
  (with-slots (parent offset size) health
    (with-slots ((view-offset offset) (view-size size)) (game-view *game*)
      (let ((x1 (x offset)) (y1 (y offset))
            (x2 (x view-offset)) (y2 (y view-offset))
            (w1 (x size)) (h1 (y size))
            (w2 (x view-size)) (h2 (y view-size)))
        (when (or (<= (+ x1 w1) x2)
                  (<= (+ x2 w2) x1)
                  (<= (+ y1 h1) y2)
                  (<= (+ y2 h2) y1))
          (detach parent health))))))

(defmethod collide ((health health-pack) event)
  (when (and (parent health)
             (or (typep (event-hit event) 'enemy)
                 (typep (event-hit event) 'thopter)))
    (detach (parent health) health)))

(defun spawn-wave (n)
  (let* ((root (game-root *game*)) (size (size root)) (center (/ size 2)))
    (loop for i from (* (1+ (floor n -2)) 128) to (* (floor n 2) 128) by 128
       do (let* ((xy (complex (+ (/ (x size) 2) i) (* (y size) -0.05d0)))
                 (v (* 2 (rot (unit (- center xy)) (/ pi 2)))))
            (make-instance 'enemy :parent root :offset xy :veloc v :depth 1
                           :image (make-instance 'image :name :enemy)
                           :health 4 :timer 20)))))

(defmethod game-init ((game thopter-game))
  (let* ((size #c(800 600))
         (root (make-instance 'component :size size)))
    (setf (game-root game) root
          (game-view game) (make-instance 'component :size size)
          (game-sheet game)
          (make-instance 'sheet :source (resource "disp/thopter.png"))
          (game-wave game) (make-instance 'wave-controller :parent root))
    (let ((thopter (make-instance
                    'thopter :parent root
                    :offset (complex (/ (x size) 2) (* (y size) 3/4))
                    :image (make-instance 'anim :name :thopter)
                    :health 4)))
      (subscribe (game-keys game) thopter))
    (spawn-wave (+ 2 (level (game-wave game))))))

(defmethod game-update :after ((game thopter-game))
  (let ((s (format nil "wave: ~a, health: ~a, fps: ~,2f"
                   (level (game-wave game))
                   (iter (for i in-vector (children (game-root game)))
                         (when (typep i 'thopter) (return (health i))))
                   (sdl:average-fps))))
    (set-caption s s))

  (when (and (zerop (iter (for i in-vector (children (game-root game)))
                          (when (typep i 'enemy) (count i))))
             (not (timer (game-wave game))))
    (setf (timer (game-wave game)) 30)))

(defmethod alarm ((wave wave-controller) event)
  (with-slots (level) wave
    (incf level)
    (spawn-wave (+ 2 level))))

;; For interactive use:
(defun thopter ()
  (let ((*game* (make-instance 'thopter-game)))
    (main :exit-when-done nil)))

;; For non-interactive use:
(defvar *game* (make-instance 'thopter-game))
