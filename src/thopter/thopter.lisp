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

;;;
;;; Utils - move these elsewhere at some point
;;;

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

(defclass transient (actor) ())

(defmethod update :after ((transient transient) event)
  ;; TODO: doesn't work for nested objects. Need absolute-offset.
  (with-slots (parent offset size) transient
    (with-slots ((view-offset offset) (view-size size)) (game-view *game*)
      (let ((x1 (x offset)) (y1 (y offset))
            (x2 (x view-offset)) (y2 (y view-offset))
            (w1 (x size)) (h1 (y size))
            (w2 (x view-size)) (h2 (y view-size)))
        (when (or (<= (+ x1 w1) x2)
                  (<= (+ x2 w2) x1)
                  (<= (+ y1 h1) y2)
                  (<= (+ y2 h2) y1))
          (detach parent transient))))))

;;;
;;; Thopter-specific implementation
;;;

(defclass thopter-game (game)
  ((wave
    :accessor game-wave)))

(defclass wave-controller (alarm)
  ((level
    :accessor level
    :initform 0)))

(defclass shooter (actor)
  ((bullet-class
    :initarg :bullet-class)
   (bullet-image
    :initarg :bullet-image)
   (bullet-veloc
    :initarg :bullet-veloc)
   (bullet-direction
    :initarg :bullet-direction)
   (firepower
    :accessor firepower
    :initarg :firepower
    :initform 1)))

(defclass thopter (sprite mobile collidable shooter)
  ((bullet-class :initform 'bullet)
   (bullet-image :initform (make-instance 'image :name :bullet))
   (bullet-veloc :initform #c(0 -8))
   (bullet-direction :initform :north)
   (health
    :accessor health
    :initarg :health
    :initform 0)))
(defclass bullet (sprite mobile collidable transient) ())
(defclass enemy (sprite mobile collidable alarm shooter)
  ((timer :initform 10)
   (bullet-class :initform 'enemy-bullet)
   (bullet-image :initform (make-instance 'image :name :enemy-bullet))
   (bullet-veloc :initform #c(0 8))
   (bullet-direction :initform :south)
   (health
    :accessor health
    :initarg :health
    :initform 0)))
(defclass enemy-bullet (sprite mobile collidable transient) ())
(defclass explosion (sprite mobile collidable alarm)
  ((drop-class
    :initarg :drop-class
    :initform nil)
   (drop-image
    :initarg :drop-image
    :initform nil)))
(defclass health-pack (sprite mobile collidable transient) ())
(defclass upgrade-bullet (sprite mobile collidable transient) ())

(defmethod initialize-instance :after ((thopter thopter) &key)
  (bind-key-down thopter :sdl-key-up    #'move-north)
  (bind-key-up   thopter :sdl-key-up    #'stop-north)
  (bind-key-down thopter :sdl-key-down  #'move-south)
  (bind-key-up   thopter :sdl-key-down  #'stop-south)
  (bind-key-down thopter :sdl-key-left  #'move-west)
  (bind-key-up   thopter :sdl-key-left  #'stop-west)
  (bind-key-down thopter :sdl-key-right #'move-east)
  (bind-key-up   thopter :sdl-key-right #'stop-east)
  (bind-key-down thopter :sdl-key-space #'shoot)
  (bind-key-down thopter :sdl-key-m     #'missile))

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

(defclass missile (sprite mobile collidable transient) ())

(defmethod missile ((thopter thopter) event)
  (with-slots (parent offset size veloc)
      thopter
      (make-instance 'missile
                     :parent parent 
                     :offset (+ offset (/ (x size) 2) #c(0 -4)) :depth -1
                     :veloc (+ veloc #c(0 -4))
                     :image (make-instance 'image :name :upgrade-bullet))))
(defmethod update ((missile missile) event)
  (with-slots (accel offset) missile
      (setf accel (unit (- (offset (nearest-object missile 'enemy)) offset)))))
    
(defun nearest-object (component type)
  (with-slots (offset parent) component
    (iter (for x in-vector (children parent))
          (when (typep x type)
            (finding x minimizing (dist offset (offset x)))))))

(defmethod shoot ((shooter shooter) event)
  (with-slots (parent offset size veloc firepower
                      bullet-class bullet-image bullet-veloc bullet-direction)
      shooter
    (let ((bullet-offset
           (ecase bullet-direction
             ((:north)
              (+ offset (/ (x size) 2) #c(0 -4)))
             ((:south)
              (+ offset (complex (/ (x size) 2) (y size)) #c(0 4))))))
      (loop for i from (+ (ceiling firepower -2) (if (evenp firepower) 1/2 0))
         to (floor firepower 2)
         do (make-instance bullet-class :parent parent 
                          :offset bullet-offset :depth -1
                          :veloc (+ veloc (rot bullet-veloc (* i 0.15d0 pi)))
                          :image bullet-image)))))

(defmethod collide ((thopter thopter) event)
  (with-slots (parent offset depth veloc health firepower) thopter
    (typecase (event-hit event)
      (enemy-bullet (decf health))
      (enemy        (decf health 8))
      (explosion    (decf health))
      (health-pack  (incf health 2))
      (upgrade-bullet (incf firepower)))
    (when (and parent (<= health 0))
      (make-instance 'explosion :parent parent
                     :offset offset :depth depth :veloc (/ veloc 2)
                     :image (make-instance 'image :name :explosion)
                     :timer 10)
      (detach parent thopter))))

(defmethod collide ((bullet bullet) event)
  (when (and (parent bullet) (typep (event-hit event) 'enemy))
    (detach (parent bullet) bullet)))

(defmethod collide ((bullet enemy-bullet) event)
  (when (and (parent bullet) (typep (event-hit event) 'thopter))
    (detach (parent bullet) bullet)))

(defun nearest-object (component type)
  (with-slots (offset parent) component
    (iter (for x in-vector (children parent))
          (when (typep x type)
            (finding x minimizing (dist offset (offset x)))))))

(defmethod update ((enemy enemy) event)
  (with-slots (parent (xy offset) (v veloc) (s size) accel health) enemy
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
         (away (object)
           (with-slots ((xy2 offset) (v2 veloc)) object
             (unit (rot v2 (* pi 0.25d0 (signum (cross v2 (- xy xy2))))))))
         (toward (object)
           (with-slots ((xy2 offset) (s2 size)) object
             (unit (- xy2 xy (/ s2 2d0) (/ s -2d0))))))
      (let ((nearest-thopter (nearest-object enemy 'thopter))
            (nearest-health (nearest-object enemy 'health-pack))
            (nearest-bullet (nearest-object enemy 'bullet))
            (nearest-upgrade (nearest-object enemy 'upgrade-bullet))
            (r (- xy (/ (size (game-root *game*)) 2))))
        (setf accel
              (cond ((and nearest-thopter
                          (< (dist xy (offset nearest-thopter)) 180))
                     (toward nearest-thopter))
                    ((and nearest-health
                          (< (dist xy (offset nearest-health)) 180))
                     (toward nearest-health))
                    ((and nearest-bullet
                          (< (dist xy (offset nearest-bullet)) 120))
                     (* (away nearest-bullet)
                        (if (> health 1) 0.5d0 0.75d0)))
                    ((and nearest-upgrade
                          (< (dist xy (offset nearest-upgrade)) 180))
                     (toward nearest-upgrade))
                    (t (circular r))))))))

(defmethod alarm ((enemy enemy) event)
  (with-slots (parent offset size veloc timer) enemy
    (setf timer (+ 2 (mt19937:random 25)))
    (shoot enemy event)))

(defmethod collide ((enemy enemy) event)
  (with-slots (parent offset depth veloc health firepower) enemy
    (typecase (event-hit event)
      (bullet      (decf health))
      (thopter     (decf health 8))
      (explosion   (decf health))
      (health-pack (incf health 2))
      (upgrade-bullet (incf firepower)))
    (when (and parent (<= health 0))
      (let* ((drop-class (if (zerop (mt19937:random 2))
                             'upgrade-bullet
                             'health-pack))
             (drop-image
              (make-instance 'image :name (ecase drop-class
                                            ((upgrade-bullet) :upgrade-bullet)
                                            ((health-pack) :health)))))
        (make-instance 'explosion :parent parent
                       :offset offset :depth depth :veloc (/ veloc 2)
                       :image (make-instance 'image :name :explosion)
                       :timer 10
                       :drop-class drop-class :drop-image drop-image)
        (detach parent enemy)))))

(defmethod alarm ((explosion explosion) event)
  (with-slots (parent offset size depth veloc drop-class drop-image) explosion
    (when drop-class
      (make-instance drop-class :parent parent
                     :offset (+ offset (/ (- size (size drop-image)) 2))
                     :depth depth :veloc (/ veloc 4) :image drop-image))
    (detach parent explosion)))

(defmethod collide ((health health-pack) event)
  (when (and (parent health)
             (or (typep (event-hit event) 'enemy)
                 (typep (event-hit event) 'thopter)))
    (detach (parent health) health)))

(defmethod collide ((upgrade upgrade-bullet) event)
  (when (and (parent upgrade)
             (or (typep (event-hit event) 'enemy)
                 (typep (event-hit event) 'thopter)))
    (detach (parent upgrade) upgrade)))

(defun spawn-wave (n)
  (let* ((root (game-root *game*)) (size (size root)) (center (/ size 2)))
    (loop for i from (* (1+ (floor n -2)) 128) to (* (floor n 2) 128) by 128
       do (let* ((xy (complex (+ (/ (x size) 2) i) (* (y size) -0.05d0)))
                 (v (* 2 (rot (unit (- center xy)) (/ pi 2)))))
            (make-instance 'enemy :parent root :offset xy :veloc v :depth 1
                           :image (make-instance 'image :name :enemy)
                           :health 4 :firepower (ceiling n 5) :timer 20)))))

(defmethod game-init ((game thopter-game))
  (let* ((size #c(800 600))
         (root (make-instance 'component :size size)))
    (setf (game-root game) root
          (game-view game) (make-instance 'component :size size)
          (game-sheet game)
          (make-instance 'sheet :source (resource "disp/thopter.png"))
          (game-wave game) (make-instance 'wave-controller :parent root))
    (ecase *mode*
      ((:normal)
       (let ((thopter (make-instance
                       'thopter :host :normal :parent root
                       :offset (complex (/ (x size) 2) (* (y size) 3/4))
                       :image (make-instance 'anim :name :thopter)
                       :health 4 :firepower 3)))
         (subscribe (game-keys game) thopter)))
      ((:server :client)
        (let ((thopter1 (make-instance
                         'thopter :host :server :parent root
                         :offset (complex (* (x size) 1/4) (* (y size) 3/4))
                         :image (make-instance 'anim :name :thopter)
                         :health 4 :firepower 1))
              (thopter2 (make-instance
                         'thopter :host :client :parent root
                         :offset (complex (* (x size) 3/4) (* (y size) 3/4))
                         :image (make-instance 'anim :name :thopter)
                         :health 4 :firepower 1)))
          (subscribe (game-keys game) thopter1)
          (subscribe (game-keys game) thopter2))))
    (spawn-wave (+ 2 (level (game-wave game))))))

(defmethod game-update :after ((game thopter-game))
  (let* ((thopter (iter (for i in-vector (children (game-root game)))
                        (when (and (typep i 'thopter)
                                   (eql (event-host i) *mode*))
                          (return i))))
         (s (format nil "wave: ~a, health: ~a, firepower ~a, fps: ~,2f"
                    (level (game-wave game))
                    (when thopter (health thopter))
                    (when thopter (firepower thopter))
                    (sdl:average-fps))))
    (set-caption s s))

  (when (and (zerop (iter (for i in-vector (children (game-root game)))
                          (when (typep i 'enemy) (count i))))
             (not (timer (game-wave game))))
    (setf (timer (game-wave game)) 90)))

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
