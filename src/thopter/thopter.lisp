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
  (with-slots (parent offset size) transient
    (let ((x1 (x offset)) (y1 (y offset))
          (x2 (x (offset parent))) (y2 (y (offset parent)))
          (w1 (x size)) (h1 (y size))
          (w2 (x (size parent))) (h2 (y (size parent))))
      (when (or (<= (+ x1 w1) x2)
                (<= (+ x2 w2) x1)
                (<= (+ y1 h1) y2)
                (<= (+ y2 h2) y1))
        (detach parent transient)))))

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
    :initform 0)
   (missiles
    :accessor missiles
    :initarg :missiles
    :initform 0)))
(defclass bullet (sprite mobile collidable alarm)
  ((timer :initform 60)))
(defclass missile (sprite mobile collidable alarm)
  ((timer :initform 120)))
(defclass enemy (sprite mobile collidable alarm shooter)
  ((timer :initform 10)
   (bullet-class :initform 'enemy-bullet)
   (bullet-image :initform (make-instance 'image :name :enemy-bullet))
   (bullet-veloc :initform #c(0 8))
   (bullet-direction :initform :south)
   (health
    :accessor health
    :initarg :health
    :initform 0)
   (difficulty
    :initarg :difficulty
    :initform 0)))
(defclass enemy-bullet (sprite mobile collidable alarm)
  ((timer :initform 60)))
(defclass explosion (sprite mobile collidable alarm)
  ((drop-class
    :initarg :drop-class
    :initform nil)
   (drop-image
    :initarg :drop-image
    :initform nil)))
(defclass health-pack (sprite mobile collidable transient) ())
(defclass upgrade-bullet (sprite mobile collidable transient) ())
(defclass upgrade-missile (sprite mobile collidable transient) ())

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
  (bind-key-down thopter :sdl-key-lctrl #'missile)
  (bind-key-down thopter :sdl-key-lalt  #'missile)
  ;; TODO: add reset function
  ;(bind-key-down thopter :sdl-key-r     #'reset)
  )

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

(defun nearest-object (component type)
  (with-slots (offset parent) component
    (iter (for x in-vector (children parent))
          (when (and (typep x type) (not (equal component x)))
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
                          :veloc (+ veloc (rot bullet-veloc (* i 0.1495d0 pi)))
                          :image bullet-image)))))

(defmethod missile ((thopter thopter) event)
  (with-slots (parent offset size veloc missiles) thopter
    (when (> missiles 0)
      (decf missiles)
      (make-instance 'missile
                     :parent parent 
                     :offset (+ offset (/ (x size) 2) #c(0 -4)) :depth -1
                     :veloc veloc
                     :image (make-instance 'image :name :missile-n)))))

(defmethod update ((missile missile) event)
  (with-slots (parent offset size veloc accel image) missile
    (let* ((nearest-enemy (nearest-object missile 'enemy))
           (theta (theta veloc))
           (new-image (make-instance 'image :name
                                     (cond ((or (> theta (* 0.875 pi))
                                                (< theta (* -0.875 pi)))
                                            :missile-w)
                                           ((> theta (* 0.625 pi))
                                            :missile-sw)
                                           ((> theta (* 0.375 pi))
                                            :missile-s)
                                           ((> theta (* 0.125 pi))
                                            :missile-se)
                                           ((< theta (* -0.625 pi))
                                            :missile-nw)
                                           ((< theta (* -0.375 pi))
                                            :missile-n)
                                           ((< theta (* -0.125 pi))
                                            :missile-ne)
                                           (t :missile-e)))))
      (if nearest-enemy
          (setf veloc (* (unit veloc) (min (abs veloc) 12d0))
                accel (* 2d0 (unit (- (+ (offset nearest-enemy)
                                         (/ (size nearest-enemy) 2d0))
                                      (+ offset (/ size 2d0)))))
                offset (+ offset (/ (size image) 2d0) (/ (size new-image) -2d0))
                image new-image)
          (setf accel 0)))))

(defmethod collide ((thopter thopter) event)
  (with-slots (parent offset depth veloc health firepower missiles) thopter
    (typecase (event-hit event)
      (enemy-bullet (decf health))
      (enemy        (decf health 8))
      (explosion    (decf health))
      (health-pack  (incf health 2))
      (upgrade-bullet (incf firepower))
      (upgrade-missile (incf missiles)))
    (when (and parent (<= health 0))
      (make-instance 'explosion :parent parent
                     :offset offset :depth depth :veloc (/ veloc 2)
                     :image (make-instance 'image :name :explosion)
                     :timer 10)
      (detach parent thopter))))

(defmethod update :after ((thopter thopter) event)
  (with-slots (parent offset size veloc) thopter
    ;; Clamp thopter on screen.
    (when (< (x offset) 0) (setf offset (complex 0 (y offset))))
    (when (< (y offset) 0) (setf offset (x offset)))
    (when (> (x offset) (- (x (size parent)) (x size)))
      (setf offset (complex (- (x (size parent)) (x size)) (y offset))))
    (when (> (y offset) (- (y (size parent)) (y size)))
      (setf offset (complex (x offset) (- (y (size parent)) (y size)))))))

(defmethod collide ((bullet bullet) event)
  (when (and (parent bullet) (typep (event-hit event) 'enemy))
    (detach (parent bullet) bullet)))

(defmethod alarm ((bullet bullet) event)
  (when (parent bullet)
    (detach (parent bullet) bullet)))

(defmethod collide ((missile missile) event)
  (with-slots (parent offset size depth veloc) missile
    (when (and parent (or (typep (event-hit event) 'enemy)
                          (typep (event-hit event) 'explosion)))
      (let ((explosion (make-instance 'image :name :explosion)))
        (make-instance 'explosion :parent parent
                       :offset (+ offset (/ size 2) (/ (size explosion) -2))
                       :depth depth :veloc (/ veloc 2)
                       :image explosion
                       :timer 10))
      (detach parent missile))))

(defmethod alarm ((missile missile) event)
  (with-slots (parent offset size depth veloc) missile
    (when parent
      (let ((explosion (make-instance 'image :name :explosion)))
        (make-instance 'explosion :parent parent
                       :offset (+ offset (/ size 2) (/ (size explosion) -2))
                       :depth depth :veloc (/ veloc 2)
                       :image explosion
                       :timer 10))
      (detach parent missile))))

(defmethod collide ((bullet enemy-bullet) event)
  (when (and (parent bullet) (typep (event-hit event) 'thopter))
    (detach (parent bullet) bullet)))

(defmethod alarm ((bullet enemy-bullet) event)
  (when (parent bullet)
    (detach (parent bullet) bullet)))

(defmethod update ((enemy enemy) event)
  (with-slots (parent offset size (xy offset) (v veloc) (s size) accel health difficulty)
      enemy
    (labels
        ((circular (r)
           (+ ;; circular motion
            (* (unit (- r)) (min 4 (/ (dot v v) (abs r))))
            ;; correction for radius 
            (radius-correction r)
            ;; correction for parallel veloc
            (* (- (proj v r)) 0.1d0)
            ;; correction for tangential veloc
            (* (unit (norm v r)) (- 2 (abs (norm v r))) 0.1d0)))
         ;; correction for radius, available for use
         (radius-correction (r)
           (* (unit (- r)) (- (abs r) 250) 0.01d0))
         ;; steer away from nearest enemy
         (enemy-correction (nearest-enemy)
           (if (and nearest-enemy
                (< (dist xy (offset nearest-enemy)) 32))
             (* (away nearest-enemy) 
                (/ (- 32 (dist xy (offset nearest-enemy))) 10.0d0))
             0))
         (away (object)
           (with-slots ((xy2 offset) (v2 veloc)) object
             (unit (rot v2 (* pi 0.25d0 (signum (cross v2 (- xy xy2))))))))
         (toward (object)
           (with-slots ((xy2 offset) (s2 size)) object
             (unit (- xy2 xy (/ s2 2d0) (/ s -2d0))))))
      (let ((nearest-thopter (nearest-object enemy 'thopter))
            (nearest-health (nearest-object enemy 'health-pack))
            (nearest-bullet (nearest-object enemy 'bullet))
            (nearest-missile (nearest-object enemy 'missile))
            (nearest-upgrade-b (nearest-object enemy 'upgrade-bullet))
            (nearest-upgrade-m (nearest-object enemy 'upgrade-missile))
            (r (- xy (/ (size (game-root *game*)) 2))))
        (setf v (* (unit v) (min (abs v) (+ 8d0 (* 2d0 difficulty))))
              accel
              (+ (cond ((and nearest-thopter
                             (< (dist xy (offset nearest-thopter)) 180))
                        (* (toward nearest-thopter) (max 0.5d0 difficulty)))
                       ((and nearest-health
                             (< (dist xy (offset nearest-health)) 180))
                        (* (toward nearest-health) (max 0.5d0 difficulty)))
                       ((and nearest-missile
                             (< (dist xy (offset nearest-missile)) 120))
                        (+ (* (away nearest-missile)
                              (+ 0.5d0 (* 0.1d0 difficulty))
                              (if (> health 1) 1d0 1.5d0))
                           (radius-correction r)))
                       ((and nearest-bullet
                             (< (dist xy (offset nearest-bullet)) 120))
                        (+ (* (away nearest-bullet)
                              (+ 0.5d0 (* 0.1d0 difficulty))
                              (if (> health 1) 1d0 1.5d0))
                           (radius-correction r)))
                       ((and nearest-upgrade-b
                             (< (dist xy (offset nearest-upgrade-b)) 180))
                        (* (toward nearest-upgrade-b) (max 0.5d0 difficulty)))
                       ((and nearest-upgrade-m
                             (< (dist xy (offset nearest-upgrade-m)) 180))
                        (* (toward nearest-upgrade-m) (max 0.5d0 difficulty)))
                       (t (circular r)))
                 (enemy-correction (nearest-object enemy 'enemy))))))))

(defmethod alarm ((enemy enemy) event)
  (with-slots (parent offset size veloc timer) enemy
    (setf timer (+ 5 (mt19937:random 30) (mt19937:random 30)))
    (shoot enemy event)))

(defmethod collide ((enemy enemy) event)
  (with-slots (parent offset depth veloc health firepower) enemy
    (typecase (event-hit event)
      (bullet      (decf health))
      (missile     (decf health))
      (thopter     (decf health 8))
      (explosion   (decf health))
      (health-pack (incf health 2))
      (upgrade-bullet (incf firepower)))
    (when (and parent (<= health 0))
      (let* ((random-choice (mt19937:random 3))
             (drop-class (ecase random-choice
                           ((0) 'upgrade-bullet)
                           ((1) 'upgrade-missile)
                           ((2) 'health-pack)))
             (drop-image
              (make-instance 'image :name (ecase drop-class
                                            ((upgrade-bullet) :upgrade-bullet)
                                            ((upgrade-missile) :upgrade-missile)
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

(defmethod collide ((upgrade upgrade-missile) event)
  (when (and (parent upgrade)
             (or (typep (event-hit event) 'enemy)
                 (typep (event-hit event) 'thopter)))
    (detach (parent upgrade) upgrade)))

(defun spawn-wave (w n)
  (let* ((root (game-root *game*)) (size (size root)) (center (/ size 2)))
    (loop for i from (* (1+ (floor n -2)) 64) to (* (floor n 2) 64) by 64
       do (let* ((xy (complex (+ (/ (x size) 2) i) (* (y size) -0.05d0)))
                 (v (* 2 (rot (unit (- center xy)) (/ pi 2)))))
            (make-instance 'enemy :parent root :offset xy :veloc v :depth 1
                           :image (make-instance 'image :name :enemy)
                           :health 4 :timer 20
                           :firepower (max 1 (ceiling w 3))
                           :difficulty (floor w 5))))))

(defmethod game-init ((game thopter-game))
  (let* ((size #c(800 600))
         (root (make-instance 'component :size size)))
    (setf (game-root game) root
          (game-view game) (make-instance 'component :size size)
          (game-sheet game)
          (make-instance 'sheet :source (resource "disp/thopter.png"))
          (game-wave game) (make-instance 'wave-controller :parent root))
    (ecase *host*
      ((:normal)
       (let ((thopter (make-instance
                       'thopter :host :normal :parent root
                       :offset (complex (/ (x size) 2) (* (y size) 3/4))
                       :image (make-instance 'anim :name :thopter)
                       :health 4 :firepower 3 :missiles 2)))
         (subscribe (game-keys game) thopter)))
      ((:server :client)
        (let ((thopter1 (make-instance
                         'thopter :host :server :parent root
                         :offset (complex (* (x size) 1/4) (* (y size) 3/4))
                         :image (make-instance 'anim :name :thopter)
                         :health 4 :firepower 3 :missiles 1))
              (thopter2 (make-instance
                         'thopter :host :client :parent root
                         :offset (complex (* (x size) 3/4) (* (y size) 3/4))
                         :image (make-instance 'anim :name :thopter2)
                         :health 4 :firepower 3 :missiles 1)))
          (subscribe (game-keys game) thopter1)
          (subscribe (game-keys game) thopter2))))
    (spawn-wave (level (game-wave game)) (+ 2 (level (game-wave game))))
    (play
     (make-instance
      'sample :name :music :source (resource "sound/music.mp3") :type :music)
     :loop t)))

(defmethod game-update :after ((game thopter-game))
  (let* ((thopter (iter (for i in-vector (children (game-root game)))
                        (when (and (typep i 'thopter)
                                   (eql (event-host i) *host*))
                          (return i))))
         (s (format
             nil "wave: ~a, health: ~a, firepower: ~a, missiles: ~a, fps: ~,2f"
             (level (game-wave game))
             (when thopter (health thopter))
             (when thopter (firepower thopter))
             (when thopter (missiles thopter))
             (sdl:average-fps))))
    (set-caption s s))

  (when (and (zerop (iter (for i in-vector (children (game-root game)))
                          (when (typep i 'enemy) (count i))))
             (not (timer (game-wave game))))
    (setf (timer (game-wave game)) 120)))

(defmethod alarm ((wave wave-controller) event)
  (with-slots (level) wave
    (incf level)
    (spawn-wave level (+ 2 level))))

;; For interactive use:
(defun thopter ()
  (let ((*game* (make-instance 'thopter-game)))
    (main :exit-when-done nil)))

;; For non-interactive use:
(defvar *game* (make-instance 'thopter-game))
