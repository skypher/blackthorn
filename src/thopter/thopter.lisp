;;;; Blackthorn -- Lisp Game Engine
;;;;
;;;; Copyright (c) 2007-2009, Elliott Slaughter <elliottslaughter@gmail.com>
;;;; Copyright (c) 2010-2011, Elliott Slaughter <elliottslaughter@gmail.com>
;;;;                          and Michael Matthews <iismichaels@gmail.com>
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

(defmethod clear-flags ((object flag-mixin))
  (with-slots (flags) object
    (clrhash flags)))

(defclass direction-mixin (flag-mixin)
  ((speed
    :accessor speed
    :initarg :speed)))

;;;
;;; Thopter-specific implementation
;;;

(defvar *game-size* #c(960 720))

(defclass thopter-game (game)
  ((game-view
    :initform (make-instance 'component :size *game-size*))
   (font
    :accessor game-font)
   (sound
    :accessor game-sound)
   (player
    :accessor game-player)
   (players
    :accessor game-players)
   (play-screen
    :accessor game-play-screen)
   (menu-screen
    :accessor game-menu-screen)
   (item-screen
    :accessor game-item-screen)))

(defclass thopter-play-screen (screen)
  ((quit
    :accessor game-quit)
   (pause
    :accessor game-pause)
   (wave
    :accessor game-wave)
   (background
    :accessor game-background)
   (sound
    :accessor game-sound)
   (enemy-missiles
    :accessor enemy-missiles
    :initform 0)
   (missile-lock
    :accessor missile-lock
    :initform nil)
   (players-left
    :accessor players-left)))

(defclass thopter-menu-screen (screen)
  ((game-root
    :initform (make-instance 'component :size *game-size*))
   (game-view
    :initform (make-instance 'component :size *game-size*))
   (start
    :accessor game-start)
   (quit
    :accessor game-quit)))

(defclass thopter-item-screen (screen)
  ((game-root
    :initform (make-instance 'component :size *game-size*))
   (game-view
    :initform (make-instance 'component :size *game-size*))
   (resume
    :accessor game-resume)
   (quit
    :accessor game-quit)))

(defclass start-controller (actor)
  ())

(defclass pause-controller (actor)
  ())

(defclass resume-controller (actor)
  ())

(defclass wave-controller (alarm)
  ((level
    :accessor level
    :initform 0)))

(defclass background-controller (alarm)
  ())

(defclass players-left-controller (alarm)
  ((players-left
    :accessor players-left
    :initform 0)))

(defclass quit-controller (actor) ())

(defclass tile (sprite mobile transient)
  ())

(defclass player ()
  ((host
    :reader event-host
    :initarg :host
    :initform nil)))

(defclass weapon-class ()
  ((projectile-class
    :initarg :projectile-class)
   (projectile-image
    :initarg :projectile-image)
   (projectile-n-directions
    :initarg :projectile-n-directions)
   (projectile-veloc
    :initarg :projectile-veloc)
   (projectile-timer
    :initarg :projectile-timer)
   (fire-sound
    :accessor fire-sound
    :initarg :fire-sound
    :initform nil)
   (cooldown
    :accessor cooldown
    :initarg :cooldown
    :initform nil)
   (initial-firepower
    :accessor initial-irepower
    :initarg :initial-firepower
    :initform nil)
   (initial-ammo
    :accessor initial-ammo
    :initarg :initial-ammo
    :initform 1)
   (ammo-refill-rate
    :accessor ammo-refill-rate
    :initarg :ammo-refill-rate
    :initform 1)
   (ammo-deplete-rate
    :accessor ammo-deplete-rate
    :initarg :ammo-deplete-rate
    :initform 1)
   (angle-increment
    :accessor angle-increment
    :initarg :angle-increment
    :initform 0.15d0)
   (max-spread
    :accessor max-spread
    :initarg :max-spread
    :initform 2)
   (weapon-type
    :accessor  weapon-type
    :initarg  :weapon-type
    :initform :radial)))

(defclass weapon (alarm)
  ((timer :initform nil)
   (firepower 
    :accessor firepower
    :initarg :firepower
    :initform nil)
   (ammo
    :accessor ammo
    :initarg :ammo
    :initform 1)
   (weapon-class
    :accessor weapon-class
    :initarg :weapon-class
    :initform nil)))

(defclass spread-weapon (weapon-class)
  ((projectile-class :initform  'bullet)
   (projectile-image :initform :bullet)
   (projectile-n-directions :initform 16)
   (projectile-veloc :initform #c(0 -12))
   (projectile-timer :initform 120)
   (fire-sound
    :initform (make-instance 'sample :name :thopter-gun
                             :source (resource "sound/thoptergun.ogg")
                             :type :sample))
   (cooldown :initform 4)
   (initial-firepower :initform nil)
   (initial-ammo :initform 1)
   (ammo-refill-rate :initform 200)
   (ammo-deplete-rate :initform 200)))

(defclass chaingun-weapon (weapon-class)
  ((projectile-class :initform  'bullet)
   (projectile-image :initform :bullet)
   (projectile-n-directions :initform 16)
   (projectile-veloc :initform #c(0 -12))
   (projectile-timer :initform 120)
   (fire-sound
    :initform (make-instance 'sample :name :thopter-gun
                             :source (resource "sound/thoptergun.ogg")
                             :type :sample))
   (cooldown :initform 4)
   (initial-firepower :initform nil)
   (initial-ammo :initform 1)
   (ammo-refill-rate :initform 200)
   (ammo-deplete-rate :initform 200)
   (weapon-type :initform :straight)))

(defclass missile-weapon (weapon-class)
  ((projectile-class :initform 'missile)
   (projectile-image :initform :missile)
   (projectile-n-directions :initform 16)
   (projectile-veloc :initform #c(0 -4))
   (projectile-timer :initform 60)
   (fire-sound
    :initform (make-instance 'sample :name :thopter-gun
                             :source (resource "sound/missile.ogg")
                             :type :sample))
   (cooldown :initform nil)
   (initial-firepower :initform 1)
   (initial-ammo :initform 2)
   (ammo-refill-rate :initform 1)
   (ammo-deplete-rate :initform 1)))

(defclass thopter (mobile sprite collidable direction-mixin)
  ((speed :initform 8)
   (timer :initform nil)
   (primary-weapon
    :accessor primary-weapon
    :initarg :primary-weapon)
;   (primary-weapon2
;    :accessor primary-weapon2
;    :initarg :primary-weapon2)
   (secondary-weapon
    :accessor secondary-weapon
    :initarg :secondary-weapon)
   (boosted-speed
    :accessor boosted-speed
    :initform 10)
   (speed-boost
    :accessor speed-boost
    :initarg :speed-boost
    :initform 0)
   (max-speed-timer
    :accessor max-speed-timer
    :initarg :max-speed-timer
    :initform 900)
   (boost-on
    :accessor boost-on
    :initarg :max-speed-timer
    :initform t)
   (health
    :accessor health
    :initarg :health
    :initform 0)))

(defclass bullet (sprite mobile collidable transient alarm)
  ((timer :initform 60)
   (reactive-collisions-only-p :initform t)))
(defclass missile (sprite mobile collidable alarm)
  ((health
    :accessor health
    :initarg :health
    :initform 4)
   (timer :initform 120)))

(defclass enemy (mobile sprite collidable alarm)
  ((primary-weapon :accessor primary-weapon :initarg :primary-weapon)
   (secondary-weapon :accessor secondary-weapon :initarg :secondary-weapon)
   (timer :initform (+ 20 (mt19937:random 10) (mt19937:random 10)))
   (speed-boost :initform 0 :accessor speed-boost :initarg :speed-boost)
   (health
    :accessor health
    :initarg :health
    :initform 0)
   (missile-chance
    :accessor missile-chance
    :initarg :missile-chance
    :initform 0)
   (difficulty
    :initarg :difficulty
    :initform 0)
   (veloc-scale
    :initarg :veloc-scale
    :initform 1d0)
   (ignore-objects
    :initarg :ignore-objects
    :initform nil)
   (fire-rate
    :initarg :fire-rate
    :initform 1)))

(defclass enemy-ship (enemy)
  ((image :initform (make-image :enemy))
   (health :initform 4)
   (veloc-scale :initform 2d0)))

(defclass enemy-boss (enemy)
  ((image :initform (make-image :boss))
   (bullet-timer :initform 120)
   (health :initform 100)
   (veloc-scale :initform 0.5d0)
   (ignore-objects :initform t)
   (fire-rate :initform 2)))

(defclass enemy-bullet (sprite mobile collidable transient alarm)
  ((timer :initform 60)
   (reactive-collisions-only-p :initform t)))

(defclass enemy-missile (sprite mobile collidable alarm)
  ((health
    :accessor health
    :initarg :health
    :initform 8)
   (timer :initform 120)))
(defclass explosion (sprite mobile collidable alarm)
  ((drop-class
    :initarg :drop-class
    :initform nil)
   (drop-image
    :initarg :drop-image
    :initform nil)
   (reactive-collisions-only-p :initform t)))
(defclass health-pack (sprite mobile collidable transient)
  ((reactive-collisions-only-p :initform t)))
(defclass upgrade-bullet (sprite mobile collidable transient)
  ((reactive-collisions-only-p :initform t)))
(defclass upgrade-missile (sprite mobile collidable transient)
  ((reactive-collisions-only-p :initform t)))
(defclass upgrade-speed (sprite mobile collidable transient)
  ((reactive-collisions-only-p :initform t)))

(defvar *spread-weapon* (make-instance 'spread-weapon
                                       :max-spread 1/4))
(defvar *chaingun-weapon* (make-instance 'chaingun-weapon))
(defvar *missile-weapon* (make-instance 'missile-weapon))
(defvar *enemy-spread-weapon* (make-instance 'spread-weapon
                                             :projectile-class 'enemy-bullet
                                             :projectile-image :enemy-bullet
                                             :projectile-veloc #c(0 +12)
                                             :fire-sound nil))
(defvar *enemy-chaingun-weapon* (make-instance 'chaingun-weapon
                                               :projectile-class 'enemy-bullet
                                               :projectile-image :enemy-bullet
                                               :projectile-veloc #c(0 +12)
                                               :fire-sound nil))
(defvar *enemy-missile-weapon* (make-instance 'missile-weapon
                                              :projectile-class 'enemy-missile
                                              :projectile-image :enemy-missile
                                              :projectile-veloc #c(0 +4)))

(defmethod initialize-instance :after ((weapon weapon) &key weapon-class
                                       firepower ammo)
  (with-slots (initial-firepower initial-ammo ammo-deplete-rate) weapon-class
    (let ((firepower (or firepower initial-firepower)))
      (setf (firepower weapon) firepower
            (ammo weapon) (if firepower
                              (or ammo initial-ammo)
                              (* (or ammo initial-ammo)
                                 ammo-deplete-rate))))))

(defvar *dir-names* '(:north :south :west :east))
(defvar *dir-vectors* '(#c(0 -1) #c(0 1) #c(-1 0) #c(1 0)))

(defmethod change-veloc ((object direction-mixin) event)
  (with-slots (veloc speed boosted-speed boost-on) object
    (setf veloc
          (* (if (and (> (speed-boost object) 0) boost-on) boosted-speed speed)
             (unit (iter (for d in *dir-names*) (for v in *dir-vectors*)
                         (when (get-flag object d) (sum v))))))))

(defun doall (&rest handlers)
  #'(lambda (object event)
      (iter (for handler in handlers) (funcall handler object event))))

(defmethod initialize-instance :after ((thopter thopter) &key)
  (iter (for (d k) in '((:north :sdl-key-up) (:south :sdl-key-down)
                        (:west :sdl-key-left) (:east :sdl-key-right)
                        (:north :sdl-key-w) (:south :sdl-key-s)
                        (:west :sdl-key-a) (:east :sdl-key-d)
                        (:north :sdl-key-i) (:south :sdl-key-k)
                        (:west :sdl-key-j) (:east :sdl-key-l)))
        (bind-key-down thopter k (doall (set-flag d t) #'change-veloc))
        (bind-key-up thopter k (doall (set-flag d nil) #'change-veloc)))
  (bind-key-down thopter :sdl-key-space #'start-shoot-primary)
  (bind-key-up thopter :sdl-key-space #'stop-shoot-primary)
  (iter (for k in '(:sdl-key-lctrl :sdl-key-lalt
                    :sdl-key-rctrl :sdl-key-ralt))
        (bind-key-down thopter k #'start-shoot-secondary)
        (bind-key-up thopter k #'stop-shoot-secondary))
  (bind-key-down thopter :sdl-key-lshift #'toggle-boost)
  (bind-key-down thopter :sdl-key-rshift #'toggle-boost))

(defun nearest-object (component type radius)
  (find-nearest-object component radius :test #'(lambda (x) (typep x type))))

(defun quadrant (x n)
  (mod (floor (+ (/ (* (theta x) n) (* 2 pi)) 0.5d0)) n))

(defmethod stop-all ((thopter thopter) event)
  (clear-flags thopter)
  (change-veloc thopter event)
  (stop-shoot-primary thopter event)
  (stop-shoot-secondary thopter event))

(defmethod start-shoot ((weapon weapon) event)
  (shoot weapon event)
  (setf (timer weapon) (cooldown (weapon-class weapon))))

(defmethod stop-shoot ((weapon weapon) event)
  (setf (timer weapon) nil))

(defmethod alarm ((weapon weapon) event)
  (shoot weapon event)
  (setf (timer weapon) (cooldown (weapon-class weapon))))

(defmethod start-shoot-primary ((thopter thopter) event)
  (start-shoot (primary-weapon thopter) event))
  ;(start-shoot (primary-weapon2 thopter) event))

(defmethod stop-shoot-primary ((thopter thopter) event)
  (stop-shoot (primary-weapon thopter) event))
  ;(stop-shoot (primary-weapon2 thopter) event))

(defmethod start-shoot-secondary ((thopter thopter) event)
  (start-shoot (secondary-weapon thopter) event))

(defmethod stop-shoot-secondary ((thopter thopter) event)
  (stop-shoot (secondary-weapon thopter) event))

(defmethod toggle-boost ((thopter thopter) event)
  (if (boost-on thopter)
      (progn (setf (boost-on thopter) nil) (change-veloc thopter event))
      (setf (boost-on thopter) t)))

(defmethod shoot ((weapon weapon) event)
  (with-slots (weapon-class) weapon
    (with-slots (fire-sound weapon-type) weapon-class
      (shoot-helper weapon weapon-type event)
      (when fire-sound
        (play fire-sound)))))

(defmethod shoot-helper ((weapon weapon) (weapon-type (eql :radial)) event)
  (with-slots ((shooter parent) ammo weapon-class (offset-w offset)) weapon
    (with-slots (projectile-class projectile-image
                 projectile-n-directions projectile-veloc projectile-timer
                 ammo-deplete-rate angle-increment max-spread) weapon-class
      (with-slots (parent offset size veloc) shooter
        (when (> ammo 0)
          (let* ((firepower (if (firepower weapon)
                                (firepower weapon)
                                (ceiling ammo ammo-deplete-rate)))
                 (increment (if (< (* firepower angle-increment) max-spread)
                                (* angle-increment pi)
                                (/ (* max-spread pi) firepower))))
            (iter (for i from (+ (ceiling firepower -2)
                                 (if (evenp firepower) 1/2 0))
                       to (floor firepower 2))
                  (for v = (+ veloc (rot projectile-veloc (* i increment))))
                  (for image = (if projectile-n-directions
                                   (make-image
                                    "~a-~2,'0d" projectile-image
                                    (quadrant v projectile-n-directions))
                                   (make-image projectile-image)))
                  (make-instance
                   projectile-class :parent parent 
                   :offset (+ offset offset-w (/ size 2) (- (/ (size image) 2))
                              (* (rot (unit projectile-veloc)
                                      (* i increment))
                                 (+ (x size) (y size))
                                 0.25d0))
                   :depth -1
                   :veloc v
                   :image image
                   :timer projectile-timer))
            (decf ammo firepower)))))))

(defmethod shoot-helper ((weapon weapon) (weapon-type (eql :straight)) event)
  (with-slots ((shooter parent) ammo weapon-class (offset-w offset)) weapon
    (with-slots (projectile-class projectile-image
                 projectile-n-directions projectile-veloc projectile-timer
                 ammo-deplete-rate angle-increment max-spread) weapon-class
      (with-slots (parent offset size veloc) shooter
        (when (> ammo 0)
          (let* ((firepower (if (firepower weapon)
                                (firepower weapon)
                                (ceiling ammo ammo-deplete-rate)))
                 (increment (if (< (* firepower angle-increment) max-spread)
                                (* angle-increment pi)
                                (/ (* max-spread pi) firepower))))
            (iter (for i from (+ (ceiling firepower -2)
                                 (if (evenp firepower) 1/2 0))
                       to (floor firepower 2))
                  (for v = (+ veloc projectile-veloc))
                  (for image = (if projectile-n-directions
                                   (make-image
                                    "~a-~2,'0d" projectile-image
                                    (quadrant v projectile-n-directions))
                                   (make-image projectile-image)))
                  (make-instance
                   projectile-class :parent parent 
                   :offset (+ offset offset-w (/ (size weapon) 2)
                              (- (/ (size image) 2))
                              (* (rot (unit projectile-veloc)
                                      (* i increment))
                                 (max (x size) (y size))
                                 0.5d0))
                   :depth -1
                   :veloc v
                   :image image
                   :timer projectile-timer))
            (decf ammo firepower)))))))

(defmethod missile ((enemy enemy) event)
  (shoot (secondary-weapon enemy) event)
  (when (<= (enemy-missiles (game-screen *game*)) 0)
    (setf (missile-lock (game-screen *game*))
      (play (make-instance 'sample
              :name :beep
              :source (resource "sound/beep.ogg")
              :type :sample) :loop t)))
  (incf (enemy-missiles (game-screen *game*))))

(defmethod update ((missile missile) event)
  (with-slots (parent offset size veloc accel image) missile
    (let* ((nearest-enemy (nearest-object missile 'enemy 600))
            (new-image (make-image "~a-~2,'0d" :missile (quadrant veloc 16))))
      (if nearest-enemy
        (setf veloc (* (unit veloc) (min (abs veloc) 12d0))
          accel (* 2d0 (unit (- (+ (offset nearest-enemy)
                                  (/ (size nearest-enemy) 2d0))
                               (+ offset (/ size 2d0)))))
          offset (+ offset (/ (size image) 2d0) (/ (size new-image) -2d0))
          image new-image)
        (setf accel 0)))))

(defmethod update ((missile enemy-missile) event)
  (with-slots (parent offset size veloc accel image) missile
    (let* ((nearest-enemy (nearest-object missile 'thopter 400))
            (new-image (make-image "~a-~2,'0d" :enemy-missile
                         (quadrant veloc 16))))
      (if nearest-enemy
        (setf veloc (* (unit veloc) (min (abs veloc) 12d0))
          accel (* 2d0 (unit (- (+ (offset nearest-enemy)
                                  (/ (size nearest-enemy) 2d0))
                               (+ offset (/ size 2d0)))))
          offset (+ offset (/ (size image) 2d0) (/ (size new-image) -2d0))
          image new-image)
        (setf accel 0)))))

(defmethod collide ((thopter thopter) event)
  (with-slots (parent offset depth veloc health primary-weapon
               secondary-weapon missiles speed-boost) thopter
    (typecase (event-hit event)
      (enemy-bullet  (decf health))
      (enemy-missile (decf health))
      (enemy         (decf health 8))
      (explosion     (decf health))
      (health-pack   (incf health 2))
      (upgrade-bullet (incf (ammo primary-weapon)
                            (ammo-refill-rate (weapon-class primary-weapon))))
      (upgrade-missile (incf (ammo secondary-weapon)
                             (ammo-refill-rate
                              (weapon-class secondary-weapon))))
      (upgrade-speed (progn
                       (setf speed-boost
                             (min (+ speed-boost 240) (max-speed-timer thopter)))
                       (change-veloc thopter event))))
    (when (and parent (<= health 0))
      (make-instance 'explosion :parent parent
                     :offset offset :depth depth :veloc (/ veloc 2)
                     :image (make-anim :explosion)
                     :timer 28)
      (detach parent thopter)
      (decf (players-left (players-left (game-screen *game*)))))))

(defmethod update :after ((thopter thopter) event)
  (with-slots (parent offset size veloc) thopter
    ;; Clamp thopter on screen.
    (when (< (x offset) 0) (setf offset (complex 0 (y offset))))
    (when (< (y offset) 0) (setf offset (x offset)))
    (when (> (x offset) (- (x (size parent)) (x size)))
      (setf offset (complex (- (x (size parent)) (x size)) (y offset))))
    (when (> (y offset) (- (y (size parent)) (y size)))
      (setf offset (complex (x offset) (- (y (size parent)) (y size)))))
    (when (and (> (speed-boost thopter) 0) (boost-on thopter))
      (decf (speed-boost thopter))
      (change-veloc thopter event))))

(defmethod collide ((bullet bullet) event)
  (when (and (parent bullet) (typecase (event-hit event)
                               (enemy t)
                               (enemy-missile t)))
    (detach (parent bullet) bullet)))

(defmethod alarm ((bullet bullet) event)
  (when (parent bullet)
    (detach (parent bullet) bullet)))

(defmethod collide ((missile missile) event)
  (with-slots (parent offset size depth veloc health) missile
    (typecase (event-hit event)
      (enemy-bullet  (decf health))
      (enemy-missile (setf health 0))
      (enemy         (setf health 0))
      (explosion     (decf health)))
    (when (and parent (<= health 0))
      (let ((explosion (make-anim :explosion)))
        (make-instance 'explosion :parent parent
                       :offset (+ offset (/ size 2) (/ (size explosion) -2))
                       :depth depth :veloc (/ veloc 2)
                       :image explosion
                       :timer 28))
      (detach parent missile))))

(defmethod collide ((missile enemy-missile) event)
  (with-slots (parent offset size depth veloc health) missile
    (typecase (event-hit event)
      (bullet    (decf health))
      (missile   (setf health 0))
      (thopter   (setf health 0))
      (explosion (decf health)))
    (when (and parent (<= health 0))
      (let ((explosion (make-anim :explosion)))
        (make-instance 'explosion :parent parent
                       :offset (+ offset (/ size 2) (/ (size explosion) -2))
                       :depth depth :veloc (/ veloc 2)
                       :image explosion
                       :timer 28))
      (detach parent missile)
      (decf (enemy-missiles (game-screen *game*)))
      (when (and (<= (enemy-missiles (game-screen *game*)) 0)
                 (missile-lock (game-screen *game*)))
        (stop :channel (missile-lock (game-screen *game*)))
        (setf (missile-lock (game-screen *game*)) nil)))))

(defmethod alarm ((missile missile) event)
  (with-slots (parent offset size depth veloc) missile
    (when parent
      (let ((explosion (make-anim :explosion)))
        (make-instance 'explosion :parent parent
                       :offset (+ offset (/ size 2) (/ (size explosion) -2))
                       :depth depth :veloc (/ veloc 2)
                       :image explosion
                       :timer 28))
      (detach parent missile))))

(defmethod alarm ((missile enemy-missile) event)
  (with-slots (parent offset size depth veloc) missile
    (when parent
      (let ((explosion (make-anim :explosion)))
        (make-instance 'explosion :parent parent
                       :offset (+ offset (/ size 2) (/ (size explosion) -2))
                       :depth depth :veloc (/ veloc 2)
                       :image explosion
                       :timer 28))
      (detach parent missile)
      (decf (enemy-missiles (game-screen *game*)))
      (when (and (<= (enemy-missiles (game-screen *game*)) 0)
                 (missile-lock (game-screen *game*)))
        (stop :channel (missile-lock (game-screen *game*)))
        (setf (missile-lock (game-screen *game*)) nil)))))

(defmethod collide ((bullet enemy-bullet) event)
  (when (and (parent bullet) (typecase (event-hit event)
                               (thopter t)
                               (missile t)))
    (detach (parent bullet) bullet)))

(defmethod alarm ((bullet enemy-bullet) event)
  (when (parent bullet)
    (detach (parent bullet) bullet)))

(defmethod update ((enemy enemy) event)
  (with-slots (parent offset size (xy offset) (v veloc) (s size)
               accel health difficulty veloc-scale ignore-objects) enemy
    (labels
        ((circular (r)
           (+ ;; circular motion
            (* (unit (- r)) (/ (dot v v) (abs r)))
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
           (if nearest-enemy
               (* (away nearest-enemy)
                  (- (/ (+ (x s) (y s)) 2d0)
                     (dist xy (offset nearest-enemy)))
                  0.1d0)
               0))
         (away (object)
           (with-slots ((xy2 offset) (v2 veloc)) object
             (unit (rot v2 (* pi 0.25d0 (signum (cross v2 (- xy xy2))))))))
         (toward (object)
           (with-slots ((xy2 offset) (s2 size)) object
             (unit (- xy2 xy (/ s2 2d0) (/ s -2d0))))))
      (let ((nearest-thopter (nearest-object enemy 'thopter 180))
            (nearest-health (nearest-object enemy 'health-pack 180))
            (nearest-missile (nearest-object enemy 'missile 120))
            (nearest-explosion (nearest-object enemy 'explosion 120))
            (nearest-bullet (nearest-object enemy 'bullet 120))
            (nearest-upgrade-b (nearest-object enemy 'upgrade-bullet 180))
            (nearest-upgrade-m (nearest-object enemy 'upgrade-missile 180))
            (nearest-upgrade-s (nearest-object enemy 'upgrade-speed 180))
            (r (+ xy (/ s 2) (/ (size (game-root *game*)) -2))))
        (setf v (* (unit v) (min 
                     (abs (* v (if (> (speed-boost enemy) 0) 1.2d0 1d0)))
                     (+ 8d0 (* veloc-scale difficulty))))
              accel
                 (+ (cond (ignore-objects (circular r))
                       (nearest-thopter
                        (* (toward nearest-thopter) (max 0.5d0 difficulty)))
                       (nearest-health
                        (* (toward nearest-health) (max 0.5d0 difficulty)))
                       (nearest-missile
                        (+ (* (away nearest-missile)
                              (+ 0.5d0 (* 0.1d0 difficulty))
                              (if (> health 1) 1d0 1.5d0))
                           (radius-correction r)))
                       (nearest-explosion
                        (+ (* (away nearest-explosion)
                              (+ 0.5d0 (* 0.1d0 difficulty))
                              (if (> health 1) 1d0 1.5d0))
                           (radius-correction r)))
                       (nearest-bullet
                        (+ (* (away nearest-bullet)
                              (+ 0.5d0 (* 0.1d0 difficulty))
                              (if (> health 1) 1d0 1.5d0))
                           (radius-correction r)))
                       (nearest-upgrade-b
                        (* (toward nearest-upgrade-b) (max 0.5d0 difficulty)))
                       (nearest-upgrade-m
                        (* (toward nearest-upgrade-m) (max 0.5d0 difficulty)))
                       (nearest-upgrade-s
                        (* (toward nearest-upgrade-s) (max 0.5d0 difficulty)))
                       (t (circular r)))
                 (enemy-correction (nearest-object enemy 'enemy 0))))
        (when (> (speed-boost enemy) 0)
          (decf (speed-boost enemy)))))))

(defmethod alarm ((enemy enemy) event)
  (with-slots (parent offset size veloc timer 
                      secondary-weapon missile-chance fire-rate) enemy
    (setf timer
          (ceiling (+ 5 (mt19937:random 30) (mt19937:random 30)) fire-rate))
    (let* ((shoot-decision (mt19937:random 1d0)))
      (if (and (> (ammo secondary-weapon) 0) (< shoot-decision missile-chance))
        (missile enemy event)
        (shoot (primary-weapon enemy) event)))))

(defmethod collide ((enemy enemy) event)
  (with-slots (parent offset size depth veloc health primary-weapon 
               secondary-weapon missiles) enemy
    (typecase (event-hit event)
      (bullet      (decf health))
      (missile     (decf health))
      (thopter     (decf health 8))
      (explosion   (decf health))
      (health-pack (incf health 2))
      (upgrade-bullet (incf (ammo primary-weapon)
                             (ammo-refill-rate (weapon-class primary-weapon))))
      (upgrade-missile (incf (ammo secondary-weapon)
                             (ammo-refill-rate
                              (weapon-class secondary-weapon))))
      (upgrade-speed (setf (speed-boost enemy) 240)))
    (when (and parent (<= health 0))
      (let ((i (if (typep enemy 'enemy-boss) 25 1)))
        (loop repeat i 
           do (let* ((random-choice (mt19937:random 1d0))
                     ; not happy with way chances are, prefer individual
                     (upgrade-bullet-chance 0.30)
                     (upgrade-missile-chance 0.6d0)
                     (health-pack-chance 0.90)
                     (upgrade-speed-chance 1.00d0)
                     (drop-class (if (< random-choice upgrade-bullet-chance)
                                     'upgrade-bullet
                                     (if (< random-choice upgrade-missile-chance)
                                         'upgrade-missile
                                         (if (< random-choice health-pack-chance)
                                             'health-pack
                                             'upgrade-speed))))
                     (drop-image
                      (make-anim-or-image (ecase drop-class
                                            ((upgrade-bullet) :upgrade-bullet)
                                            ((upgrade-missile) :upgrade-missile)
                                            ((health-pack) :health)
                                            ((upgrade-speed) :upgrade-speed))))
                     (image (make-anim :explosion))
                     (bound (- size (size image))))
                (make-instance
                 'explosion :parent parent
                 :offset (+ offset
                            (complex (mt19937:random (max 1 (x bound)))
                                     (mt19937:random (max 1 (y bound)))))
                 :depth depth
                 :veloc (if (= i 1)
                            (/ veloc 2)
                            (+ (/ veloc 2) (complex (- (mt19937:random 6d0) 3d0)
                                                    (- (mt19937:random 6d0) 3d0))))
                 :image image
                 :timer 28
                 :drop-class drop-class :drop-image drop-image)))
        (detach parent enemy)))))

(defmethod alarm ((explosion explosion) event)
  (with-slots (parent offset size depth veloc drop-class drop-image) explosion
    (when drop-class
      (make-instance drop-class :parent parent
                     :offset (+ offset (/ (- size (size drop-image)) 2))
                     :depth depth 
                     :veloc (* (unit veloc) (min (/ (abs veloc) 4) 1))
                     :image drop-image))
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

(defmethod collide ((upgrade upgrade-speed) event)
  (when (and (parent upgrade)
             (or (typep (event-hit event) 'enemy)
                 (typep (event-hit event) 'thopter)))
    (detach (parent upgrade) upgrade)))

(defmethod initialize-instance :after ((screen thopter-play-screen) &key)
  (setf (game-sheet screen) (load-sheet (resource "disp/thopter.png"))))

(defun setup-game (screen)
  (setf (game-root screen) (make-instance 'component :size *game-size*)
        (game-view screen) (make-instance 'component :size *game-size*))
  (let* ((root (game-root screen))
         (size (size root)))
    (setf (game-wave screen) (make-instance 'wave-controller :parent root)
          (game-quit screen) (make-instance 'quit-controller :parent root)
          (game-pause screen) (make-instance 'pause-controller :parent root)
          (game-background screen)
          (make-instance 'background-controller :parent root :timer 0)
          (players-left screen)
          (make-instance 'players-left-controller :parent root)
          (game-sound screen)
          (play (make-instance 'sample
                               :name :thopter-blades
                               :source (resource "sound/thopterblades.ogg")
                               :type :sample)
                :loop t :volume 80))
    (subscribe (game-keys screen) (game-quit screen))
    (subscribe (game-keys screen) (game-pause screen))
    (iter (for player in (game-players (game screen))) (for i from 0)
          (with n = (length (game-players (game screen))))
          (let* ((body-image (make-image "~a~a-~a" :thopter (mod i 4) :body))
                 (blade-anim (make-anim "~a~a-~a" :thopter (mod i 4) :blades))
                 (primary-weapon (make-instance 'weapon
                                                :weapon-class *spread-weapon*))
                 ;(primary-weapon (make-instance 'chaingun-weapon
                 ;                  ;:max-spread 1/4
                 ;                  :max-spread 1
                 ;                  :offset #c(-33 -22)
                 ;                  :size (size blade-anim)
                 ;                  :weapon-type :straight))
                ;                   :offset (complex 
                ;                            (x (/ (size body-image) 2)) 0)))
                ; (primary-weapon2 (make-instance 'chaingun-weapon
                ;                   :max-spread 1/4
                ;                   :offset (complex 
                ;                           (- (x (/ (size body-image) 2))) 0)))
                 (secondary-weapon (make-instance
                                    'weapon
                                    :weapon-class *missile-weapon*))
                 (thopter (make-instance
                           'thopter
                           :host player
                           :parent root
                           :offset (- (complex (* (x size) (/ (+ i 1/2) n))
                                               (* (y size) 3/4))
                                      (/ (size body-image) 2))
                           :image body-image
                           :primary-weapon primary-weapon
                           ;:primary-weapon2 primary-weapon2
                           :secondary-weapon secondary-weapon
                           :health 16)))
            (attach thopter primary-weapon)
            ;(attach thopter primary-weapon2)
            (attach thopter secondary-weapon)
            (make-instance
             'sprite
             :parent thopter
             :offset #c(-33 -22)
             :depth -1
             :image blade-anim)
            (subscribe (game-keys screen) thopter)
            (incf (players-left (players-left screen)))))
    (let* ((tile-names '(:forest-0 :forest-1 :forest-2 :forest-3
                         :forest-4 :forest-5 :forest-6))
           (tiles (iter (for name in tile-names) (collect (make-image name))))
           (num-tiles (length tiles))
           (tile-size (size (first tiles))))
      (iter (for x from 0 below (x size) by (x tile-size))
            (iter (for y from 0 below (y size) by (y tile-size))
                  (make-instance 'tile
                                 :parent root
                                 :offset (complex x y)
                                 :veloc #c(0 1)
                                 :image (nth (mt19937:random num-tiles)
                                             tiles)
                                 :depth 100))))))


(defmethod activate :after ((screen thopter-play-screen))
  (iter (for i in-vector (children (game-root screen)))
        (when (typep i 'thopter)
          (stop-all i nil)))
  ;; Now is a relatively good time to do garbage collection.
  (gc :full t))

(defmethod initialize-instance :after ((screen thopter-menu-screen) &key)
  (let* ((root (game-root screen))
         (size (size root)))
    (setf (game-sheet screen) (load-sheet (resource "disp/thopter-screen.png"))
          (game-start screen) (make-instance 'start-controller :parent root)
          (game-quit screen) (make-instance 'quit-controller :parent root))
    (make-instance 'sprite :image (make-image :title) :depth 1 :parent root)
    (let* ((paragraph '("Concept by Elliott Slaughter and Douglas Martin"
                        "Engine by Elliott Slaughter and Michael Matthews"
                        "Art by Peter Balazs"
                        "Music is Oslodum 2004 by DJ Dolores"
                        ""
                        "Website: http://code.google.com/p/blackthorn-engine/"))
           (images (iter (for text in paragraph)
                         (collect (make-text text (game-font (game screen))))))
          (offset #c(20 570))
          (height (y (size (first images)))))
      (iter (for image in images)
            (for y from (y offset) by height)
            (make-instance 'sprite :offset (+ (x offset) (complex 0 y))
                           :parent root :image image)))
    (let* ((paragraph '("Controls:"
                        ""
                        "Move    => Arrow keys/IJKL/WASD"
                        "Shoot   => Space bar"
                        "Missile => Ctrl/Alt"
                        "Speed   => Shift"
                        "Pause   => Tab"
                        "Quit    => Escape"
                        ""
                        "Press space bar to start."))
           (images (iter (for text in paragraph)
                         (collect (make-text text (game-font (game screen))))))
          (offset #c(620 470))
          (height (y (size (first images)))))
      (iter (for image in images)
            (for y from (y offset) by height)
            (make-instance 'sprite :offset (+ (x offset) (complex 0 y))
                           :parent root :image image)))
    (subscribe (game-keys screen) (game-quit screen))
    (subscribe (game-keys screen) (game-start screen))))


(defmethod initialize-instance :after ((screen thopter-item-screen) &key)
  (let* ((root (game-root screen))
         (size (size root)))
    (setf (game-sheet screen) (load-sheet (resource "disp/thopter-item.png"))
          (game-resume screen) (make-instance 'resume-controller :parent root)
          (game-quit screen) (make-instance 'quit-controller :parent root))
    (let* ((paragraph '("Game Paused"))
           (images (iter (for text in paragraph)
                         (collect (make-text text (game-font (game screen))))))
          (offset #c(20 20))
          (height (y (size (first images)))))
      (iter (for image in images)
            (for y from (y offset) by height)
            (make-instance 'sprite :offset (+ (x offset) (complex 0 y))
                           :parent root :image image)))
    (let* ((paragraph '("Controls:"
                        ""
                        "Move    => Arrow keys/IJKL/WASD"
                        "Shoot   => Space bar"
                        "Missile => Ctrl/Alt"
                        "Speed   => Shift"
                        "Pause   => Tab"
                        "Quit    => Escape"
                        ""
                        "Press tab to resume game."))
           (images (iter (for text in paragraph)
                         (collect (make-text text (game-font (game screen))))))
          (offset #c(620 20))
          (height (y (size (first images)))))
      (iter (for image in images)
            (for y from (y offset) by height)
            (make-instance 'sprite :offset (+ (x offset) (complex 0 y))
                           :parent root :image image)))
    (subscribe (game-keys screen) (game-quit screen))
    (subscribe (game-keys screen) (game-resume screen))))

(defmethod game-init ((game thopter-game) &key player players &allow-other-keys)
  (setf (game-player game) player
        (game-players game) players
        (game-font game) (make-font :font-10x20)
        (game-menu-screen game) (make-instance 'thopter-menu-screen :game game)
        (game-item-screen game) (make-instance 'thopter-item-screen :game game)
        (game-play-screen game) (make-instance 'thopter-play-screen :game game))
  (activate (game-menu-screen game))
  (play
   (make-instance
    'sample :name :music :source (resource "sound/music.mp3") :type :music)
   :loop t :volume 80))

(defmethod game-update :after ((screen thopter-menu-screen))
  (let ((s "Thopter"))
    (set-caption s s)))

(defmethod game-update :after ((screen thopter-item-screen))
  (let ((s "Thopter"))
    (set-caption s s)))

(defmethod game-update :after ((screen thopter-play-screen))
  (let* ((thopter (iter (for i in-vector (children (game-root screen)))
                        (when (and (typep i 'thopter)
                                   (eql (event-host i) (game-player *game*)))
                          (return i))))
         (anyone-alive
          (or thopter
              (iter (for i in-vector (children (game-root screen)))
                    (when (typep i 'thopter)
                      (return i)))))
         (s (if thopter
                (format
                 nil "wave: ~a, health: ~a, firepower: ~a, ammo: ~a, missiles: ~a, speed-time: ~a, fps: ~,2f"
                 (level (game-wave screen))
                 (health thopter)
                 (ceiling (ammo (primary-weapon thopter))
                          (ammo-deplete-rate
                           (weapon-class (primary-weapon thopter))))
                 (ammo (primary-weapon thopter))
                 (ammo (secondary-weapon thopter))
                 (speed-boost thopter)
                 (sdl:average-fps))
                (if anyone-alive
                    (format
                     nil "Congrats! You made it to wave ~a! (Waiting for other players to finish...)"
                     (level (game-wave screen)))
                    (format
                     nil "Congrats! You made it to wave ~a! (Returning to menu in 5 seconds...)"
                     (level (game-wave screen)))))))
    (set-caption s s))

  (when (and (zerop (iter (for i in-vector (children (game-root screen)))
                          (when (typep i 'enemy) (count i))))
             (not (timer (game-wave screen))))
    (setf (timer (game-wave screen)) 120)))

(defmethod initialize-instance :after ((start start-controller) &key)
  (bind-key-down start :sdl-key-space #'(lambda (s e)
                                          (setup-game (game-play-screen *game*))
                                          (activate
                                           (game-play-screen *game*)))))

(defmethod initialize-instance :after ((pause pause-controller) &key)
  (bind-key-down pause :sdl-key-tab #'(lambda (s e)
                                        (activate
                                         (game-item-screen *game*)))))

(defmethod initialize-instance :after ((resume resume-controller) &key)
  (bind-key-down resume :sdl-key-tab #'(lambda (s e)
                                         (activate
                                          (game-play-screen *game*)))))

(defmethod initialize-instance :after ((quit quit-controller) &key)
  #+darwin
  (progn
    (bind-key-down quit :sdl-key-q
              #'(lambda (q e)
                  (declare (ignore q e))
                  (when (intersection '(:sdl-key-mod-lmeta :sdl-key-mod-rmeta)
                                      (event-mod-key e))
                    (quit)))))
  (bind-key-down quit :sdl-key-escape
                 #'(lambda (q e) (declare (ignore q e)) (quit))))

(defun spawn-wave (wave count health ammo missiles missile-chance
                   enemy-type)
  (let* ((root (game-root *game*))
         (root-size (size root)))
    (loop for i from (1+ (floor count -2)) to (floor count 2)
       do (let* ((primary-weapon (make-instance
                                  'weapon
                                  :weapon-class *enemy-spread-weapon*
                                  :ammo ammo))
                 (secondary-weapon (make-instance
                                    'weapon
                                    :weapon-class *enemy-missile-weapon*
                                    :ammo missiles))
                 (enemy (make-instance enemy-type
                                       :parent root
                                       :depth 1
                                       :health health
                                       :missile-chance missile-chance
                                       :difficulty (floor wave 5)
                                       :primary-weapon primary-weapon
                                       :secondary-weapon secondary-weapon)))
            (with-slots (offset size veloc) enemy
              (attach enemy primary-weapon)
              (attach enemy secondary-weapon)
              (setf offset
                    (complex (+ (/ (x root-size) 2) (* i (x size) 2))
                             (* (y size) -2))))))))

(defmethod alarm ((wave wave-controller) event)
  (with-slots (level) wave
    (incf level)
    (if (zerop (mod level 10))
        (spawn-wave level (truncate level 10) (+ 100 (* level 10)) (* level 4)
                    (truncate level 2) (min 1d0 (* level 0.024)) 'enemy-boss)
        (spawn-wave level (+ 2 level) (min (ceiling level 3) 8)
                    (max 1 (ceiling level 3)) 0
                    (min 1d0 (* level 0.11)) 'enemy-ship))))

(defmethod alarm ((controller background-controller) event)
  (with-slots (parent timer) controller
    (let* ((size (size parent))
           (tile-names '(:forest-0 :forest-1 :forest-2 :forest-3
                         :forest-4 :forest-5 :forest-6))
           (tiles (iter (for name in tile-names)
                        (collect (make-image name))))
           (num-tiles (length tiles))
           (tile-size (size (first tiles))))
      (iter (for x from 0 below (x size) by (x tile-size))
            (let ((y (1+ (- (y tile-size)))))
              (make-instance 'tile
                             :parent parent
                             :offset (complex x y)
                             :veloc #c(0 1)
                             :image (nth (mt19937:random num-tiles)
                                         tiles)
                             :depth 100)))
      (setf timer (1- (y tile-size))))))

(defmethod (setf players-left) :after (value (left players-left-controller))
  (when (<= value 0)
    (stop :channel (game-sound (game-screen *game*)))
    (when (missile-lock (game-screen *game*))
      (stop :channel (missile-lock (game-screen *game*))))
    (setf (timer left) 150)))

(defmethod alarm ((left players-left-controller) event)
  (declare (ignore event))
  (activate (game-menu-screen *game*)))

;; For interactive use:
(defun thopter ()
  (let ((*game* (make-instance 'thopter-game)))
    (main :exit-when-done nil)))

;; For non-interactive use:
(defvar *game* (make-instance 'thopter-game))
