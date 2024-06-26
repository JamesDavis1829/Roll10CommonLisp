(defparameter *items* ())

(defstruct item
  agility-requirement
  intelligence-requirement
  strength-requirement
  category
  description
  name
  range
  weight
  wield
  roll-function)

(defmethod perform-action ((user rpg-character) (target rpg-character) (usable-item item))
  (funcall (item-roll-function usable-item) user target))

(defmethod armor-mod ((c rpg-character))
  (let* ((equiped-armor (remove-if-not (lambda (x) (equal (item-category x) "armor")) (rpg-character-equipment c)))
         (armor-values (mapcar (lambda (armor-item) (first (perform-action c c armor-item))) equiped-armor))
         (total-armor (apply '+ armor-values)))
    (list total-armor (write-to-string total-armor) "ARMOR")))

(defmethod can-equip ((user rpg-character) (item item))
  (and (>= (rpg-character-agility user)      (item-agility-requirement item))
       (>= (rpg-character-intelligence user) (item-intelligence-requirement item))
       (>= (rpg-character-strength user)     (item-strength-requirement item))))

(defmethod unequip ((user rpg-character) item)
  (setf (rpg-character-inventory user) (remove item (rpg-character-inventory user) :count 1)))

(defmethod equip ((user rpg-character) item)
  (when (can-equip user item)
    (push (rpg-character-inventory user) item)))

(defmacro define-item (item-name agi-req int-req str-req range weight wield category &body body)
  (let ((function-name (read-from-string (format nil "make-~a" item-name))))
  `(progn
    (defun ,function-name ()
      (make-item
       :agility-requirement ,agi-req
       :intelligence-requirement ,int-req
       :strength-requirement ,str-req
       :category ,category
       :name (format-name ',item-name)
       :range ,range
       :weight ,weight
       :wield ,wield
       :roll-function (lambda (user target)
                              (declare (ignorable user))
                              (declare (ignorable target))
                              ,@body)))
    (pushnew (,function-name) *items* :test (lambda (x y) (equal (item-name x) (item-name y)))))))

(defun random-item ()
  (nth (random (length *items*)) *items*))

(define-item dagger 6 6 6 1 "light" 1 "weapon"
  (gen-combat-roll 1 (roll-die 1 10) (roll-die 1 6) (agi-mod user)))

(define-item short-sword 10 9 10 1 "light" 1 "weapon"
  (gen-combat-roll 1 (roll-die 1 10) (roll-die 1 6) (str-mod user) (agi-mod user)))

(define-item warhammer 10 8 12 1 "heavy" 2 "weapon"
  (gen-combat-roll 1 (roll-die 1 10) (roll-die 1 8) (str-mod user) (agi-mod user)))

(define-item longsword 10 9 10 1 "heavy" 1 "weapon"
  (gen-combat-roll 1 (roll-die 1 10) (roll-die 1 8) (str-mod user) (agi-mod user)))

(define-item shield 8 6 8 0 "medium" 1 "armor"
  (gen-combat-roll 0 (const 2)))

(define-item gambeson 6 6 8 0 "light" 0 "armor"
  (gen-combat-roll 0 (const 1)))

(define-item chain-shirt 6 4 8 0 "medium" 0 "armor"
  (gen-combat-roll 0 (const 3)))

(define-item natural-armor 0 0 0 0 "light" 0 "armor"
  (gen-combat-roll 0 (dur-mod user)))

(define-item weapon-guard 0 0 0 0 "light" 1 "armor"
  (gen-combat-roll 0 (const 1)))

(define-item scale-mail 0 0 12 0 "heavy" 0 "armor"
  (gen-combat-roll 0 (const 4)))

(define-item war-axe 8 8 12 1 "heavy" 1 "weapon"
  (gen-combat-roll 1 (roll-die 1 10) (roll-die 1 8) (str-mod user)))

(define-item zweihander 10 9 10 2 "medium" 2 "weapon"
  (gen-combat-roll 1 (roll-die 1 10) (roll-die 1 8) (str-mod user) (agi-mod user)))

(define-item whip 11 9 8 3 "light" 1 "weapon"
  (gen-combat-roll 1 (roll-die 1 10) (roll-die 1 4) (agi-mod user)))

(define-item short-spear 9 6 9 1 "medium" 1 "weapon"
  (gen-combat-roll 1 (roll-die 1 10) (roll-die 1 6) (str-mod user)))

(define-item spear 9 6 9 2 "medium" 2 "weapon"
  (gen-combat-roll 1 (roll-die 1 10) (roll-die 1 8) (str-mod user) (agi-mod user)))

(define-item polearm 11 9 11 3 "medium" 2 "weapon"
  (gen-combat-roll 1 (roll-die 2 10) (str-mod user)))

(define-item short-bow 8 8 10 20 "light" 2 "weapon"
  (gen-combat-roll 1 (roll-die 1 10) (roll-die 1 6) (agi-mod user)))

(define-item long-bow 8 8 12 20 "medium" 2 "weapon"
  (gen-combat-roll 1 (roll-die 1 10) (roll-die 1 8) (agi-mod user)))

(define-item crossbow 10 9 10 20 "medium" 2 "weapon"
  (gen-combat-roll 1 (roll-die 1 10) (roll-die 1 8) (agi-mod user)))

(define-item heavy-crossbow 10 9 11 30 "heavy" 2 "weapon"
  (gen-combat-roll 1 (roll-die 1 10) (roll-die 2 6) (agi-mod user)))

(define-item hand-crossbow 10 9 8 10 "light" 1 "weapon"
  (gen-combat-roll 1 (roll-die 1 10) (roll-die 1 4) (agi-mod user)))

(define-item club 6 4 10 1 "medium" 1 "weapon"
  (gen-combat-roll 1 (roll-die 1 10) (roll-die 1 4) (str-mod user)))

(define-item brigadine 8 6 11 0 "heavy" 0 "armor"
  (gen-combat-roll 0 (const 4)))

(define-item plate 8 6 12 0 "heavy" 0 "armor"
  (gen-combat-roll 0 (const 5)))

(define-item leather-armor 9 6 9 0 "light" 0 "armor"
  (gen-combat-roll 0 (const 2)))

(define-item half-plate 8 6 11 0 "medium" 0 "armor"
  (gen-combat-roll 0 (const 3)))

(define-item buckler 9 8 7 0 "light" 1 "armor"
  (gen-combat-roll 0 (const 1)))

(define-item greatshield 8 6 12 0 "heavy" 1 "armor"
  (gen-combat-roll 0 (const 3)))
