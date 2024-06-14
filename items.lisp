(load "utilities.lisp")
(defparameter *items* nil)

(defstruct item
  agility-requirement
  intelligence-requirement
  strength-requirement
  category
  description
  name
  range
  weight
  wield)

(defmethod use-item ((user rpg-character) (target rpg-character) item))

(defmethod can-equip ((user rpg-character) item)
  (and (>= (rpg-character-agility user)      (item-agility-requirement item))
       (>= (rpg-character-intelligence user) (item-intelligence-requirement item))
       (>= (rpg-character-strength user)     (item-strength-requirement item))))

(defmethod unequip ((user rpg-character) item)
  (setf (rpg-character-inventory user) (remove item (rpg-character-inventory user) :count 1)))

(defmethod equip ((user rpg-character) item)
  (when (can-equip user item)
    (push (rpg-character-inventory user) item)))

(defmacro define-item (item-name agi-req int-req str-req range weight wield category &body body)
  `(progn
    (defstruct (,item-name (:include item
                                      (agility-requirement ,agi-req)
                                      (intelligence-requirement ,int-req)
                                      (strength-requirement ,str-req)
                                      (category ,category)
                                      (name (format-name ',item-name))
                                      (range ,range)
                                      (weight ,weight)
                                      (wield ,wield))))
    (defmethod use-item ((user rpg-character) (target rpg-character) (item ,item-name))
      ,@body)
    (pushnew ',item-name *items*)))

(defun random-item ()
  (make-instance-from-name (nth (random (length *items*)) *items*)))

(define-item dagger 6 6 6 1 "light" 1 "weapon")
(define-item short-sword 10 9 10 1 "light" 1 "weapon")
(define-item warhammer 10 8 12 1 "heavy" 2 "weapon")
(define-item shield 8 6 8 0 "medium" 1 "armor")
(define-item gambeson 6 6 8 0 "light" 0 "armor")
(define-item chain-shirt 6 4 8 0 "medium" 0 "armor")
(define-item natural-armor 0 0 0 0 "light" 0 "armor")
(define-item scale-mail 0 0 12 0 "heavy" 0 "armor")
(define-item war-axe 8 8 12 1 "heavy" 1 "weapon")
(define-item zweihander 10 9 10 2 "medium" 2 "weapon")
(define-item whip 11 9 8 1 "light" 1 "weapon")
(define-item short-spear 9 6 9 1 "medium" 1 "weapon")
(define-item spear 9 6 9 2 "medium" 2 "weapon")
(define-item polearm 11 9 11 3 "medium" 2 "weapon")
(define-item short-bow 8 8 10 20 "light" 2 "weapon")
(define-item long-bow 8 8 12 20 "medium" 2 "weapon")
(define-item crossbow 10 9 10 20 "medium" 2 "weapon")
(define-item heavy-crossbow 10 9 11 30 "heavy" 2 "weapon")
(define-item hand-crossbow 10 9 8 10 "light" 1 "weapon")
(define-item club 6 4 10 1 "medium" 1 "weapon")
(define-item brigadine 8 6 11 0 "heavy" 0 "armor")
(define-item plate 8 6 12 0 "heavy" 0 "armor")
(define-item leather-armor 9 6 9 0 "light" 0 "armor")
(define-item half-plate 8 6 11 0 "medium" 0 "armor")
(define-item buckler 9 8 7 0 "light" 1 "armor")
(define-item greatshield 8 6 12 0 "heavy" 1 "armor")
