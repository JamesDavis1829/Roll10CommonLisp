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

(defmacro make-weapon (weapon-name agi-req int-req str-req range weight wield &body body)
  `(progn
    (defstruct (,weapon-name (:include item
                                      (agility-requirement ,agi-req)
                                      (intelligence-requirement ,int-req)
                                      (strength-requirement ,str-req)
                                      (category "weapon")
                                      (name (string-capitalize (substitute #\space #\- (string ',weapon-name))))
                                      (range ,range)
                                      (weight ,weight)
                                      (wield ,wield))))
    (defmethod use-item ((user rpg-character) (target rpg-character) (item ,weapon-name))
      ,@body)))

(defstruct (test-dagger (:include item (agility-requirement 6) (intelligence-requirement 6))))

(make-weapon dagger 6 6 6 1 "light" 1
             (format t "~A ~A ~A" user target item))
