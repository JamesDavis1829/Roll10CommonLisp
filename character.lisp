(load "utilities.lisp")
(load "dice.lisp")

(defstruct rpg-character
  (agility *base-stat*)
  (intelligence *base-stat*)
  (stamina *base-stat*)
  (strength *base-stat*)
  (durability *base-stat*)
  (insight *base-stat*)

  (cur-hp *base-stat*)
  (cur-sta *base-stat*)
  actions
  (caster-type "none")
  equipment
  feats
  inventory
  level
  name
  spells)

(defmethod damage ((character rpg-character) x &key (stamina-only nil))
  (let* ((char-sta (rpg-character-cur-sta character))
        (damaged-sta (- char-sta x)))
    (if stamina-only
      (setf (rpg-character-cur-sta character) (max 0 ))
      (if (< damaged-sta 0)
        (progn
         (setf (rpg-character-cur-sta character) 0)
         (setf (rpg-character-cur-hp character) (max 0 (+ damaged-sta (rpg-character-cur-hp character)))))
        (setf (rpg-character-cur-sta character) (max 0 damaged-sta))))
    character))

(defmethod long-rest ((character rpg-character))
  (setf (rpg-character-cur-hp character) (rpg-character-durability character))
  (setf (rpg-character-cur-sta character) (rpg-character-stamina character))
  character)

(defmethod is-dead ((character rpg-character))
  (when (= (rpg-character-cur-hp character) 0)
    t))

(defmethod agi-mod ((c rpg-character))
  (let ((mod (max 0 (- (rpg-character-agility c) *base-stat*))))
    (list mod (format nil "~A" mod) "AGI")))

(defmethod str-mod ((c rpg-character))
  (let ((mod (max 0 (- (rpg-character-strength c) *base-stat*))))
    (list mod (format nil "~A" mod) "STR")))

(defmethod dur-mod ((c rpg-character))
  (let ((mod (max 0 (- (rpg-character-durability c) *base-stat*))))
    (list mod (format nil "~A" mod) "DUR")))

(defmacro define-rpg-character (name level agi int sta str dur ins &key actions equipment feats inventory spells caster-type)
  (progn
   `(defstruct (,name (:include rpg-character
                                (agility ,agi)
                                (intelligence ,int)
                                (stamina ,sta)
                                (strength ,str)
                                (durability ,dur)
                                (insight ,ins)
                                (cur-hp ,dur)
                                (cur-sta ,sta)
                                (actions ,actions)
                                (caster-type ,caster-type)
                                (equipment ,equipment)
                                (feats ,feats)
                                (inventory ,inventory)
                                (level ,level)
                                (name (format-name ',name))
                                (spells ,spells))))))
