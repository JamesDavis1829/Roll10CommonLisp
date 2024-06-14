(load "utilities.lisp")
(defstruct rpg-character
  (agility 5)
  (intelligence 5)
  (stamina 5)
  (strength 5)
  (durability 5)
  (insight 5)

  (cur-hp 5)
  (cur-sta 5)
  actions
  (caster-type "none")
  equipment
  feats
  inventory
  level
  name
  spells)

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
