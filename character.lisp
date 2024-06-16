(load "utilities.lisp")
(load "base.lisp")
(load "dice.lisp")
(load "items.lisp")
(load "actions.lisp")
(load "spells.lisp")

(defparameter *rpg-characters* ())

(defmacro define-rpg-character (name level caster-type agi int sta str dur ins &key (actions ()) (equipment ()) (feats ()) (inventory ()) (spells ()))
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
                                (actions
                                 (concatenate 'list ,actions (remove-if-not (lambda (act) (action-apply-to-all act)) *actions*)))
                                (caster-type ,caster-type)
                                (equipment ,equipment)
                                (feats ,feats)
                                (inventory ,inventory)
                                (level ,level)
                                (name (format-name ',name))
                                (spells ,spells))))))

(define-rpg-character golem 2 "none" 10 6 19 13 19 10 :actions (list (make-unarmed-attack)) :equipment (list (make-natural-armor)))
