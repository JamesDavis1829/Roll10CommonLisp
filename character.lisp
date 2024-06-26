(defparameter *rpg-characters* ())

(defmacro define-rpg-character (name level caster-type agi int sta str dur ins &key (actions ()) (equipment ()) (feats ()) (inventory ()) (spells ()))
  `(progn
   (defstruct (,name (:include rpg-character
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
                                (spells ,spells))))
   (pushnew ',name *rpg-characters*)))

(defun character-action-p (action)
  (cond
    ((item-p action) (not (equal (item-category action) "armor")))
    ((spell-p action) t)
    ((action-p action) (not (equal "reaction" (action-type action))))
    (t nil)))

(defmethod get-actions ((c rpg-character))
  (let* ((equipment-actions (remove-if-not #'character-action-p (rpg-character-equipment c)))
         (spell-actions (remove-if-not #'character-action-p (rpg-character-spells c)))
         (action-actions (remove-if-not #'character-action-p (rpg-character-actions c))))
    (concatenate 'list equipment-actions spell-actions action-actions)))

(defun random-character ()
  (make-instance-from-name (nth (random (length *rpg-characters*)) *rpg-characters*)))

(define-rpg-character golem 2 "none" 10 6 19 13 19 10
  :actions (list (make-unarmed-attack))
  :equipment (list (make-natural-armor)))

(define-rpg-character fire-sprite 1 "full" 13 12 13 6 8 12
  :spells (list (make-lesser-fireball) (make-arcane-compose) (make-fireball)))

(define-rpg-character bard 2 "half" 12 13 13 8 10 10
  :equipment (list (make-gambeson) (make-short-sword) (make-dagger))
  :actions (list (make-unarmed-attack))
  :spells (list (make-insult-to-injury) (make-multiplicity)))

(define-rpg-character crusader 2 "quarter" 11 10 12 12 11 12
  :equipment (list (make-warhammer) (make-chain-shirt))
  :spells (list (make-strength-of-the-consecrated) (make-lesser-heal)))

(define-rpg-character prophet 2 "full" 12 12 13 8 8 13
  :spells (list (make-pestilence) (make-greater-heal) (make-lesser-heal) (make-fireball)))

(define-rpg-character artificer 3 "half" 12 15 24 9 16 11
  :equipment (list (make-chain-shirt) (make-dagger))
  :spells (list (make-arcane-compose) (make-fireball)))

(define-rpg-character slinky-automata 0 "none" 13 6 10 13 2 10
  :actions (list (make-unarmed-attack)))

(define-rpg-character human-rogue-l2 2 "none" 11 10 13 10 10 10
  :equipment (list (make-chain-shirt) (make-dagger))
  :inventory (list (make-dagger)))

(define-rpg-character human-crusader-l2 2 "half" 11 10 12 10 10 10
  :equipment (list (make-scale-mail) (make-warhammer))
  :spells (list (make-lesser-heal) (make-strength-of-the-consecrated)))

(define-rpg-character human-brawler-l2 2 "none" 12 9 10 12 11 9
  :equipment (list (make-scale-mail) (make-longsword))
  :actions (list (make-choke) (make-submission)))

(define-rpg-character human 0 "none" 8 8 10 8 8 8)
(define-rpg-character dwarven 0 "none" 8 8 8 9 9 8)
(define-rpg-character halfling 0 "none" 9 8 8 8 8 8)
(define-rpg-character orc 0 "none" 8 8 8 8 10 8)
(define-rpg-character elf 0 "none" 9 8 8 8 8 9)
