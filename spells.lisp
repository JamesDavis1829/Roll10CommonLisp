(load "utilities.lisp")
(load "base.lisp")
(defparameter *spells* ())

(defstruct spell
  name
  insight-requirement
  intelligence-requirement
  range
  aoe
  school
  tier
  description
  roll-function)

(defmethod can-equip ((user rpg-character) (spell spell))
  (and (>= (rpg-character-insight user)      (spell-insight-requirement spell))
       (>= (rpg-character-intelligence user) (spell-intelligence-requirement spell))))

(defmethod perform-action ((user rpg-character) (target rpg-character) (spell spell))
  (funcall (spell-roll-function spell) user target))

(defmacro define-spell (name ins-req int-req range aoe school tier desc &body body)
  `(define-rollable ,name *spells* spell-name
     (make-spell
      :name (format-name ',name)
      :insight-requirement ,ins-req
      :intelligence-requirement ,int-req
      :range ,range
      :aoe ,aoe
      :school ,school
      :tier ,tier
      :description ,desc
      :roll-function (lambda (user target)
                             (declare (ignorable user))
                             (declare (ignorable target))
                             ,@body))))

(define-spell chill-wind 8 12 20 3 "arcane" 1 "Stamina damage only. If this attack reduces stamina to 0 the targets next rest action restores half the normal stamina"
  (gen-combat-roll (- 6 (first (caster-mod user))) (roll-die 2 8) (int-mod user)))

(define-spell lesser-fireball 8 12 40 1 "arcane" 0 "A small flash of fire erupts at a point that the caster can see"
  (gen-combat-roll (- 4 (first (caster-mod user))) (roll-die 2 8) (int-mod user)))

(define-spell arcane-compose 8 0 0 0 "arcane" 0 "Reduces HP to gain Stamina"
  (let ((cur-hp (rpg-character-cur-hp user)))
    (when (> (- cur-hp 1) 0)
      (setf (rpg-character-cur-sta user) (min (rpg-character-stamina user) (+ 4 (rpg-character-cur-sta user))))
      (setf (rpg-character-cur-hp user) (max 0 (- (rpg-character-cur-hp user) 1)))
      (const 0))))

(define-spell lesser-heal 10 8 0 0 "divine" 0 "Reduces stamina to gain hp"
  (let ((stamina-cost (- 2 (first (caster-mod user)))))
    (damage user stamina-cost)
    (setf (rpg-character-cur-hp target) (min (rpg-character-durability target) (+ (rpg-character-cur-hp target) 1)))
    (const 1)))

(define-spell fireball 8 12 40 9 "arcane" 1 "A large flash of flame erupts at a point the caster can see."
  (gen-combat-roll (- 5 (first (caster-mod user))) (roll-die 3 8) (int-mod user)))

(define-spell multiplicity 10 12 3 0 "arcane" 1 "An exact copy of the bard is created in a space they can see within range. Additional copies may be created by paying the stamina cost again. The caster may speak through the duplicates and move them on the casters turn. Each duplicate has 1 dur and 1 sta. The caster must expend 1 stamina per turn per duplicate or the duplicates vanish."
  (gen-combat-roll (- 2 (first (caster-mod user))) (const 0)))

(define-spell insult-to-injury 11 11 20 0 "arcane" 0 "The caster targets a creature which they believe can hear them. If the target cannot hear them the spell fails: The target makes an opposed skill check using int or ins. If the target fails the check they take 1 hp damage."
  (gen-combat-roll (- 2 (first (caster-mod user))) (roll-die 1 10) (int-mod user)))

(define-spell greater-heal 14 10 1 0 "divine" 2 "The caster targets a creature which they believe can hear them. If the target cannot hear them the spell fails: The target makes an opposed skill check using int or ins. If the target fails the check they take 1 hp damage."
  (let ((stamina-cost (- 7 (first (caster-mod user)))))
    (damage user stamina-cost)
    (setf (rpg-character-cur-hp target) (min (rpg-character-durability target) (+ (rpg-character-cur-hp target) 10)))
    (const 10)))

(define-spell strength-of-the-consecrated  11 8 0 0 "divine" 1 "The caster calls upon divine energy and for 1 minute they add their Ins modifier to their strength and agility scores. "
  (gen-combat-roll (- 3 (first (caster-mod user))) (ins-mod user)))

(define-spell pestilence 14 8 20 5 "divine" 2 "The caster calls upon the divine to manifest a swarm of a pestilent creature such as locust or hornets at a point within range. Any creature within 5m of that point is then attacked by the swarm. The caster may continue the spell on subsequent turns by using their action to maintain the spell and expending 2 additional sta per turn. The caster may move the swam up to the casting range each turn. "
  (gen-combat-roll (- 3 (first (caster-mod user))) (roll-die 2 8) (ins-mod user)))

(define-spell divine-aid 12 8 0 0 "divine" 0 "Add your ins modifier to your next skill roll"
  (gen-combat-roll 0 (ins-mod user)))

(define-spell hover 8 13 0 0 "arcane" 2 "The creature may move in 3 dimensions unimpeded by gravity. When the spell ends the creature becomes affected by gravity again. "
  (gen-combat-roll (- 4 (first (caster-mod user)))))
