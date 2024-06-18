(load "utilities.lisp")
(load "base.lisp")

(defparameter *actions* ())

(defstruct action
  situation
  (include-base-dice t)
  (apply-to-all t)
  description
  name
  type)

(defmacro define-action (name situation description type include-base-dice apply-to-all &body body)
  `(progn
    (defstruct (,name (:include action
                                (name (format-name ',name))
                                (include-base-dice ,include-base-dice)
                                (apply-to-all ,apply-to-all)
                                (description ,description)
                                (type ,type)
                                (situation ,situation))))
    (defmethod perform-action ((user rpg-character) (target rpg-character) (spell ,name))
      ,@body)
    (pushnew (make-instance-from-name ',name) *actions* :test (lambda (x y) (equal (action-name x) (action-name y))))))

(define-action defend "combat" "Take the defend action." "reaction" t t
  (gen-combat-roll 1 (roll-die 1 10) (armor-mod user) (agi-mod user)))

(define-action initiative "standard" "Roll for turn order." "standard" t t
  (gen-combat-roll 0 (roll-die 1 10) (agi-mod user)))

(define-action dash "combat" "Take another move action." "standard" nil t
  (gen-combat-roll 1 (const 0)))

(define-action compose "combat" "Rest and regains stamina." "standard" nil t
  (setf (rpg-character-cur-sta user) (min (rpg-character-stamina user) (+ 2 (rpg-character-cur-sta user))))
  (const 0))

(define-action taunt "combat" "As a reaction expend one stamina to add your int mod to the defense roll of another creature." "standard" nil nil
  (gen-combat-roll 1 (int-mod user)))

(define-action restrain "combat" "As an action attempt to restrain a creature. It's speed becomes 0." "attack" t t
  (gen-combat-roll 1 (roll-die 1 10) (str-mod user)))

(define-action take-down "combat" "You target a creature with melee range and attempt to knock that creature to the ground. Make an opposed strength check. If successful the target suffers from the prone condition." "attack" t t
  (gen-combat-roll 2 (roll-die 1 10) (str-mod user)))

(define-action study "combat" "As an action study the characteristics of a creature in combat. Add your Int and Ins modifiers to your next attack against that creature." "standard" nil nil
  (gen-combat-roll 0 (int-mod user) (ins-mod user)))

(define-action arcane-shield "combat" "A spheroid of arcane energy manifests around the caster protecting them from harm." "reaction" nil nil
  (gen-combat-roll 1 (roll-die 1 10) (int-mod user)))

(define-action submission "combat" "While grappling a creature attempt to break it's bones or crush an organ. If successful the targets hp pool is reduced by 5 until magically healed or 6 weeks elapses." "attack" t nil
  (gen-combat-roll 4 (roll-die 1 10) (str-mod user) (agi-mod user)))

(define-action choke "combat" "Attempt to prevent a creature from breathing. Creatures who do not breath are immune. If successful the creature cannot rest on their next turn." "attack" t nil
  (gen-combat-roll 4 (roll-die 1 10) (str-mod user) (agi-mod user)))

(define-action unarmed-attack "combat" "Punch with your fists or other natural weapons." "attack" nil nil
  (gen-combat-roll 1 (roll-die 1 10) (str-mod user) (agi-mod user)))
