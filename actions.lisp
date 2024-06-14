(load "utilities.lisp")
(defparameter *actions* nil)

(defstruct action
  situation
  (include-base-dice t)
  (apply-to-all t)
  description
  name
  type)

(defmacro define-action (name situation description type &key (include-base-dice t) (apply-to-all t))
  `(progn
    (defstruct (,name (:include action
                                (name (format-name ',name))
                                (include-base-dice ,include-base-dice)
                                (apply-to-all ,apply-to-all)
                                (description ,description)
                                (type ,type)
                                (situation ,situation))))
    (pushnew (make-instance-from-name ',name) *actions* :test (lambda (x y) (equal (action-name x) (action-name y))))))

(define-action defend "combat" "Take the defend action." "reaction")
(define-action initiative "combat" "Roll for turn order." "standard")
(define-action dash "combat" "Take another move action." "standard" :include-base-dice  nil)
(define-action compose "combat" "Rest and regains stamina." "standard" :include-base-dice nil)
(define-action taunt "combat" "As a reaction expend one stamina to add your int mod to the defense roll of another creature." "standard" :include-base-dice nil :apply-to-all nil)
(define-action restrain "combat" "As an action attempt to restrain a creature. It's speed becomes 0." "attack")
(define-action take-down "combat" "You target a creature with melee range and attempt to knock that creature to the ground. Make an opposed strength check. If successful the target suffers from the prone condition." "attack")
(define-action study "combat" "As an action study the characteristics of a creature in combat. Add your Int and Ins modifiers to your next attack against that creature." "standard" :include-base-dice nil :apply-to-all nil)
(define-action arcane-shield "combat" "A spheroid of arcane energy manifests around the caster protecting them from harm." "reaction" :include-base-dice nil)
(define-action submission "combat" "While grappling a creature attempt to break it's bones or crush an organ. If successful the targets hp pool is reduced by 5 until magically healed or 6 weeks elapses." "attack" :apply-to-all nil)
(define-action choke "combat" "Attempt to prevent a creature from breathing. Creatures who do not breath are immune. If successful the creature cannot rest on their next turn." "attack" :apply-to-all nil)
