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

(defmacro gen-mod (fun-name slot-name)
  `(defmethod ,fun-name ((c rpg-character))
     (let ((mod (max 0 (- (funcall (read-from-string (format nil "rpg-character-~a" ',slot-name)) c) *base-stat*))))
       (list mod (write-to-string mod) (subseq (string-upcase ',slot-name) 0 3)))))

(gen-mod agi-mod agility)
(gen-mod str-mod strength)
(gen-mod dur-mod durability)
(gen-mod sta-mod stamina)
(gen-mod ins-mod insight)
(gen-mod int-mod intelligence)

(defmethod caster-mod ((c rpg-character))
  (let ((caster-type (rpg-character-caster-type c)))
    (concatenate 'list
                 (cond
                   ((equal caster-type "quarter") (list 1 "1"))
                   ((equal caster-type "half") (list 2 "2"))
                   ((equal caster-type "full") (list 3 "3"))
                   (t (list 0 "0"))) '("CASTERMOD"))))

(defmethod armor-mod ((c rpg-character))
  (let* ((equiped-armor (remove-if-not (lambda (x) (equal (item-category x) "armor")) (rpg-character-equipment c)))
         (armor-values (mapcar (lambda (armor-item) (first (perform-action c c armor-item))) equiped-armor))
         (total-armor (apply '+ armor-values)))
    (list total-armor (write-to-string total-armor) "ARMOR")))

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

(defmacro gen-combat-roll (sta-cost &rest rolls)
  `(if (>= (- (rpg-character-cur-sta user) ,sta-cost) 0)
     (let* ((rolls (list ,@rolls))
            (roll-damage (max 0 (apply #'+ (mapcar #'first rolls)))))
       (decf (rpg-character-cur-sta user) ,sta-cost)
       (damage target roll-damage)
       (list roll-damage (format nil "~{~A~^ + ~} = ~A" (mapcar #'second rolls) roll-damage) (format nil "~{~A~^ + ~}" (mapcar #'third rolls))))
     nil))
