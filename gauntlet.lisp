(load "character.lisp")

;Pick one of the predefined characters (done)
;Then the system will randomly make a gauntlet of 5 other characters to fight (done)
;The battle ends when either characters hp reaches 0 (done)
;In between battles the player can choose on of three random items and change their equipment
;The player wins if they beat all 5 of the characters (done)

(defun numbered-character-list (lst)
  (let* ((character-names (mapcar (lambda (x) (rpg-character-name x)) lst))
         (options (loop for index from 1 for item in character-names collect (format nil "~A. ~A" index item))))
    options))

(defun select-character ()
  (let ((choices (mapcar #'make-instance-from-name *rpg-characters*)))
    (format t "Choose a character: ~%~{~A ~%~}" (numbered-character-list choices))
    (let ((option (parse-integer (read-line) :junk-allowed t)))
      (if option
        (nth (1- option) choices)
        (progn
         (format t "That is not a valid option.")
         (select-character))))))

(defun get-action-name (action)
  (cond
    ((action-p action) (action-name action))
    ((item-p action) (item-name action))
    ((spell-p action) (spell-name action))
    (t "Unkown")))

(defun choose-action (player)
  (let* ((actions (get-actions player))
         (action-names (mapcar #'get-action-name actions))
         (options (loop for index from 1 for item in action-names collect (format nil "~A. ~A" index item))))
    (labels ((choose ()
                     (format t "~&Choose an action: ~%~{~A ~%~}" options)
                     (let ((option (parse-integer (read-line) :junk-allowed t)))
                       (if option
                         (nth (1- option) actions)
                         (progn
                          (format t "That is not a valid option.")
                          (choose))))))
            (choose))))

(defun print-results (user target action result &key is-ai)
  (if (equal "Compose" (get-action-name action))
    (format t "~&~:[You~;NPC~] used ~A. They have ~A STA and ~A HP remaining."
            is-ai
            (get-action-name action)
            (rpg-character-cur-sta user)
            (rpg-character-cur-hp user))
    (format t "~&~:[You~;NPC~] hit the ~A for ~A (~A) damage with a ~a. They have ~A STA and ~A HP remaining."
            is-ai
            (rpg-character-name target)
            (first result)
            (second result)
            (get-action-name action)
            (rpg-character-cur-sta target)
            (rpg-character-cur-hp target))))

(defun perform-player-action (player opponent)
  (let* ((action (choose-action player))
         (result (perform-action player opponent action)))
    (print-results player opponent action result)))


(defun score-action (ai opponent action)
  (let* ((ai-copy (copy-structure ai))
         (opponent-copy (copy-structure opponent))
         (result (perform-action ai-copy opponent-copy action)))
    (if result
      (/ (+ (rpg-character-cur-hp ai-copy) (rpg-character-cur-sta ai-copy) (first result))
         (max (+ (rpg-character-cur-hp opponent-copy) (rpg-character-cur-sta opponent-copy)) 1))
      0)))

(defun choose-ai-action (ai opponent)
  (cond
    ((= 0 (rpg-character-cur-sta ai))
     (make-compose))
    (t
     (let* ((actions (get-actions ai))
            (scores (mapcar (lambda (x) (score-action ai opponent x)) actions))
            (max-score (loop for score in scores maximize score)))
       (nth (position max-score scores) actions)))))

(defun perform-ai-action (ai opponent)
  (let* ((action (choose-ai-action ai opponent))
         (result (perform-action ai opponent action)))
    (print-results ai opponent action result :is-ai t)))


(defun battle (player opponent opponents)
  (let ((opponent-name (rpg-character-name opponent)))
    (cond
      ((= (rpg-character-cur-hp player) 0)
       (format t "~&You were defeated by ~A. You lose." opponent-name))
      ((= (rpg-character-cur-hp opponent) 0)
       (format t "~&You defeated ~A." opponent-name)
       (let ((next-opponent (first opponents))
             (rest-opponents (rest opponents)))
         (if next-opponent
           (progn
            (format t "~&Your next opponent is ~A." (rpg-character-name next-opponent))
            (battle player next-opponent rest-opponents))
           (format t "~&You have defeated all your opponents. You are victorious."))))
      (t
       (perform-player-action player opponent)
       (when (not (= (rpg-character-cur-hp opponent) 0))
         (perform-ai-action opponent player))
       (battle player opponent opponents)))))

(defun gauntlet ()
  (when (uiop:file-exists-p "plugins.lisp")
    (load "plugins.lisp"))

  (let ((player (select-character))
        (opponents (loop for x below 5 collect (random-character))))
    (format t "You are a ~A and must face the following opponents to win.~%~{~A ~%~}"
            (rpg-character-name player)
            (numbered-character-list opponents))
    (format t "~&Your first opponent is ~A." (rpg-character-name (first opponents)))
    (battle player (first opponents) (rest opponents))))
