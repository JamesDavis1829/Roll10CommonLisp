(load "character.lisp")

;Pick one of the predefined characters
;Then the system will randomly make a gauntlet of 5 other characters to fight
;The battle ends when either characters hp reaches 0
;In between battles the player can choose on of three random items and change their equipment
;The player wins if they beat all 5 of the characters

(defun gauntlet ()
  (when (uiop:file-exists-p "plugins.lisp")
    (load "plugins.lisp"))

  )
