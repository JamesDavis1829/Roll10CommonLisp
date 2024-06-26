(asdf:defsystem
 "roll10-cl"
 :components ((:file "utilities")
              (:file "base")
              (:file "dice")
              (:file "items")
              (:file "actions")
              (:file "spells")
              (:file "character")))

(asdf:defsystem
 "roll10-cl/gauntlet"
 :components ((:file "utilities")
              (:file "base")
              (:file "dice")
              (:file "items")
              (:file "actions")
              (:file "spells")
              (:file "character")
              (:file "gauntlet")))
