;;;; breakout.asd

(asdf:defsystem #:breakout
  :description "A Remake of the Classic Game Breakout"
  :author "Jan Tatham <jan@sebity.com>"
  :license "GPL v2"
  :depends-on (#:lispbuilder-sdl
               #:lispbuilder-sdl-ttf
               #:lispbuilder-sdl-mixer)
  :serial t
  :components ((:file "package")
               (:file "breakout")))

