(asdf:defsystem "click"
  :description "Click 2D OpenGL graphics engine."
  :version "0.1"
  :author "Jens Thiede"
  :licence "BSD-Style License"
  :depends-on ("cffi" "lispbuilder-sdl" "resource-tree" "cl-opengl" "cl-devil"
               "cl-fad" "cl-ppcre")
  :components ((:file "package")
               (:file "utils" :depends-on ("package"))
               (:file "stopwatch" :depends-on ("package"))
               (:file "listenable" :depends-on ("package"))
               (:file "listener" :depends-on ("package"))
               (:file "sdl-events" :depends-on ("package"))
               (:file "event-converter" :depends-on
                      ("listenable" "sdl-events"))
               (:file "sprite" :depends-on ("package"))
               (:file "color-sprite" :depends-on ("sprite"))
               (:file "texture-sprite" :depends-on ("sprite"))
               (:file "animation-sprite" :depends-on ("sprite" "stopwatch"))
               (:file "image-conditions" :depends-on ("package"))
               (:file "image-handling" :depends-on 
                      ("image-conditions" "texture-sprite"))
               (:file "image-sequence" :depends-on ("package"))
               (:file "sprite-sheet" :depends-on 
                      ("image-handling" "image-sequence" "texture-sprite"
                       "animation-sprite"))
               (:file "sprite-tree" :depends-on
                      ("color-sprite" "texture-sprite" "animation-sprite"))
               (:file "font-tools" :depends-on ("texture-sprite"))
               (:file "igo" :depends-on
                      ("listenable" "listener" "sprite-tree"))
               (:file "container" :depends-on ("igo"))
               (:file "display-system" :depends-on ("container" "stopwatch"))))
