;;; CFFI-Grovel is needed for processing grovel-file components
(cl:eval-when (:load-toplevel :execute)
  (asdf:operate 'asdf:load-op 'cffi-grovel))

(asdf:defsystem "click"
  :description "Click OpenGL GUI."
  :version "0.1"
  :author "Jens Thiede"
  :licence "BSD-Style License"
  :depends-on ("cffi" "lispbuilder-sdl" "cl-opengl" "cl-devil" "cl-fad" 
               "cl-ppcre")
  :serial t
  :components ((:file "package")
               (cffi-grovel:grovel-file "grovel")
               (:file "utils" :depends-on ("package"))
               (:file "click" :depends-on ("package"))
               (:file "listenable" :depends-on ("package"))
               (:file "listener" :depends-on ("package"))
               (:file "sprite" :depends-on ("click"))
               (:file "color-sprite" :depends-on ("click"))
               (:file "texture-sprite" :depends-on ("sprite"))
               (:file "image-handling" :depends-on ("package"))
               (:file "sprite-management" :depends-on ("click"))
               (:file "font-tools" :depends-on ("texture-sprite"))
               (:file "screen-manager" :depends-on ("listenable"))
               (:file "widget"
                :depends-on ("listenable" "listener" "sprite-management"))
               (:file "screen" :depends-on ("widget"))
               (:file "screen-system" :depends-on ("widget"))))
