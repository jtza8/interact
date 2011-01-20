(asdf:defsystem "click"
  :description "Click OpenGL GUI."
  :version "0.1"
  :author "Jens Thiede"
  :licence "BSD-Style License"
  :depends-on ("cffi" "lispbuilder-sdl" "cl-opengl" "cl-devil" "cl-fad")
  :components ((:file "package")
               (:file "utils" :depends-on ("package"))
               (:file "click" :depends-on ("package"))
               (:file "listenable" :depends-on ("package"))
               (:file "listener" :depends-on ("package"))
               (:file "sprite" :depends-on ("click"))
               (:file "image_sprite" :depends-on ("sprite"))
               (:file "sprite_management" :depends-on ("click"))
               (:file "font_tools" :depends-on ("image_sprite"))
               (:file "screen_manager" :depends-on ("listenable"))
               (:file "widget"
                :depends-on ("listenable" "listener" "sprite_management"))
               (:file "screen" :depends-on ("widget"))
               (:module "widgets" :depends-on ("widget" "screen_manager")
                :components ((:file "title_bar")
                             (:file "window" :depends-on ("title_bar"))
                             (:file "button")))
               (:file "basic_gui" :depends-on ("widgets"))))
