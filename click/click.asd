(asdf:defsystem "click"
  :description "Click OpenGL GUI."
  :version "0.1"
  :author "Jens Thiede"
  :licence "BSD-Style License"
  :depends-on ("cffi" "lispbuilder-sdl" "cl-opengl" "cl-devil" "cl-fad")
  :components ((:file "package")
               (:file "click" :depends-on ("package"))
               (:file "listenable" :depends-on ("package"))
               (:file "listener" :depends-on ("package"))
               (:file "sprite" :depends-on ("click"))
               (:file "image_sprite" :depends-on ("sprite"))
               (:file "sprite_management" :depends-on ("click"))
               (:file "window_manager" :depends-on ("listenable"))
               (:file "widget"
                :depends-on ("listenable" "listener" "sprite_management"))
               (:module "widgets" :depends-on ("widget" "window_manager")
                :components ((:file "title_bar")
                             (:file "window" :depends-on ("title_bar"))))
               (:file "basic_gui" :depends-on ("widgets"))))
