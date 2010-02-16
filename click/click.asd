(defsystem "click"
  :description "Click OpenGL GUI."
  :version "0.1"
  :author "Jens Thiede"
  :licence "BSD-Style License"
  :depends-on ("lispbuilder-sdl" "lispbuilder-sdl-image" "cl-opengl" "cl-fad")
  :components ((:file "package")
               (:file "click" :depends-on ("package"))
               (:file "listenable" :depends-on ("package"))
               (:file "listener" :depends-on ("package"))
               (:file "sprite" :depends-on ("click"))
               (:file "image_sprite" :depends-on ("sprite"))
               (:file "sprite_management" :depends-on ("click"))
               (:file "window_manager" :depends-on ("listenable"))
               (:file "widget" :depends-on ("listenable" "listener"
                                            "sprite_management"))
               (:file "title_bar" :depends-on ("widget"))
               (:file "window" :depends-on ("title_bar" "window_manager"))
               (:file "basic_gui" :depends-on ("window_manager"))))