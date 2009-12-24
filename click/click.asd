(defsystem "click"
  :description "Click OpenGL GUI."
  :version "0.1"
  :author "Jens Thiede"
  :licence "BSD-Style License"
  :depends-on ("lispbuilder-sdl" "cl-opengl" "cl-fad")
  :components ((:file "package")
               (:file "click" :depends-on ("package"))
               (:file "widget" :depends-on ("package"))
               (:module "theme_module"
                        :depends-on ("package")
                        :components ((:file "theme_manager")
                                     (:file "window_theme_manager")))
               (:file "window" :depends-on ("package" "widget"))
               (:file "window_manager"
                :depends-on ("package" "widget" "window"))
               (:file "basic_gui"
                :depends-on ("package" "widget" "window"))))