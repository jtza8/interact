(defsystem "click"
  :description "Click OpenGL GUI."
  :version "0.1"
  :author "Jens Thiede"
  :licence "BSD-Style License"
  :depends-on ("lispbuilder-sdl" "cl-opengl" "cl-fad")
  :components ((:file "package")
               (:file "click" :depends-on ("package"))
               (:file "resource_manager" :depends-on ("click"))
               (:file "widget" :depends-on ("resource_manager"))
               (:file "window" :depends-on ("widget"))
               (:file "window_manager" :depends-on ("window"))
               (:file "basic_gui" :depends-on ("window_manager"))))