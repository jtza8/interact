(defsystem "click"
  :description "Click OpenGL GUI."
  :version "0.1"
  :author "Jens Thiede"
  :licence "BSD-Style License"
  :depends-on ("lispbuilder-sdl" "lispbuilder-sdl-image" "cl-opengl" "cl-fad")
  :components ((:file "package")
               (:file "image_management" :depends-on ("package"))
               (:file "click" :depends-on ("package"))
               (:file "widget" :depends-on ("click"))
               (:file "window_manager" :depends-on ("package"))
               (:file "window" :depends-on ("widget" "window_manager"))
               (:file "basic_gui" :depends-on ("window_manager"))))