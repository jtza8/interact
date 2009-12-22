(defsystem "click"
  :description "Click OpenGL GUI."
  :version "0.1"
  :author "Jens Thiede"
  :licence "BSD-Style License"
  :depends-on ("lispbuilder-sdl" "cl-opengl")
  :components ((:file "package")
               (:file "click" :depends-on ("package"))
               (:file "widget" :depends-on ("package"))
               (:file "window" :depends-on ("package" "widget"))
               (:file "window_manager"
                :depends-on ("package" "widget" "window"))
               (:file "click_wm"
                :depends-on ("package" "widget" "window"))))