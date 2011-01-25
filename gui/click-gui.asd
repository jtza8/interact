(asdf:defsystem "click-gui"
  :description "Standard GUI library for click."
  :version "0.1"
  :author "Jens Thiede"
  :licence "BSD-Style License"
  :depends-on ("click")
  :components ((:file "package")
               (:file "button" :depends-on ("package"))))
;               (:file "title-bar" :depends-on ("package"))
;               (:file "window" :depends-on ("title-bar"))
