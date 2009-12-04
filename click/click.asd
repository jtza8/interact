(defsystem "click"
  :description "Click GUI"
  :version "0.1"
  :author "Jens Thiede"
  :licence "BSD-Style License"
  :components ((:file "package")
               (:file "widget" :depends-on ("package"))))