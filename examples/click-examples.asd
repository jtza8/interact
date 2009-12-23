(defsystem "click-examples"
  :description "Examples of using the Click GUI."
  :version "0.1"
  :author "Jens Thiede"
  :licence "BSD-Style License"
  :depends-on ("click")
  :components ((:file "examples_package")
               (:file "hello_world" :depends-on ("examples_package"))))