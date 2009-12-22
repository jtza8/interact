(defsystem "click-tests"
  :description "Click OpenGL GUI Tests"
  :version "0.1"
  :author "Jens Thiede"
  :licence "BSD-Style License"
  :depends-on ("click" "xlunit")
  :components ((:file "test-package")
               (:file "dummy_widget" :depends-on ("test-package"))
               (:file "widget_test"
                :depends-on ("test-package" "dummy_widget"))))