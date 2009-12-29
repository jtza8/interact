(defsystem "click-tests"
  :description "Click OpenGL GUI Tests"
  :version "0.1"
  :author "Jens Thiede"
  :licence "BSD-Style License"
  :depends-on ("click" "xlunit")
  :components ((:file "test_package")
               (:file "file_manager_test" :depends-on ("test_package"))
               (:file "dummy_widget" :depends-on ("test_package"))
               (:file "dummy_listener")
               (:file "widget_test"
                :depends-on ("test_package" "dummy_widget"))))