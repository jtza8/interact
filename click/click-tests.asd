(asdf:defsystem "click-tests"
  :description "Click OpenGL GUI Tests"
  :version "0.1"
  :author "Jens Thiede"
  :licence "BSD-Style License"
  :depends-on ("click" "xlunit")
  :components ((:module "tests"
                :components ((:file "test_package")
                             (:file "click_test" :depends-on ("test_package"))
                             (:file "dummy_widget" :depends-on ("test_package"))
                             (:file "dummy_listener")
                             (:file "listening_test"
                              :depends-on ("test_package" "dummy_widget"))
                             (:file "sprite_management_test"
                              :depends-on ("test_package" "dummy_widget"))
                             (:file "simple_widget"
                              :depends-on ("test_package"))
                             (:file "screen_test"
                              :depends-on ("test_package" "simple_widget"))))))
