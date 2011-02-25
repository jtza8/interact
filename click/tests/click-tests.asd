(asdf:defsystem "click-tests"
  :description "Click OpenGL GUI Tests"
  :version "0.1"
  :author "Jens Thiede"
  :licence "BSD-Style License"
  :depends-on ("click" "xlunit")
  :components ((:file "test-package")
               (:file "stopwatch-test" :depends-on ("test-package"))
               (:file "dummy-widget" :depends-on ("test-package"))
               (:file "dummy-listener")
               (:file "listening-test"
                      :depends-on ("test-package" "dummy-widget"))
               (:file "image-handling-test" :depends-on ("test-package"))
               (:file "sprite-management-test"
                      :depends-on ("test-package" "dummy-widget"))
               (:file "simple-widget"
                      :depends-on ("test-package"))
               (:file "screen-test"
                      :depends-on ("test-package" "simple-widget"))
               (:file "animation-sprite-test"
                      :depends-on ("test-package" "simple-widget"))))
