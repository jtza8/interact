(asdf:defsystem "click-tests"
  :description "Click OpenGL GUI Tests"
  :version "0.1"
  :author "Jens Thiede"
  :licence "BSD-Style License"
  :depends-on ("click" "xlunit")
  :components ((:file "test-package")
               (:file "stopwatch-test" :depends-on ("test-package"))
               (:file "dummy-igo" :depends-on ("test-package"))
               (:file "dummy-listener" :depends-on ("test-package"))
               (:file "listening-test"
                      :depends-on ("test-package" "dummy-igo"))
               (:file "event-converter-test"
                      :depends-on ("dummy-listener"))
               (:file "image-editing-test" :depends-on ("test-package"))
               (:file "image-sequence-test" :depends-on ("test-package"))
               (:file "sprite-sheet-test" :depends-on ("image-sequence-test"))
               (:file "sprite-tree-test"
                      :depends-on ("test-package" "dummy-igo"))
               (:file "simple-igo"
                      :depends-on ("test-package"))
               (:file "interactive-container" :depends-on ("test-package"))
               (:file "container-test"
                      :depends-on ("test-package" "simple-igo" 
                                   "interactive-container"))
               (:file "animation-sprite-test"
                      :depends-on ("test-package" "simple-igo"))
               (:file "font-sheet-test"
                      :depends-on ("test-package"))
               (:file "bitmap-font-sprite-test"
                      :depends-on ("test-package" "simple-igo"))
               (:file "fps-counter-test"
                      :depends-on ("test-package"))))
