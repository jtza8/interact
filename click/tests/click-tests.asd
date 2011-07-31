(asdf:defsystem "click-tests"
  :description "Click OpenGL GUI Tests"
  :version "0.1"
  :author "Jens Thiede"
  :licence "BSD-Style License"
  :depends-on ("click" "events-extra" "xlunit")
  :serial t
  :components ((:file "test-package")
               (:file "dummy-igo")
               (:file "event-converter-test")
               (:file "image-editing-test")
               (:file "image-sequence-test")
               (:file "sprite-sheet-test")
               (:file "sprite-tree-test")
               (:file "interactive-container")
               (:file "container-test")
               (:file "animation-sprite-test")
               (:file "font-sheet-test")
               (:file "bitmap-font-sprite-test")
               (:file "fps-counter-test")
               (:file "polygon-sprite-test")
               (:file "ttf-sprite-test")
               (:file "camera-test")
               (:file "shader-test")
               (:file "filter-test")))