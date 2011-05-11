(asdf:defsystem "click"
  :description "Click 2D OpenGL graphics engine."
  :version "0.1"
  :author "Jens Thiede"
  :licence "BSD-Style License"
  :depends-on ("cffi" "lispbuilder-sdl" "resource-tree" "cl-opengl" "cl-devil"
               "cl-fad" "cl-ppcre" "meta-package")
  :serial t
  :components ((:file "package")
               (:file "conditions")
               (:file "macros")
               (:file "stopwatch")
               (:file "listener")
               (:file "listenable")
               (:file "sdl-events")
               (:file "event-assistant")
               (:file "event-converter")
               (:file "sprite")
               (:file "color-sprite")
               (:file "texture-sprite")
               (:file "animation-sprite")
               (:file "image-editing")
               (:file "image-sequence")
               (:file "sprite-sheet")
               (:file "font-sheet")
               (:file "bitmap-font-sprite")
               (:file "sprite-tree")
               (:file "igo")
               (:file "container")
               (:file "display-system")
               (:file "export")))
