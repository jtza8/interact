(asdf:defsystem "interact"
  :description "Click 2D OpenGL graphics engine."
  :version "0.1"
  :author "Jens Thiede"
  :licence "BSD-Style License"
  :depends-on ("cl-glfw-vanilla" "cl-opengl" "cl-devil" "cl-fad" "cl-ppcre"
               "vecto" "meta-package" "events" "resource-tree" "watch")
  :serial t
  :components ((:file "package")
               (:file "conditions")
               (:file "macros")
               (:file "sprite")
               (:file "colour-sprite")
               (:file "texture-sprite")
               (:file "animation-sprite")
               (:file "sprite-snippets")
               (:file "image-editing")
               (:file "image-sequence")
               (:file "sprite-sheet")
               (:file "font-sheet")
               (:file "bitmap-font-sprite")
               (:file "vector-sprite")
               (:file "polygon-sprite")
               (:file "ttf-sprite")
               (:file "sprite-tree")
               (:file "widget")
               (:file "painter")
               (:file "container")
               ;; (:file "shader")
               ;; (:file "filter")
               ;; (:file "camera")
               (:file "events")
               (:file "display-system")
               ;; (:file "fps-counter")
               (:file "export")))
