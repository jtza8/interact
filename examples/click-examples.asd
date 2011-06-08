(asdf:defsystem "click-examples"
  :author "Jens Thiede"
  :license "BSD-Style"
  :depends-on (click)
  :serial t
  :components ((:file "package")
               (:module "asteroids"
                        :components ((:file "asteroid")
                                     (:file "asteroid-container")
                                     (:file "asteroids")))
               (:module "shaders"
                        :components ((:file "warp-shader")
                                     (:file "shader-demo")))))