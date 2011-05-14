(asdf:defsystem "click-examples"
  :author "Jens Thiede"
  :license "BSD-Style"
  :depends-on (click)
  :components ((:file "package")
               (:module "asteroids"
                        :depends-on ("package")
                        :components ((:file "asteroid")
                                     (:file "asteroid-container")
                                     (:file "asteroids"
                                            :depends-on ("asteroid"))))))
