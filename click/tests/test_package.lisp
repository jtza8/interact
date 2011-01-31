; Copyright 2009 Jens Thiede. All rights reserved.
; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :click)

(use-package :xlunit)
(defparameter *test-sprite-path*
  (asdf:system-relative-pathname :click "tests/test_sprites"))

(init-screen-system :sprite-path *test-sprite-path*)
(sdl:quit-video)
