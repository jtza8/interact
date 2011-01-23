; Copyright 2009 Jens Thiede. All rights reserved.
; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :click)

(use-package :xlunit)

(defun set-test-sprite-path ()
  (setf (getf *settings* :sprite-path)
        (asdf:system-relative-pathname :click "tests/test_sprites"))
  (init-click))

(init-screen-system)
(sdl:quit-video)
