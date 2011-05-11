; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :click)

(use-package :xlunit)
(defparameter *test-sprite-path*
  (asdf:system-relative-pathname :click "tests/test-sprites"))
(defparameter *test-image-path*
  (asdf:system-relative-pathname :click-tests "test-images/"))

(start-display-system)
(sdl:quit-video)
