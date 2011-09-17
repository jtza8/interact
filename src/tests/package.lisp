; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :interact)

(use-package :xlunit)
(defparameter *test-sprite-path*
  (asdf:system-relative-pathname :interact "tests/test-sprites"))
(defparameter *test-image-path*
  (asdf:system-relative-pathname :interact-tests "test-images/"))
(defparameter *test-fonts-path*
  (asdf:system-relative-pathname :interact "tests/test-fonts/"))