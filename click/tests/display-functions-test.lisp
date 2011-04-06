; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :click)

(defclass display-functions-test (test-case)
  ())

(def-test-method test-overlay-rectangles ((test display-functions-test))
  (let ((a '(10 11 90 101))
        (b '(20 22 200 202))
        (c '(110 111 100 101)))
    (assert-equal '(20 22 80 90) (overlay-rectangles a b))
    (assert-equal '(110 111 0 1) (overlay-rectangles a c))))