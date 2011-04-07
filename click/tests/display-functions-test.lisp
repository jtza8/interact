; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :click)

(defclass display-functions-test (test-case)
  ())

(def-test-method test-overlay-rectangles ((test display-functions-test))
  (let ((a #(10 110 90 101))
        (b #(99 210 90 101))
        (c #(101 212 90 101)))
    (assert-equal '(99 210 1 1) (coerce (overlay-rectangles a b) 'list))
    (assert-equal '(101 212 0 0) (coerce (overlay-rectangles a c) 'list))))