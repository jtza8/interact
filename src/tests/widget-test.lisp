; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :interact)

(defclass widget-test (test-case)
  (dummy container))

(defmethod set-up ((test widget-test))
  (with-slots (dummy container) test
    (setf dummy (make-instance 'dummy-widget :x 10 :y 13 :width 20 :height 15)
          container (make-instance 'container :x 50 :y -200))))

(def-test-method test-absolute-pos ((test widget-test))
  (with-slots (dummy container) test
    (add-widget container dummy)
    (destructuring-bind (x y) (absolute-pos dummy)
      (assert-equal 60 x)
      (assert-equal -187 y))))

;; (def-test-method test-relative-pos ((test widget-test))
;;   (with-slots (dummy container) test
;;     (destructuring-bind (x y) (relative-pos dummy 10 28)
;;       (assert-equal ))))