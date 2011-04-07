; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :click)

(defclass container-test (test-case)
  ())

(def-test-method test-propegation ((test container-test))
  (let ((container (make-instance 'container))
        (dummy (make-instance 'dummy-igo
                              :desired-events '(:key-down :key-up))))
    (add-igo container dummy :dummy)
    (send-event container #1='(:key-down :key :a))
    (assert-equal #1# (latest-event dummy))))

(defun test-container-manually ()
  (with-display-system ()
    (load-sprite-path (asdf:system-relative-pathname 
                       :click-tests "test-sprites"))
    (let ((igo-1 (make-instance 'simple-igo :x 10 :y 10 
                                :sprite (diverge (sprite-node :test-sheet))))
          (container-a (make-instance 'container :x 100 :y 100
                                      :width 200 :height 200))
          (container-b (make-instance 'container :x 0 :y 0
                                      :width 200 :height 200))
          (igo-2 (make-instance 'simple-igo :x 0 :y 0 :sprite
                                (make-instance 'color-sprite
                                               :color '(1.0 0.0 0.0)
                                               :width 200 :height 200))))
      (add-igo *root-container* igo-1)
      (add-igo container-b igo-2)
      (add-igo container-a container-b)
      (add-igo *root-container* container-a))))
