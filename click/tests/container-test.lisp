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
          (container (make-instance 'container :x 100 :y 120 
                                    :width 25 :height 340))
          (igo-2 (make-instance 'simple-igo :x -10 :y 0
                                :sprite (diverge (sprite-node :test-sheet)))))
      (add-igo *root-container* igo-1)
      (add-igo container igo-2)
      (add-igo *root-container* container))))
