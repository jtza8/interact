; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :click)

(defclass container-test (test-case)
  ())

(def-test-method test-add-and-remove-igo ((test container-test))
  (let ((igo (make-instance 'dummy-igo))
        (container (make-instance 'container)))
    (add-igo container igo :igo)
    (assert-condition 'type-error (add-igo container '(blah)))
    (assert-true (find igo (igos container)))
    (assert-eql igo (igo-of container :igo))
    (remove-igo container igo :remove-listeners nil)
    (assert-condition 'tag-error (igo-of container :igo))
    (assert-false (find igo (igos container)))
    (assert-eql nil (latest-event igo))
    (send-event container #1='(:mouse-motion :x 10 :y 23))
    (assert-equal #1# (latest-event igo))
    (add-igo container igo)
    (remove-igo container igo)
    (send-event container '(:mouse-motion :x 37 :y 73))
    (assert-equal #1# (latest-event igo))))

(def-test-method test-tagging ((test container-test))
  (let ((igo-1 (make-instance 'dummy-igo))
        (igo-2 (make-instance 'dummy-igo))
        (container (make-instance 'container)))
    (add-igo container igo-1)
    (tag-igo container igo-1 :igo-1)
    (add-igo container igo-2)
    (assert-condition 'tag-error (tag-igo container igo-2 :igo-1))
    (assert-condition 'tag-error (tag-igo container igo-1 :igo-2))
    (assert-eql igo-1 (igo-of container :igo-1))
    (tag-igo container igo-2 :igo-2)
    (assert-condition 'tag-error (igo-of container :igo-3))
    (remove-tag container igo-1)
    (assert-condition 'tag-error (igo-of container :igo-1))
    (assert-eql igo-2 (igo-of container :igo-2))
    (remove-tag container :igo-2)
    (assert-condition 'tag-error (igo-of container :igo-2))))
    
(def-test-method test-tag-of ((test container-test))
  (let ((igo (make-instance 'dummy-igo))
        (container (make-instance 'container)))
    (add-igo container igo)
    (tag-igo container igo :igo)
    (assert-eql :igo (tag-of container igo))
    (remove-tag container :igo)
    (assert-eql nil (tag-of container igo))))

(def-test-method test-propegation ((test container-test))
  (let ((container (make-instance 'container))
        (dummy (make-instance 'dummy-igo
                              :desired-events '(:key-down :key-up))))
    (add-igo container dummy :dummy)
    (send-event container #1='(:key-down :key :a))
    (assert-equal #1# (latest-event dummy))))

(defun test-rotation-manually ()
  (with-display-system (:bg-color '(0 0 0 1))
    (load-sprite-path (asdf:system-relative-pathname 
                       :click-tests "test-sprites"))
    (add-listener *root-container* 
                  (make-instance 'event-assistant 
                                 :quit-button :escape))
    (add-igo *root-container* 
             (make-instance 'interactive-container
                            :x 100 :y 100 :width 100 :height 100
                            :pivot-x 50 :pivot-y 50 :rotation 20
                            :background (make-instance 'color-sprite
                                                       :color '(0.5 0.5 0.5)
                                                       :width 100 :height 100))
             :container-a)
    (add-igo (igo-of *root-container* :container-a)
             (make-instance 'simple-igo
                            :x 18 :y 18 :width 64 :height 64
                            :pivot-x 32 :pivot-y 32 :rotation 45
                            :sprite (sprite-node :test-sheet)))
    (add-igo (igo-of *root-container* :container-a)
             (make-instance 'interactive-container
                            :x 100 :y 0 :width 64 :height 64
                            :pivot-x 0 :pivot-y 0 :rotation 0
                            :background (sprite-node :test-sheet)))))
