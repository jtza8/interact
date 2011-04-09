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

(defun test-clipping-manually ()
  (with-display-system ()
    (add-igo *root-container* 
             (make-instance 'container :x 10 :y 10
                            :width 100 :height 100)
             :container-a)
    (add-igo (igo-of *root-container* :container-a)
             (make-instance 'simple-igo 
                            :sprite
                            (make-instance 'color-sprite
                                           :color '(1.0 0.0 0.0)
                                           :width 100 :height 100)))
    (add-igo *root-container* 
             (make-instance 'container :x 120 :y 10
                            :width 50 :height 50)
             :container-b)
    (add-igo (igo-of *root-container* :container-b)
             (make-instance 'simple-igo 
                            :sprite
                            (make-instance 'color-sprite
                                           :color '(1.0 0.0 0.0)
                                           :width 100 :height 100)))
    (add-igo *root-container* 
             (make-instance 'container :x 240 :y 10
                            :width 100 :height 100)
             :container-c)
    (add-igo (igo-of *root-container* :container-c)
             (make-instance 'simple-igo 
                            :sprite
                            (make-instance 'color-sprite
                                           :color '(0.0 1.0 0.0)
                                           :width 100 :height 100)))
    (add-igo (igo-of *root-container* :container-c)
             (make-instance 'container :x 70 :y 60
                            :width 50 :height 50)
             :container-d)
    (add-igo (igo-of (igo-of *root-container* :container-c) :container-d)
             (make-instance 'simple-igo 
                            :sprite
                            (make-instance 'color-sprite
                                           :color '(0.0 0.0 1.0)
                                           :width 100 :height 100)))
    (add-igo (igo-of (igo-of *root-container* :container-c) :container-d)
             (make-instance 'container :x -10 :y 0
                            :width 25 :height 25)
             :container-e)
    (add-igo (igo-of (igo-of (igo-of *root-container* :container-c)
                             :container-d)
                     :container-e)
             (make-instance 'simple-igo 
                            :sprite
                            (make-instance 'color-sprite
                                           :color '(1.0 0.0 0.0)
                                           :width 100 :height 100)))))
