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
