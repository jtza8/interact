; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :interact)

(defclass container-test (test-case)
  ())

(def-test-method test-add-and-remove-widget ((test container-test))
  (let ((widget (make-instance 'dummy-widget))
        (container (make-instance 'container)))
    (add-widget container widget :widget)
    (assert-true (find widget (widgets container)))
    (assert-eql widget (widget-of container :widget))
    (remove-widget container widget :unsubscribes nil)
    (assert-condition 'widget-tag-error (widget-of container :widget))
    (assert-false (find widget (widgets container)))
    (assert-eql nil (latest-event widget))
    (send-event container #1='(:mouse-motion :x 10 :y 23))
    (assert-equal #1# (latest-event widget))
    (add-widget container widget)
    (remove-widget container widget)
    (send-event container '(:mouse-motion :x 37 :y 73))
    (assert-equal #1# (latest-event widget))))

(def-test-method test-tagging ((test container-test))
  (let ((widget-1 (make-instance 'dummy-widget))
        (widget-2 (make-instance 'dummy-widget))
        (container (make-instance 'container)))
    (add-widget container widget-1)
    (tag-widget container widget-1 :widget-1)
    (add-widget container widget-2)
    (assert-condition 'widget-tag-error (tag-widget container widget-2 :widget-1))
    (assert-condition 'widget-tag-error (tag-widget container widget-1 :widget-2))
    (assert-eql widget-1 (widget-of container :widget-1))
    (tag-widget container widget-2 :widget-2)
    (assert-condition 'widget-tag-error (widget-of container :widget-3))
    (remove-tag container widget-1)
    (assert-condition 'widget-tag-error (widget-of container :widget-1))
    (assert-eql widget-2 (widget-of container :widget-2))
    (remove-tag container :widget-2)
    (assert-condition 'widget-tag-error (widget-of container :widget-2))))
    
(def-test-method test-tag-of ((test container-test))
  (let ((widget (make-instance 'dummy-widget))
        (container (make-instance 'container)))
    (add-widget container widget)
    (tag-widget container widget :widget)
    (assert-eql :widget (tag-of container widget))
    (remove-tag container :widget)
    (assert-eql nil (tag-of container widget))))

(def-test-method test-propegation ((test container-test))
  (let ((container (make-instance 'container))
        (dummy (make-instance 'dummy-widget
                              :desired-events '(:key-down :key-up))))
    (add-widget container dummy :dummy)
    (send-event container #1='(:key-down :key :a))
    (assert-equal #1# (latest-event dummy))))

(defun test-rotation-manually ()
  (with-display-system (:screen-colour '(0 0 0 1) 
                        :screen-width 1280 :screen-height 800)
    (load-sprite-path (asdf:system-relative-pathname 
                       :interact-tests "test-sprites"))
    (add-root-listener (make-instance 'event-assistant 
                                      :quit-key :escape
                                      :fullscreen-key :F12))
    (add-to-root (make-instance 'interactive-container
                                 :x 100 :y 100 :width 100 :height 100
                                 :pivot-x 50 :pivot-y 50 :rotation 20
                                 :background 
                                 (make-instance 'colour-sprite
                                                :colour '(0.5 0.5 0.5)
                                                :width 100 :height 100))
                  :container-a)
    (add-widget (widget-of-root :container-a)
             (make-instance 'painter
                            :x 18 :y 18 :width 64 :height 64
                            :pivot-x 32 :pivot-y 32 :rotation 45
                            :sprite (diverge (sprite-node :test-sheet))))
    (add-widget (widget-of-root :container-a)
             (make-instance 'interactive-container
                            :x 100 :y 0 :width 64 :height 64
                            :pivot-x 0 :pivot-y 0 :rotation 0
                            :background (diverge (sprite-node :test-sheet))))))
