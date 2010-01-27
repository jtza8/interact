; Copyright 2009 Jens Thiede. All rights reserved.
; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :click)

(defclass window-test (test-case)
  ())

(defmethod set-up ((test window-test))
  (sdl:init-video)
  (init-click))

(defmethod tear-down ((test window-test))
  (sdl:quit-sdl))

(def-test-method test-add-and-remove-widget ((test window-test))
  (let ((widget (make-instance 'dummy-widget))
        (window (make-instance 'window)))
    (add-widget window widget)
    (assert-condition 'type-error (add-widget window '(blah)))
    (assert-true (find widget (widgets window)))
    (remove-widget window widget)
    (assert-false (find widget (widgets window)))))

(def-test-method test-event-system ((test window-test))
  (let ((widget (make-instance 'dummy-widget))
        (window (make-instance 'window)))
    (assert-equal nil (latest-event widget))
    (add-widget window widget)
    (handle-event window '(:mouse-move :x 20 :y 40))
    (assert-equal '(:mouse-move :x 20 :y 40) (latest-event widget))))

(def-test-method test-tagging ((test window-test))
  (let ((widget-1 (make-instance 'dummy-widget))
        (widget-2 (make-instance 'dummy-widget))
        (window (make-instance 'window)))
    (add-widget window widget-1)
    (tag-widget window widget-1 :widget-1)
    (add-widget window widget-2)
    (assert-condition 'tag-error (tag-widget window widget-2 :widget-1))
    (assert-condition 'tag-error (tag-widget window widget-1 :widget-2))
    (assert-equal widget-1 (widget window :widget-1))
    (tag-widget window widget-2)
    (assert-condition 'tag-error (widget window :widget-3))))

(defun interactive-window-test ()
  (init-basic-gui)
  (make-instance 'window :x 50 :y 50 :width 150 :height 200)
  (run-basic-gui))
