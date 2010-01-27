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

(defun interactive-window-test ()
  (init-basic-gui)
  (make-instance 'window :x 50 :y 50 :width 150 :height 200)
  (run-basic-gui))
