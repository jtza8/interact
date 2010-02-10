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
    (assert-eql nil (latest-event widget))
    (add-widget window widget)
    (send-event window '(:mouse-move :x 20 :y 40))
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
    (assert-eql widget-1 (widget window :widget-1))
    (tag-widget window widget-2 :widget-2)
    (assert-condition 'tag-error (widget window :widget-3))))

(def-test-method test-active-window ((test window-test))
  (let ((window-1 (make-instance 'window))
        (window-2 (make-instance 'window)))
    (assert-eql window-2 (car (last (windows *window-manager*))))
    (assert-eql window-2 (active-window *window-manager*))
    #1=(setf (active-window *window-manager*) window-1)
    #1#
    (assert-eql window-1 (active-window *window-manager*))
    (assert-equal (list window-2 window-1) (windows *window-manager*))
    (setf (active-window *window-manager*) window-2)
    (assert-equal (list window-1 window-2) (windows *window-manager*))
    (assert-eql window-2 (active-window *window-manager*))))

(defun interactive-window-test ()
  (init-basic-gui)
  (make-instance 'window :x 20 :y 20 :width 400 :height 300)
  (make-instance 'window :x 50 :y 50 :width 150 :height 200)
  (make-instance 'window :x 100 :y 80 :width 200 :height 150)
  (run-basic-gui))
