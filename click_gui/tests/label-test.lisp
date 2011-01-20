; Copyright 2010 Jens Thiede. All rights reserved.
; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :click)

(defun test-label ()
  (init-screen-system 1024 640 nil)
  (unless (sdl:initialise-default-font sdl:*ttf-font-vera*)
    (error "FONT-EXAMPLE: Cannot initialize the default font."))
  (let ((window (make-instance 'window :x 20 :y 20 :width 400 :height 300))
        (label (make-instance 'label :x 25 :y 25 :text "Hello World")))
    (add-widget window label))
  (run-screen-system))
