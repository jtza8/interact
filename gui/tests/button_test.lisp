; Copyright 2009 Jens Thiede. All rights reserved.
; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :click-gui)

(defun button-test ()
  (init-screen-system :bg-colour '(0 0 0 0)
                      :window-title "Button Test"
                      :sprite-path (asdf:system-relative-pathname 
                                    :click-gui "sprites"))
  (let ((screen (make-instance 'screen
                               :width 800 :height 600
                               :x 100 :y 100))
        (button (make-instance 'button :x 10 :y 10)))
    (add-widget screen button))
  (run-screen-system))