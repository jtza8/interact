; Copyright 2010 Jens Thiede. All rights reserved.
; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :click)

(defun screen-test ()
  (init-basic-gui)
  (let ((screen (make-instance 'screen :height 100 :width 100 :x 10 :y 10))
        (button (make-instance 'button :x 10 :y 10)))
    (add-widget screen button :button))
  (run-basic-gui))

