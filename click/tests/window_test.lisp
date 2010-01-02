; Copyright 2009 Jens Thiede. All rights reserved.
; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :click)

(defun window-test-main ()
  (init-basic-gui)
  (make-instance 'window :x 50 :y 50 :width 300 :height 400)
  (run-basic-gui))