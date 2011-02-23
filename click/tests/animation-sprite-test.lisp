; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :click)

(defun animation-sprite-test ()
  (init-screen-system :sprite-path
                      (asdf:system-relative-pathname :click-tests
                                                     "test-sprites"))
  (let* ((screen (make-instance 'screen :height 0 :width 0 :x 0 :y 0))
         (sprites (loop for r from 0.0 upto 1.0 by 0.01
                        collect (make-instance 'color-sprite
                                         :color `(,r 0.0 0.0 1.0)
                                         :height 100
                                         :width 100)
                          into sprite-list
                        finally (return (coerce sprite-list 'vector))))
         (animation-1 (make-instance 'animation-sprite
                                     :sprite-vector sprites :fps 10
                                     :width 100 :height 100
                                     :start t))
         (animation-2 (make-instance 'animation-sprite
                                     :sprite-vector sprites :fps 30
                                     :width 100 :height 100
                                     :start t))
         (widget-1 (make-instance 'simple-widget 
                                  :x 10 :y 10
                                  :sprite animation-1))
         (widget-2 (make-instance 'simple-widget 
                                  :x 120 :y 10 
                                  :sprite animation-2)))
    (add-widget screen widget-1 :widget-1)
    (add-widget screen widget-2 :widget-2)
  (run-screen-system)))
