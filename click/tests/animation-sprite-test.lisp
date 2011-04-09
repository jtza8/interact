; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :click)

(defun test-animation-sprite-manually ()
  (start-display-system)
  (load-sprite-path (asdf:system-relative-pathname :click-tests
                                                   "test-sprites"))
  (let* ((container (make-instance 'container :height 0 :width 0 :x 0 :y 0))
         (sprites (loop for r from 0.0 upto 1.0 by 0.01
                        collect (make-instance 'color-sprite
                                         :color `(,r 0.0 0.0 1.0)
                                         :height 100
                                         :width 100)
                          into sprite-list
                        finally (return (coerce sprite-list 'vector))))
         (animation-1 (make-instance 'animation-sprite
                                     :sprite-vector sprites :fps 10
                                     :width 100 :height 100))
         (animation-2 (diverge animation-1))
         (igo-1 (make-instance 'simple-igo 
                                  :x 10 :y 10
                                  :sprite animation-1))
         (igo-2 (make-instance 'simple-igo 
                                  :x 120 :y 10 
                                  :sprite animation-2)))
    ; The following is bad code, it is a cheat only used for testing
    ; purposes.
    (setf (slot-value animation-2 'fps) 30)
    (add-igo container igo-1 :igo-1)
    (add-igo container igo-2 :igo-2)
  (run-display-system)))
