; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :interact)

(defun test-animation-sprite-manually ()
  (with-display-system (:width 800
                        :height 600)
    (load-sprite-path (asdf:system-relative-pathname :interact-tests
                                                     "test-sprites"))
    (let* ((sprites (loop for r from 0.0 upto 1.0 by 0.01
                       collect (make-instance 'colour-sprite
                                              :colour `(,r 0.0 0.0 1.0)
                                              :height 100
                                              :width 100)
                       into sprite-list
                       finally (return (coerce sprite-list 'vector))))
           (animation-1 (make-animation-sprite :sprite-vector sprites :fps 10
                                               :width 100 :height 100
                                               :looping nil))
           (animation-2 (clone animation-1))
           (widget-1 (make-instance 'painter
                                 :x 10 :y 10
                                 :sprite animation-1))
           (widget-2 (make-instance 'painter
                                 :x 120 :y 10
                                 :sprite animation-2)))
      (setf (fps animation-2) 30)
      (add-to-root widget-1 :widget-1)
      (add-to-root widget-2 :widget-2))
    (with-event-loop
        (event
         quit
         (case (event-type event)
           (:key-down
            (when (eq (getf (event-data event) :key) :escape)
              (setf quit t)))))
      (update-display-system))))
