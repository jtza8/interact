; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :interact)

(defclass test-polygon-sprite (test-case)
  ())

(defun test-polygon-sprite-manually ()
  (with-display-system (screen-width 800
                        screen-height 600
                        screen-colour '(0 0 0 1))
    (let* ((sprite (make-polygon-sprite :width 128 :height 128
                                        :points #(#(10 10) #(100 20)
                                                  #(90 85) #(5 80))))
           (widget (make-painter :sprite sprite :x 100 :y 0)))
      (add-to-root widget)
      (setf (fill-colour sprite) '(1.0 0.0 0.0 1.0)))))