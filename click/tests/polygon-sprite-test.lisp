; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :click)

(defclass test-polygon-sprite (test-case)
  ())

(def-test-method test-add-points ((test test-polygon-sprite))
  (let ((polygon (make-polygon-sprite :width 64 :height 64)))
    (assert-equal '() (points polygon))
    (add-points polygon #(10 10) #(2 4))
    (assert-true (find #(10 10) (points polygon)))))

(defun test-polygon-sprite-manually ()
  (with-display-system (screen-width 800
                        screen-height 600
                        screen-bg-colour '(0 0 0 1))
    (let* ((sprite (make-polygon-sprite :width 128 :height 128
                                        :points #(#(10 10) #(100 20)
                                                  #(90 85) #(5 80))))
           (igo (make-simple-igo :sprite sprite :x 100 :y 0)))
      (add-root-igo igo)
      (setf (fill-colour sprite) '(1.0 0.0 0.0 1.0)))))