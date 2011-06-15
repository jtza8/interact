; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :click)

(defun test-camera-manually ()
  (with-display-system (screen-colour '(0 0 0 1)
                        screen-width 800
                        screen-height 600)
    (load-sprite-path *test-fonts-path*)
    (let ((camera-1 (make-camera :x 0 :y 0 :width 400 :height 600))
          (camera-2 (make-camera :x 400 :y 0 :width 400 :height 600))
          (container (make-container))
          (red (make-painter :x 0 :y 0
                                :sprite 
                                (make-polygon-sprite 
                                  :points #(#(2 2) #(200 200) #(300 2))
                                  :width 512 :height 512
                                  :line-colour '(1.0 1.0 1.0 1.0)
                                  :fill-colour '(1.0 0.0 0.0 1.0)
                                  :line-width 2)))
          (fps-counter (make-fps-counter :font-sprite (sprite-node :8x16)
                                         :x 10 :y (- (screen-height) 42))))
      ;; (print (typep camera-1 'igo))
      (add-igo container red)
      (add-igo container fps-counter)
      (setf (root camera-1) container
            (root camera-2) container)
      (add-to-root camera-1)
      (add-to-root camera-2))))
