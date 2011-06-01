; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :click)

(defun test-camera-manually ()
  (with-display-system (screen-bg-colour '(0 0 0 1))
    (create-camera :camera-1 :width 400 :height 600)
    ;; (create-camera :camera-2 :x 400 :width 400 :height 600)
    (load-sprite-path *test-fonts-path*)
    (let ((red (make-simple-igo :x 0 :y 0
                                :sprite 
                                (make-colour-sprite :colour '(1.0 0.0 0.0 1.0)
                                                    :width 100 :height 100)))
          (fps-counter 
           (make-fps-counter :font-sprite (diverge (sprite-node :8x16))
                             :x 10 :y 10)))
      (add-root-igo red)
      (add-root-igo fps-counter))))