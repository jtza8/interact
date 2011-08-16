; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :interact)

(defun test-ttf-sprite-manually ()
  (with-display-system (:clear-colour '(1.0 1.0 1.0 1.0))
    (load-sprite-path *test-fonts-path*)
    (let* ((sprite (diverge (sprite-node :vera) :size 30 :text "Hello World"))
           (widget (make-painter :sprite sprite :x 10 :y 10
                                 :width 800 :height 600)))
      (add-to-root widget))
    (with-event-loop () (update-display-system))))
