; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :interact)

(defun test-fps-counter-manually ()
  (with-display-system (:clear-colour '(0.0 0.0 0.0 1.0))
    (load-sprite-path (asdf:system-relative-pathname :interact-tests
                                                     "test-fonts"))
    (let ((fps-counter (make-instance 'fps-counter
                                      :font-sprite (sprite-node :8x16)
                                      :y (- (height *screen*) 42)
                                      :x 10)))
      (add-to-root fps-counter))
    (with-event-loop () (update-display-system))))
