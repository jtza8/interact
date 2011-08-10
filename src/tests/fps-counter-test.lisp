; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :interact)

(defun test-fps-counter-manually ()
  (with-display-system (screen-colour '(0 0 0 1))
    (load-sprite-path (asdf:system-relative-pathname :interact-tests
                                                     "test-fonts"))
    (let ((fps-counter (make-instance 'fps-counter
                                      :font-sprite (sprite-node :8x16)
                                      :y (- (screen-height) 42)
                                      :x 10)))
      (add-to-root fps-counter))))