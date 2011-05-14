; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :click)

(defun test-fps-counter-manually ()
  (with-display-system (screen-bg-color '(0 0 0 1))
    (load-sprite-path (asdf:system-relative-pathname :click-tests
                                                     "test-fonts"))
    (let ((fps-counter (make-instance 'fps-counter
                                      :font-sprite (sprite-node :8x16))))
      (add-root-igo fps-counter)
      )))