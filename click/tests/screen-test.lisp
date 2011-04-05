; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :click)

(defun test-screen-manually ()
  (with-display-system ()
    (load-sprite-path (asdf:system-relative-pathname 
                       :click-tests "test-sprites"))
    (let ((screen (make-instance 'screen :height 100 :width 100 :x 10 :y 10))
          (igo (make-instance 'simple-igo :x 10 :y 10 
                                 :sprite (sprite-node :test-sheet))))
      (add-igo screen igo :simple-igo))))