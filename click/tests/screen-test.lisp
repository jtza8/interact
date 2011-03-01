; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :click)

(defun test-screen-manually ()
  (init-screen-system :sprite-path
                      (asdf:system-relative-pathname :click-tests 
                                                     "test-sprites"))
  (let ((screen (make-instance 'screen :height 100 :width 100 :x 10 :y 10))
        (widget (make-instance 'simple-widget :x 10 :y 10 
                               :sprite (sprite-node :test-sheet))))
    (start (sprite-node :test-sheet))
    (add-widget screen widget :simple-widget))
  (run-screen-system))