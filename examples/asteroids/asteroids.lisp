; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :click-examples)

(defun asteroids ()
  (click:with-display-system (:bg-color '(0 0 0 1) :title "Asteroids Demo")
    (load-sprite-path 
     (asdf:system-relative-pathname :click-examples #p"asteroids/sprites"))
    (add-root-listener
     (make-instance 'event-assistant :quit-key :escape))
    (add-root-igo (make-instance 'asteroid :x 100 :y 200))))