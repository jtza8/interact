; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :click)

(defclass animation-sprite (sprite)
  ((fps :initarg :fps
        :initform (error "must specify fps")
        :reader fps)
   (sprite-vector :initarg :sprite-vector
                  :initform (error "must specify sprite vector"))
   (timer :initform (make-instance stop-watch))))