; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :click)

(defclass ttf-sprite (vector-sprite)
  ((text :initform ""
         :reader text)))

(defmethod update-texture ((sprite ttf-sprite))
  ())