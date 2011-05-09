; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :click)

(defclass bitmap-font-sprite (sprite)
  ((font-dimensions :reader font-dimensions)
   (message :initform "" 
            :accessor message)
   font-vector))

(defmethod initialize-instance :after ((sprite bitmap-font-sprite))
  (