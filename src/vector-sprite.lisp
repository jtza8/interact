; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :interact)

(defclass vector-sprite (texture-sprite)
  ((texture :initform (car (gl:gen-textures 1)))))

(defmethod initialize-instance :after ((sprite vector-sprite) &key)
  (update-texture sprite))

(defmethod update-texture ((sprite vector-sprite))
  (error "No UPDATE-TEXTURE method specified for ~s." sprite))

(defmacro define-vector-sprite-writers (class-name &body slot-names)
  `(progn ,@(loop for slot-name in slot-names
                  collect `(defmethod (setf ,slot-name)
                               (value (sprite ,class-name))
                             (setf (slot-value sprite ',slot-name) value)
                             (update-texture sprite)))))
