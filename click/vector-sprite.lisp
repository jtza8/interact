; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :click)

(defclass vector-sprite (texture-sprite)
  ((texture :initform nil)))

(defmethod initialize-instance :around ((sprite vector-sprite) &key)
  (call-next-method)
  (provide-events sprite :texture-update)
  (update-texture sprite))

(defmethod update-texture ((sprite vector-sprite))
  (send-event sprite `(:texture-update :origin ,sprite)))