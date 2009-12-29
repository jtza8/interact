; Copyright 2009 Jens Thiede. All rights reserved.
; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :click)

(defclass file-manager ()
  ((root-path :initarg :root-path
              :initform #P"."
              :reader root-path)))

(defmethod initialize-instance :after ((manager file-manager) &key)
  (with-slots (root-path) manager
    (assert (directory-exists-p root-path)
            (root-path)
            (format nil
                    "Couldn't initialize ~S. Directory ~S doesn't exist."
                    manager
                    root-path))))

;(defmethod