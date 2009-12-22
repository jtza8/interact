; Copyright 2009 Jens Thiede. All rights reserved.
; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :click)

(defclass dummy-listener ()
  ((latest-event :initform nil
                 :reader latest-event)))

(defmethod event-update ((listener dummy-listener) event &rest args)
  (setf (slot-value listener 'latest-event) `(,event ,@args)))