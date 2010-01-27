; Copyright 2009 Jens Thiede. All rights reserved.
; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :click)

(defclass listener ()
  ((listen-for-events :initform '()
                      :reader listen-for-events)))

(defmethod select-handler ((listener listener) event-type) nil)
