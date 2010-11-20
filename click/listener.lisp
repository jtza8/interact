; Copyright 2009 Jens Thiede. All rights reserved.
; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :click)

(defclass listener ()
  ((desired-events :initform '()
                   :reader desired-events)))


(defmethod select-handler ((listener listener) event-type)
  (getf (slot-value listener 'desired-events) event-type))