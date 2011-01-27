; Copyright 2009 Jens Thiede. All rights reserved.
; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :click)

(defparameter *settings* nil)
(defparameter *sprite-path* nil)
(defparameter *screen-manager* nil)
(defparameter *sprite-tree* nil)

(defun init-click (&key (settings *settings*))
  (when (null *sprite-path*)
    (error "Sprite path not set."))
  (il:init)
  (setf *screen-manager* (make-instance 'screen-manager)
        *sprite-tree* (make-sprite-tree (getf settings :sprite-path))))
