; Copyright 2009 Jens Thiede. All rights reserved.
; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :click)

(defvar *window-manager* nil)

(define-condition no-window-manager-condition (error)
  ((window-manager :initform *window-manager*)))

(defclass window-manager ()
  ((windows :initform '())
   (focus :initform nil)))

(defun init-window-manager ()
  (setf *window-manager* (make-instance 'window-manager)))

(defmacro assert-window-manager-exists ()
  '(assert (not (eq *window-manager* nil)) ()
    'no-window-manager-condition))
