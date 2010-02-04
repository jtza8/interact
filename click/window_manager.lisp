; Copyright 2009 Jens Thiede. All rights reserved.
; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :click)

(define-condition no-window-manager-condition (error)
  ((window-manager :initform *window-manager*)))

(defclass window-manager ()
  ((windows :initform '()
            :reader windows)))

(defmacro active-window ()
  (car (last (windows *window-manager*))))

(defmacro assert-window-manager-exists ()
  '(assert (not (eq *window-manager* nil)) ()
    'no-window-manager-condition))

(defmethod add-window ((manager window-manager) window)
  (with-slots (windows) manager
    (when (find window windows)
      (return-from add-window))
    (if (> (length windows) 0)
        (setf (cdr (last windows)) (cons window nil))
        (push window windows))))

(defmethod remove-window ((manager window-manager) window)
  (with-slots (windows) manager
    (setf windows (delete window windows))))

(defmethod draw ((manager window-manager))
  (with-slots (windows) manager
    (dolist (window windows)
      (draw window))))

(defmethod handle-event ((manager window-manager) event)
  (with-slots (windows) manager
    (handle-event (car (last windows)) event)))