; Copyright 2009 Jens Thiede. All rights reserved.
; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :click)

(define-condition no-window-manager-condition (error)
  ((window-manager :initform *window-manager*)))

(defclass window-manager ()
  ((windows :initform '()
            :reader windows)
   (active-window :initform nil
                  :reader active-window)))

(defmacro assert-window-manager-exists ()
  '(assert (not (eq *window-manager* nil)) ()
    'no-window-manager-condition))

(defmethod add-window ((manager window-manager) window &key (set-active t))
  (with-slots (windows active-window) manager
    (when (find window windows)
      (return-from add-window))
    (if (> (length windows) 0)
        (setf (cdr (last windows)) (cons window nil))
        (push window windows))
    (when set-active
      (setf active-window window))))

(defmethod remove-window ((manager window-manager) window)
  (with-slots (windows active-window) manager
    (setf windows (delete window windows))
    (when (eq window active-window)
      (setf active-window (car (last windows))))))

(defmethod draw ((manager window-manager))
  (with-slots (windows) manager
    (dolist (window windows)
      (draw window))))

(defmethod handle-event ((manager window-manager) event)
  (with-slots (active-window windows) manager
    (handle-event active-window event)))