; Copyright 2009 Jens Thiede. All rights reserved.
; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :click)

(define-condition no-window-manager (error)
  ((window-manager :initform *window-manager*)))

(define-condition invalid-window (error)
  ((window :initarg :window
           :reader window)
   (manager :initarg :manager
            :reader manager)))

(defclass window-manager ()
  ((windows :initform '()
            :reader windows)
   (active-window :initform nil
                  :reader active-window)))

(defmacro assert-window-manager-exists ()
  '(assert (not (eq *window-manager* nil)) () 'no-window-manager))

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

(defmethod (setf active-window) (window (manager window-manager))
  (with-slots (active-window windows) manager
    (unless (find window windows :test #'eq)
      (error 'invalid-window :window window :manager manager))
    (setf windows (delete window windows)
          (cdr (last windows)) (list window)
          active-window window)))

(defmethod send-event ((manager window-manager) event)
  (with-slots (active-window windows) manager
    (when (eq (event-type event) :mouse-down)
      (with-event-keys (x y) event
        (unless (within active-window x y)
          (dolist (window (copy-seq windows))
            (when (within window x y)
              (setf (active-window manager) window))))))
    (send-event active-window event)))