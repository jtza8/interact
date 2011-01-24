; Copyright 2009 Jens Thiede. All rights reserved.
; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :click)

(define-condition no-screen-manager (error)
  ((screen-manager :initform *screen-manager*)))

(define-condition invalid-window (error)
  ((window :initarg :window
           :reader window)
   (manager :initarg :manager
            :reader manager)))

(defclass screen-manager ()
  ((windows :initform '()
            :reader windows)
   (screens :initform '()
            :reader screens)
   (active-window :initform nil
                  :reader active-window)))

(defmacro assert-screen-manager-exists ()
  '(assert (not (eq *screen-manager* nil)) () 'no-screen-manager))

(defmethod add-window ((manager screen-manager) window &key (set-active t))
  (with-slots (windows active-window) manager
    (when (find window windows)
      (return-from add-window))
    (if (> (length windows) 0)
        (setf (cdr (last windows)) (cons window nil))
        (push window windows))
    (when set-active
      (setf active-window window))))

(defmethod add-screen ((manager screen-manager) screen)
  (with-slots (screens) manager
    (pushnew screen screens :test #'eql)))

(defmethod remove-window ((manager screen-manager) window)
  (with-slots (windows active-window) manager
    (setf windows (delete window windows))
    (when (eq window active-window)
      (setf active-window (car (last windows))))))

(defmethod draw ((manager screen-manager))
  (with-slots (screens windows) manager
    (dolist (screen screens)
      (draw screen))
    (dolist (window windows)
      (draw window))))

(defmethod (setf active-window) (window (manager screen-manager))
  (with-slots (active-window windows) manager
    (unless (find window windows :test #'eq)
      (error 'invalid-window :window window :manager manager))
    (setf windows (delete window windows)
          active-window window
          (cdr (last windows)) (list window))))

(defmethod send-event ((manager screen-manager) event)
  (with-slots (active-window windows) manager
    (unless active-window
      (return-from send-event))
    (when (eq (event-type event) :mouse-down)
      (with-event-keys (x y) event
        (unless (within active-window x y)
          (dolist (window (reverse windows))
            (when (within window x y)
              (setf (active-window manager) window)
              (return))))))
    (send-event active-window event)))