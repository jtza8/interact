; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :click)

(defclass screen-manager ()
  ((windows :initform '()
            :reader windows)
   (screens :initform '()
            :reader screens)
   (active-screens :initform nil
                   :reader active-screens)))

(defparameter *screen-manager* (make-instance 'screen-manager))

(defmethod sm-add-screen ((manager screen-manager) screen)
  (check-type screen screen)
  (with-slots (screens) manager
    (pushnew screen screens :test #'eq)))

(defmethod sm-remove-screen ((manager screen-manager) screen)
  (with-slots (screens active-screens) manager
    (delete screen screens)
    (delete screen active-screens)))

(defmethod draw ((manager screen-manager))
  (with-slots (screens) manager
    (dolist (screen screens)
      (draw screen))))

(defmethod send-event ((manager screen-manager) event)
  (with-slots (active-screens) manager
    (dolist (screen active-screens)
      (send-event screen event))))

(defmethod sm-activate-screen ((manager screen-manager) screen)
  (with-slots (screens active-screens) manager
    (assert (find screen screens) () 
            "Screen ~s not under the control of manager ~s" screen manager)
    (pushnew screen active-screens :test #'eq)))

(defmethod sm-deactivate-screen ((manager screen-manager) screen)
  (with-slots (screens active-screens) manager
    (delete screen active-screens)))

(defun add-screen (screen)
  (sm-add-screen click:*screen-manager* screen))

(defun remove-screen (screen)
  (sm-remove-screen click:*screen-manager* screen))

(defun activate-screen (screen)
  (sm-activate-screen click:*screen-manager* screen))

(defun deactivate-screen (screen)
  (sm-deactivate-screen click:*screen-manager* screen))

(defun reset-screen-manager ()
  (setf *screen-manager* (make-instance 'screen-manager)))