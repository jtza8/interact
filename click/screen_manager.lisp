; Copyright 2009 Jens Thiede. All rights reserved.
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

(defmethod add-screen ((manager screen-manager) screen)
  (with-slots (screens) manager
    (pushnew screen screens :test #'eql)))

(defmethod remove-screen ((manager screen-manager) screen)
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