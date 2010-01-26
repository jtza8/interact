; Copyright 2009 Jens Thiede. All rights reserved.
; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :click)

(defparameter *settings* '())
(defparameter *theme-path* nil)
(defparameter *window-manager* nil)
(defparameter *theme-image-tree* nil)

(defun load-settings ()
  (let ((clickrc-file
         (merge-pathnames ".clickrc" (user-homedir-pathname))))
    (unless (cl-fad:file-exists-p clickrc-file)
      (set-defaults)
      (return-from load-settings))
    (with-open-file (settings clickrc-file)
      (setf *settings* (read settings)))))

(defun set-defaults ()
  (setf *settings*
        (list :theme-path 
              (asdf:system-relative-pathname :click "default_theme"))))

(defun init-click ()
  (setf *window-manager* (make-instance 'window-manager)
        *theme-image-tree* (make-image-tree (getf *settings* :theme-path))))

(load-settings)
