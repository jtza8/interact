; Copyright 2009 Jens Thiede. All rights reserved.
; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :click)

(defvar *settings* '())
(defvar *theme-path* nil)

(defun load-settings ()
  (let ((clickrc-file
         (merge-pathnames ".clickrc" (user-homedir-pathname))))
    (unless (cl-fad:file-exists-p clickrc-file)
      (return-from load-settings))
    (with-open-file (settings clickrc-file)
      (setf *settings* (read settings)))))

(defun configure-click ()
  (setf *theme-path* (make-instance 'file-manager
                                    :base-dir (getf *settings* :theme-path))))

(load-settings)
(configure-click)