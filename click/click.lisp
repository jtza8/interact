; Copyright 2009 Jens Thiede. All rights reserved.
; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :click)

(defvar *settings* '())

(defun load-settings ()
  (let ((clickrc-file
         (merge-pathnames ".clickrc" (user-homedir-pathname))))
    (print clickrc-file)
    (unless (cl-fad:file-exists-p clickrc-file)
      (return-from load-settings))
    (with-open-file (settings clickrc-file)
      (setf *settings* (read settings)))))

(load-settings)    