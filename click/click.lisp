; Copyright 2009 Jens Thiede. All rights reserved.
; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :click)

(defparameter *settings* nil)
(defparameter *sprite-path* nil)
(defparameter *screen-manager* nil)
(defparameter *sprite-tree* nil)

(defun reset-settings ()
  (setf *settings* 
        (list 
         :sprite-path (asdf:system-relative-pathname :click "../gui/sprites")
         :screen-size '(800 600))))

(defun merge-plists (dominant subserviant)
  (let ((default (gensym "DEFAULT"))
	(merged (copy-seq dominant)))
    (loop
       for (key value) on subserviant by #'cddr
       when (eq default (getf merged key default)) do
	 (setf (getf merged key) value)
       finally
	 (return merged))))

(defun load-settings ()
  (let ((clickrc-file
         (merge-pathnames ".clickrc" (user-homedir-pathname))))
    (unless (cl-fad:file-exists-p clickrc-file)
      (return-from load-settings))
    (with-open-file (settings clickrc-file)
      (setf *settings* (merge-plists (read settings) *settings*)))))

(defun init-click (&key (settings *settings*))
  (il:init)
  (setf *screen-manager* (make-instance 'screen-manager)
        *sprite-tree* (make-sprite-tree (getf settings :sprite-path))))

(reset-settings)
