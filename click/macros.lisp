; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :click)

(internal define-global-settings)
(defmacro define-global-settings ((global-variable) &body helper-args)
  `(progn
     (defparameter ,global-variable (make-hash-table))
     ,@(loop for (function-name key default-value setter-mode) in helper-args
             when (null setter-mode) do (setf setter-mode :read-write)
             collect `(progn
                        (setf (gethash ,key ,global-variable)
                              ,default-value)
                        ,@(when (find setter-mode '(:read :read-write))
                            `((declaim (inline ,function-name))
                              (defun ,function-name ()
                                (gethash ,key ,global-variable))))
                        ,(when (find setter-mode '(:write :read-write))
                           `(defsetf ,function-name () (value)
                              `(setf (gethash ,,key *display-settings*)
                                      ,value)))))))
