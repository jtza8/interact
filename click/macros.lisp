; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :click)

(internal define-global-settings)
(defmacro define-global-settings ((global-variable) &body helper-args)
  `(progn
     (defparameter ,global-variable
       (make-array 5 :adjustable t :fill-pointer 0))
     ,@(loop for (function-name default-value setter-mode) in helper-args
             for set-function-name = (intern (format nil "SET-~s"
                                                     function-name))
             for index upfrom 0
             when (null setter-mode) do (setf setter-mode :read-write)
             collect `(progn
                        (vector-push-extend ,default-value ,global-variable)
                        ,@(when (find setter-mode '(:read :read-write))
                            `((declaim (inline ,function-name
                                               ,set-function-name))
                              (defun ,function-name ()
                                (aref ,global-variable ,index))
                              (internal ,set-function-name)
                              (defun ,set-function-name (value)
                                (setf (aref ,global-variable ,index) value))))
                        ,(when (find setter-mode '(:write :read-write))
                           `(defsetf ,function-name ,set-function-name))))))

(defmacro define-instance-maker (class-name)
  (let ((instance-maker (intern (format nil "MAKE-~s" class-name))))
    `(progn (declaim (inline ,instance-maker))
            (defun ,instance-maker
                (&rest args)
              (apply #'make-instance ',class-name args)))))
