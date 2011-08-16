; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :interact)

(defmacro define-instance-maker (class-name)
  (let ((instance-maker (intern (format nil "MAKE-~s" class-name))))
    `(progn (declaim (inline ,instance-maker))
            (defun ,instance-maker
                (&rest args)
              (apply #'make-instance ',class-name args)))))

(internal with-try-again-restart)
(defmacro with-try-again-restart ((&optional
                                   (description "Re-evaluate relevant code."))
                                  &body body)
  (let ((again (gensym "TAG-")))
    `(tagbody
      ,again
        (restart-case (progn ,@body)
          (try-again () :report ,description (go ,again))))))
