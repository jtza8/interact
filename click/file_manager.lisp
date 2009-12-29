; Copyright 2009 Jens Thiede. All rights reserved.
; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :click)

(defclass file-manager ()
  ((base-dir :initarg :base-dir
              :initform #P"."
              :reader base-dir)
   (managers :initform '()
             :reader managers)
   (files :initform '()
          :reader files)))

(defmethod initialize-instance :after ((manager file-manager) &key)
  (with-slots (base-dir managers files) manager
    (assert (cl-fad:directory-exists-p base-dir)
            (base-dir)
            (format nil
                    "Couldn't initialize ~S. Directory ~S doesn't exist."
                    manager
                    base-dir))
    (loop
       for item in (cl-fad:list-directory base-dir)
       if (cl-fad:directory-exists-p item)
         do (let* ((path (car (last (pathname-directory item))))
                   (string (nsubstitute #\- #\_ (string-upcase path)))
                   (keyword (intern string "KEYWORD"))
                   (instance (make-instance 'file-manager :base-dir item)))
              (setf managers (cons keyword (cons instance managers))))
       else
         do (push item files))))

(defmethod tree-child ((manager file-manager) &rest path)
  (loop
     with child = manager
     for keyword in path
       do (setf child (getf (managers child) keyword))
     finally (return child)))
     