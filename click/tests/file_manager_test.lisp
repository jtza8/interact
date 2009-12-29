; Copyright 2009 Jens Thiede. All rights reserved.
; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :click)

(defclass file-manager-test (test-case)
  ((file-manager :initform nil)))

(defmethod set-up ((test file-manager-test))
  (setf (slot-value test 'file-manager)
        (make-instance 'file-manager
                       :base-dir (getf *settings* :theme-path))))

(def-test-method tree-child-test ((test file-manager-test))
  (with-slots (file-manager) test
    (assert-equal 3 (length (files (tree-child file-manager
                                               :window :title-bar))))))

(def-test-method init-test ((test file-manager-test))
  (with-slots (file-manager) test
    (assert-false (null file-manager))
    (assert-true (eq (class-of (tree-child file-manager :window))
                     (find-class 'file-manager))
                 (format nil "Managers are: ~S" (managers file-manager)))
    (assert-equal '() (files file-manager))
    (dolist (file (files (tree-child file-manager :window :title-bar)))
      (assert-true (cl-fad:file-exists-p file)))))
