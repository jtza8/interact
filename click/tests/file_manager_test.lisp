; Copyright 2009 Jens Thiede. All rights reserved.
; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :click)

(defclass file-manager-test (test-case)
  ())

(def-test-method file_test ((test file-manager-test))
  (let ((manager (make-instance 'file-manager
                                :base-dir (getf *settings* :theme-path))))
    (assert-false (null manager))
    (assert-true (eq (class-of (getf (sub-managers manager) :window))
                     (find-class 'file-manager)))))