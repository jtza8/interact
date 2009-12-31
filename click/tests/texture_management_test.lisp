; Copyright 2009 Jens Thiede. All rights reserved.
; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :click)

(defparameter *test-theme-path* (asdf:system-relative-pathname
                                 :click "tests/test_theme"))

(defclass texture-manager-test (test-case)
  ((texture-manager :initform nil)))


(def-test-method test-make-texture-tree ((test texture-manager-test))
  (let ((tree (make-texture-tree *test-theme-path*)))
    (assert-true (not (null (getf tree :window))))
    (assert-true (not (null (getf (getf tree :window) :shadow))))
    (assert-true 
     (not (null (getf (getf (getf tree :window) :shadow) :left-01))))))

(def-test-method test-fetch-texture ((test texture-manager-test))
  (setf *theme-texture-tree* (make-texture-tree *test-theme-path*))
  (assert-true (not (null (fetch-texture :window :shadow :left-01)))))