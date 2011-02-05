; Copyright 2011 Jens Thiede. All rights reserved.
; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :click)

(defparameter *test-image-sequence-path*
  (merge-pathnames "sequence*.png"
                   (asdf:system-relative-pathname :click-tests
                                                  "test_sequence/")))

(defclass sprite-sheet-test (test-case)
  ())

(def-test-method test-list-image-file-sequence ((test sprite-sheet-test))
  (let ((paths (list-image-file-sequence *test-image-sequence-path*)))
    (assert-equal (length paths) 20)
    (loop for path in paths
          for i upfrom 1
          do (assert-true (string= (format nil "sequence~4,'0d.png" i)
                                   (file-namestring path))))))

(def-test-method test-open-image-sequence ((test sprite-sheet-test))
  (let* ((path-list (list-image-file-sequence *test-image-sequence-path*))
         (image-sequence (open-image-sequence path-list)))
    (apply #'il:delete-images image-sequence)
    (open-image-sequence 
     (cons (merge-pathnames "sequence-bogus.png"
                            (directory-namestring *test-image-sequence-path*))
           path-list))))
    