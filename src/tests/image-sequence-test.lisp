; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :interact)

(defparameter *test-image-sequence-path*
  (asdf:system-relative-pathname :interact-tests
                                 "test-sequence/"))

(defclass image-sequence-test (test-case)
  ())

(def-test-method test-list-image-file-sequence ((test image-sequence-test))
  (let ((paths (list-image-file-sequence 
                (merge-pathnames "sequence-*.png" 
                                 *test-image-sequence-path*))))
    (assert-equal 10 (length paths))
    (loop for path in paths
          for i upfrom 0
          do (assert-equal(format nil "sequence-~2,'0d.png" i)
                          (file-namestring path)))))

(def-test-method test-open-image-sequence ((test image-sequence-test))
  (let* ((sequence-path-pattern (merge-pathnames "sequence-*.png" 
                                                 *test-image-sequence-path*))
         (path-list (list-image-file-sequence sequence-path-pattern))
         (bogus-file (merge-pathnames "sequence-bogus.png"
                                      *test-image-sequence-path*)))
    (assert-condition 'image-dimensions-error
                      (apply #'il:delete-images
                             (open-image-sequence (cons bogus-file path-list))))
    (let ((image-sequence (open-image-sequence path-list)))
      (unwind-protect (assert-true (numberp (car image-sequence)))
        (apply #'il:delete-images image-sequence)))))
