; Copyright 2011 Jens Thiede. All rights reserved.
; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :click)

(defparameter *test-image-sequence-path*
  (merge-pathnames "sequence*.png"
                   (asdf:system-relative-pathname :click-tests
                                                  "test_sequence/")))

(defparameter *test-image-path*
  (asdf:system-relative-pathname :click-tests "test_images/"))

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
  (let ((path-list (list-image-file-sequence *test-image-sequence-path*))
        (bogus-file (merge-pathnames "sequence-bogus.png"
                     (directory-namestring *test-image-sequence-path*))))
    (assert-condition 'image-sequence-error
                      (apply #'il:delete-images
                             (open-image-sequence (cons bogus-file path-list))))
    (let ((image-sequence (open-image-sequence path-list)))
      (unwind-protect (assert-true (numberp (car image-sequence)))
        (apply #'il:delete-images image-sequence)))))

(defun test-overlay-image ()
  (let ((dest-width 500)
        (dest-height 500)
        (bytes-per-pixel 4)
        (dest-format :rgba)
        (dest-type :unsigned-byte))
    (il:with-images (dest-image src-image)
      (il:bind-image dest-image)
      (il:tex-image dest-width dest-height 1 bytes-per-pixel dest-format
                    dest-type (cffi:null-pointer))
      (il:check-error)
      (il:with-bound-image src-image
        (il:load-image (merge-pathnames #p"src-image.png"
                                        *test-image-path*))
        (il:check-error))
      (clear-image 0 0 0 0)
      (time (overlay-image src-image 10 200))
      (il:enable :file-overwrite)
      (il:save-image (merge-pathnames #p"dest-image.png"
                                      *test-image-path*)))))