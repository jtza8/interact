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

(def-test-method test-image-data-pos ((test sprite-sheet-test))
  (il:with-images (image)
    (il:bind-image image)
    (il:tex-image 100 100 1 3 :rgb :unsigned-byte (cffi:null-pointer))
    (assert-false (cffi-sys:null-pointer-p (image-data-pos 0 0)))
    (assert-false (cffi-sys:null-pointer-p (image-data-pos 100 100)))
    (assert-condition 'image-data-index-error (image-data-pos 101 100))
    (assert-condition 'image-data-index-error (image-data-pos 100 101))
    (assert-condition 'image-data-index-error (image-data-pos -1 0))))

(defun test-overlay-image-manually ()
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
      (il:clear-image 0 0 0 0)
      (time (overlay-image src-image 10 200))
      (il:enable :file-overwrite)
      (il:save-image (merge-pathnames #p"overlay-image-test.png"
                                      *test-image-path*)))))

(def-test-method test-overlay-image ((test sprite-sheet-test))
  (il:with-images (src dst)
    (il:bind-image src)
    (il:tex-image 64 128 1 3 :bgr :unsigned-byte (cffi:null-pointer))
    (il:clear-image 255 0 0)
    (il:bind-image dst)
    (il:tex-image 256 512 1 3 :rgb :unsigned-byte (cffi:null-pointer))
    (il:clear-image 0 0 0)
    (overlay-image src 10 12)
    (assert-equal :bgr (il:image-format src))
    (overlay-image src 192 0)
    (assert-condition 'image-data-index-error (overlay-image src 193 0))
    (overlay-image src 0 384)
    (assert-condition 'image-data-index-error (overlay-image src 0 385))
    (il:bind-image src)
    (assert-condition 'image-data-index-error (overlay-image dst 0 0))))
