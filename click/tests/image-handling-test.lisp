; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :click)

(defparameter *test-image-sequence-path*
  (asdf:system-relative-pathname :click-tests
                                 "test-sequence/"))

(defparameter *test-image-path*
  (asdf:system-relative-pathname :click-tests "test-images/"))

(defclass sprite-sheet-test (test-case)
  ())

(def-test-method test-list-image-file-sequence ((test sprite-sheet-test))
  (let ((paths (list-image-file-sequence 
                (merge-pathnames "sequence*.png" 
                                 *test-image-sequence-path*))))
    (assert-equal 20 (length paths))
    (loop for path in paths
          for i upfrom 1
          do (assert-true (string= (format nil "sequence~4,'0d.png" i)
                                   (file-namestring path))))))

(def-test-method test-open-image-sequence ((test sprite-sheet-test))
  (let* ((sequence-path-pattern (merge-pathnames "sequence*.png" 
                                                 *test-image-sequence-path*))
         (path-list (list-image-file-sequence sequence-path-pattern))
        (bogus-file (merge-pathnames "sequence-bogus.png"
                                     *test-image-sequence-path*)))
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
    (assert-condition 'pixel-index-error (image-data-pos 101 100))
    (assert-condition 'pixel-index-error (image-data-pos 100 101))
    (assert-condition 'pixel-index-error (image-data-pos -1 0))))

(defun test-blit-manually ()
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
      (time (blit src-image 10 200 0 64 64 0 64 64 0))
      (blit src-image 470 200 0 64 0 0 64 64 0)
      (blit src-image 70 480 0 0 0 0 64 64 0)
      (il:enable :file-overwrite)
      (il:save-image (merge-pathnames #p"blit-test.png"
                                      *test-image-path*)))))

(def-test-method test-blit ((test sprite-sheet-test))
  (il:with-images (src dst)
    (il:bind-image src)
    (il:tex-image 64 128 1 3 :bgr :unsigned-byte (cffi:null-pointer))
    (il:clear-image 255 0 0)
    (il:bind-image dst)
    (il:tex-image 256 512 1 3 :rgb :unsigned-byte (cffi:null-pointer))
    (il:clear-image 0 0 0)
    (blit src 10 12 0 0 0 0 64 128 1)
    (assert-equal :bgr (il:image-format src))
    (assert-condition 'pixel-index-error (blit src 0 0 0 0 0 0 65 128 0))
    (blit src 192 0 0 0 0 0 64 128 1)
    (blit src 193 0 0 0 0 0 64 128 1)
    (assert-condition 'pixel-index-error 
                      (blit src 193 0 0 0 0 0 64 128 1 :allow-clipping nil))
    (blit src 0 384 0 0 0 0 64 128 1)
    (blit src 0 385 0 0 0 0 64 128 1)
    (assert-condition 'pixel-index-error 
                      (blit src 0 385 0 0 0 0 64 128 1 :allow-clipping nil))))

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
      (time (overlay-image src-image 10 200 0))
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
    (overlay-image src 10 12 0)
    (assert-equal :bgr (il:image-format src))
    (overlay-image src 192 0 0)
    (assert-condition 'pixel-index-error 
                      (overlay-image src 193 0 0 :allow-clipping nil))
    (overlay-image src 0 384 0)
    (assert-condition 'pixel-index-error
                      (overlay-image src 0 385 0 :allow-clipping nil))
    (il:bind-image src)
    (assert-condition 'pixel-index-error 
                      (overlay-image dst 0 0 0 :allow-clipping nil))))

(def-test-method test-sheet-header ((test sprite-sheet-test))
  (il:with-images (test)
    (il:with-bound-image test
      (il:tex-image 64 64 1 4 :rgba :unsigned-byte (cffi:null-pointer))
      (il:clear-image 0 0 0 0)
      (write-sheet-header 8 7 12 60 t)
      (let ((header (read-sheet-header)))
        (print header)
        (assert-equal 8 (getf header :frame-width))
        (assert-equal 7 (getf header :frame-height))
        (assert-equal 60 (getf header :frame-count))
        (assert-equal 12 (getf header :fps))))))

(defun test-build-sprite-sheet-manually ()
  (build-sprite-sheet (merge-pathnames #p"sequence*.png"
                                       *test-image-sequence-path*)
                      24
                      :file-overwrite t
                      :sheet-file-name 
                      (merge-pathnames "sequence-test.ss.png"
                                       *test-image-sequence-path*)))

(def-test-method test-load-sprite-sheet ((test sprite-sheet-test))
  (load-sprite-sheet (merge-pathnames "test-sheet.ss.png" *test-image-path*)))
