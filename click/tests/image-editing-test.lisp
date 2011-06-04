; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :click)

(defclass image-editing-test (test-case)
  ())

(def-test-method test-image-data-pos ((test image-editing-test))
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
      (il:with-bound-image src-image
        (il:load-image (merge-pathnames #p"src-image.png"
                                        *test-image-path*)))
      (il:clear-colour 0 0 0 0)
      (il:clear-image)
      (time (blit src-image 10 200 0 64 64 0 64 64 0))
      (blit src-image 470 200 0 64 0 0 64 64 0)
      (blit src-image 70 480 0 0 0 0 64 64 0)
      (il:enable :file-overwrite)
      (il:save-image (merge-pathnames #p"blit-test.png"
                                      *test-image-path*)))))

(def-test-method test-blit ((test image-editing-test))
  (il:with-images (src dst)
    (il:bind-image src)
    (il:tex-image 64 128 1 3 :bgr :unsigned-byte (cffi:null-pointer))
    (il:clear-colour 255 0 0 255)
    (il:clear-image)
    (il:bind-image dst)
    (il:tex-image 256 512 1 3 :rgb :unsigned-byte (cffi:null-pointer))
    (il:clear-colour 0 0 0 0)
    (il:clear-image)
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
      (il:with-bound-image src-image
        (il:enable :origin-set)
        (il:origin-func :origin-lower-left)
        (il:load-image (merge-pathnames #p"src-image.png"
                                        *test-image-path*)))
      (il:clear-colour 0 0 0 0)
      (il:clear-image)
      (time (overlay-image src-image 10 200 0))
      (il:enable :file-overwrite)
      (il:save-image (merge-pathnames #p"overlay-image-test.png"
                                      *test-image-path*)))))

(def-test-method test-overlay-image ((test image-editing-test))
  (il:with-images (src dst)
    (il:bind-image src)
    (il:tex-image 64 128 1 3 :bgr :unsigned-byte (cffi:null-pointer))
    (il:clear-colour 255 0 0 0)
    (il:clear-image)
    (il:bind-image dst)
    (il:tex-image 256 512 1 3 :rgb :unsigned-byte (cffi:null-pointer))
    (il:clear-colour 0 0 0 0)
    (il:clear-image)
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

(def-test-method test-write-and-read-pixel-header ((test image-editing-test))
  (il:with-images (image-1 image-2)
    (il:with-bound-image image-1
      (il:tex-image 192 1 1 2 :luminance-alpha :unsigned-byte 
                    (cffi:null-pointer)))
    (write-pixel-header image-1 '(:uint8 32) '(:uint32 42390) '(:int8 -123))
    (assert-equal '(32 42390 -123) (read-pixel-header image-1 :uint8
                                                      :uint32 :int8))
    (il:with-bound-image image-2
      (il:tex-image 1 1 1 1 :luminance :unsigned-byte (cffi:null-pointer)))
    (assert-condition 'error (write-pixel-header image-2 '(:uint16 8)))
    (write-pixel-header image-2 '(:uint8 8))))

(def-test-method test-vecto-flip ((test image-editing-test))
  (let* ((width 5)
         (height 5)
         (channels 4)
         (data (make-array (* height width channels) :element-type 'fixnum))
         flipped-data)
    (dotimes (index (length data))
      (setf (aref data index) index))
    (setf flipped-data (vecto-flip data width channels))
    (assert-equal (coerce (subseq data (* (1- height) width channels)
                                  (* height width channels)) 'list)
                  (coerce (subseq flipped-data 0 (* width channels)) 'list))))