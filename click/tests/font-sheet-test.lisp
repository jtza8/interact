; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :click)

(defclass font-sheet-test (test-case)
  ())

(def-test-method test-bitmap-font-header ((test font-sheet-test))
  (il:with-images (image)
    (il:with-bound-image image
      (il:tex-image 5 1 1 1 :luminance :unsigned-byte (cffi:null-pointer)))
    (write-bitmap-font-header 33 94 12 16 -1 image)
    (let ((header (read-bitmap-font-header image)))
      (assert-equal 33 (getf header :ascii-offset))
      (assert-equal 94 (getf header :glyph-count))
      (assert-equal 12 (getf header :glyph-width))
      (assert-equal 16 (getf header :glyph-height))
      (assert-equal -1 (getf header :tracking)))))

