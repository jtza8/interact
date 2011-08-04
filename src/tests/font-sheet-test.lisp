; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :interact)

(defclass font-sheet-test (test-case)
  ())

(def-test-method test-font-sheet-header ((test font-sheet-test))
  (il:with-images (image)
    (il:with-bound-image image
      (il:tex-image 5 1 1 1 :luminance :unsigned-byte (cffi:null-pointer)))
    (write-font-sheet-header 12 16 33 94 -1 image)
    (let ((header (read-font-sheet-header image)))
      (assert-equal 33 (getf header :ascii-offset))
      (assert-equal 94 (getf header :glyph-count))
      (assert-equal 12 (getf header :glyph-width))
      (assert-equal 16 (getf header :glyph-height))
      (assert-equal -1 (getf header :tracking)))))

