; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :click)

(defun write-bitmap-font-header (ascii-offset glyph-count glyph-width
                                 glyph-height tracking &optional
                                 (image :current-image))
  (write-pixel-header image 
                      `(:uint8 ,ascii-offset) `(:uint8 ,glyph-count)
                      `(:uint8 ,glyph-width) `(:uint8 ,glyph-height)
                      `(:int8 ,tracking)))

(defun read-bitmap-font-header (&optional (image :current-image))
  (let ((values (read-pixel-header image :uint8 :uint8 :uint8 :uint8 :int8)))
    (list :ascii-offset (pop values)
          :glyph-count (pop values)
          :glyph-width (pop values)
          :glyph-height (pop values)
          :tracking (pop values))))

(defun stamp-font-sheet (file-name glyph-width glyph-height &key
                         (ascii-offset 33) (glyph-count 93) (tracking -1)
                         overwrite-header)
  (il:with-images (image)
    (il:with-bound-image image
      (check-file-existance file-name)
      (il:load-image (namestring file-name))
      (unless overwrite-header
        (il:clear-colour 0 0 0 0)
        (ilu:image-parameter :placement :lower-left)
        (ilu:enlarge-canvas (il:image-width) (1+ (il:image-height)) 1))
      (write-bitmap-font-header ascii-offset glyph-count
                                glyph-width glyph-height
                                tracking)
      (il:enable :file-overwrite)
      (il:save-image (namestring file-name)))))

(defun load-font-sheet (file)
  (check-file-existance file)
  (il:with-images (font-sheet glyph)
    (il:with-bound-image font-sheet
      (il:enable :origin-set)
      (il:origin-func :origin-lower-left)
      (il:load-image (namestring file))
      (let* ((header (read-bitmap-font-header))
             (ascii-offset (getf header :ascii-offset))
             (glyph-width (getf header :glyph-width))
             (glyph-height (getf header :glyph-height))
             (glyph-count (getf header :glyph-count))
             (tracking (getf header :tracking))
             (glyph-ideal-width (power-size glyph-width 2))
             (glyph-ideal-height (power-size glyph-height 2))
             (image-type (il:image-type))
             (image-format (il:image-format))
             (image-bpp (il:image-bytes-per-pixel))
             (image-width (il:image-width))
             (image-height (il:image-height))
             (glyph-cols (/ image-width glyph-width))
             (glyph-rows (/ (1- image-height) glyph-height))
             (glyph-vector (make-array glyph-count :fill-pointer 0))
             (counter 0))
        (assert (and (integerp glyph-cols) (integerp glyph-rows)))
        (tagbody
           (dotimes (y glyph-rows)
             (dotimes (x glyph-cols)
               (il:with-bound-image glyph
                 (il:tex-image glyph-ideal-width glyph-ideal-height 1 image-bpp
                               image-format image-type (cffi:null-pointer))
                 (il:clear-image)
                 (blit font-sheet 0 0 0
                       (* glyph-width x)
                       (- image-height (* glyph-height (1+ y)) 1) 0
                       glyph-width
                       glyph-height 0)
                 ;; (il:save-image (format nil "~0,4d.png" i))
                 (vector-push (image-to-sprite t) glyph-vector)
                 (when (= (incf counter) glyph-count)
                   (go end)))))
           end)
        (make-instance 'bitmap-font-sprite
                       :width 0 :height 0
                       :glyph-width glyph-width
                       :glyph-height glyph-height
                       :ascii-offset ascii-offset
                       :glyph-vector glyph-vector
                       :tracking tracking)))))
