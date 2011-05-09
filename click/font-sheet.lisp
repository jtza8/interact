; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :click)

(defun write-bitmap-font-header (ascii-offset glyph-count glyph-width
                                 glyph-height &optional (image :current-image))
  (write-pixel-header image 
                      `(1 ,ascii-offset) `(1 ,glyph-count) `(1 ,glyph-width)
                      `(1 ,glyph-height)))

(defun read-bitmap-font-header (&optional (image :current-image))
  (let ((values (read-pixel-header image 1 1 1 1)))
    (list :ascii-offset (pop values)
          :glyph-count (pop values)
          :glyph-width (pop values)
          :glyph-height (pop values))))

(defun stamp-font-sheet (file-name glyph-width glyph-height
                         &optional (ascii-offset 33) (glyph-count 93))
  (il:with-images (image)
    (il:with-bound-image image
      (check-file-existance file-name file-name)
      (il:load-image (namestring file-name))
      (il:clear-colour 0 0 0 0)
      (ilu:image-parameter :placement :lower-left)
      (ilu:enlarge-canvas (il:image-width) (1+ (il:image-height)) 1)
      (write-bitmap-font-header ascii-offset glyph-count
                                glyph-width glyph-height)
      (il:enable :file-overwrite)
      (il:save-image (namestring file-name)))))

(defun load-font-sheet (file)
  (assert (cl-fad:file-exists-p file) (file)
          'file-existance-error
          :file-name (namestring file))
  (il:with-images (font-sheet)
    (il:with-bound-image font-sheet
      (il:load-image (namestring file))
      (let ((header (read-bitmap-font-header)))
        ()))))