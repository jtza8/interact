; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :click)

(internal memset)
(cffi:defcfun ("memset" memset) :pointer
  (dest :pointer) (value :int) (size :unsigned-int))

(internal write-sheet-header)
(defun write-sheet-header (frame-width frame-height frame-count fps looping 
                           &optional (image :current-image))
  (il:with-bound-image image
    (let* ((width (il:image-width))
           (height (il:image-height))
           (bytes-per-pixel (il:image-bytes-per-pixel))
           (header-byte-size (* 5 bytes-per-pixel))
           (pixel-count (ceiling (/ header-byte-size bytes-per-pixel)))
           (header-pixel-size (* pixel-count bytes-per-pixel))
           (header-row-size (* width bytes-per-pixel))
           (pointer (ecase (il:image-origin)
                      (:origin-lower-left (image-data-pos 0 (1- height)))
                      (:origin-upper-left (image-data-pos 0 0)))))
      (assert (>= header-row-size header-pixel-size) ()
              "too little space to write header in")
      (memset pointer #xff header-row-size)
      (dolist (item (list frame-width frame-height frame-count
                          fps (if looping #x0001 #x0000)))
        (setf (cffi:mem-aref pointer :uint16) (- #xffff item))
        (cffi:incf-pointer pointer bytes-per-pixel)))))

(internal read-sheet-header)
(defun read-sheet-header (&optional (image :current-image))
  (il:with-bound-image image
    (let* ((height (il:image-height))
           (bytes-per-pixel (il:image-bytes-per-pixel))
           (pointer (ecase (il:image-origin)
                      (:origin-lower-left (image-data-pos 0 (1- height)))
                      (:origin-upper-left (image-data-pos 0 0)))))
      (flet ((read-value () 
               (let ((value (- #xffff (cffi:mem-aref pointer :uint16))))
                 (cffi:incf-pointer pointer bytes-per-pixel)
                 value)))
        (list* :frame-width (read-value)
               :frame-height (read-value)
               :frame-count (read-value)
               :fps (read-value)
               (let ((flags (read-value)))
                 (list :looping (logbitp 0 flags))))))))
           

(defun build-sprite-sheet (sequence-path fps &key (looping t) (max-columns 5)
                           sheet-file-name file-overwrite)
  (with-image-sequence (sequence (list-image-file-sequence sequence-path))
    (il:bind-image (car sequence))
    (let* ((frame-width (il:image-width))
           (frame-height (il:image-height))
           (frame-count (length sequence))
           (width (* max-columns frame-width))
           (height (1+ (* frame-height
                          (ceiling (/ frame-count max-columns)))))
           (bytes-per-pixel (il:image-bytes-per-pixel))
           (data-type (il:image-type))
           (pixel-format (il:image-format)))
      (il:with-images (sprite-sheet)
        (il:bind-image sprite-sheet)
        (il:tex-image width height 1 bytes-per-pixel pixel-format
                      data-type (cffi:null-pointer))
        (ecase pixel-format
          (:luminance (il:clear-image 0))
          (:luminance-alpha (il:clear-image 0 0))
          (:rgb (il:clear-image 0 0 0))
          (:rgba (il:clear-image 0 0 0 0)))
        (write-sheet-header frame-width frame-height frame-count fps looping)
        (tagbody
          (let ((sequence-pointer sequence))
            (loop for sequence-y from (1- (ceiling (/ frame-count max-columns)))
                      downto 0
                  do (dotimes (sequence-x (min max-columns frame-count))
                       (when (null sequence-pointer)
                         (go end-loops))
                       (overlay-image (pop sequence-pointer)
                                      (* sequence-x frame-width)
                                      (* sequence-y frame-height)
                                      0 :allow-clipping nil))))
           end-loops)
        (when (null sheet-file-name)
          (setf sheet-file-name 
                (ppcre:regex-replace "[-_ ]?\\*"
                                     (namestring sequence-path) ".ss")))
        (if file-overwrite
            (il:enable :file-overwrite)
            (il:disable :file-overwrite))
        (il:save-image sheet-file-name)))))

(defun load-sprite-sheet (path)
  (assert (fad:file-exists-p path) ()
          "File doesn't exist: ~a" path)
  (il:with-images (sheet)
    (il:bind-image sheet)
    (il:enable :origin-set)
    (il:origin-func :origin-lower-left)
    (il:load-image path)
    (il:check-error)
    (let* ((width (il:image-width))
           (height (il:image-height))
           (image-format (il:image-format))
           (image-type (il:image-type))
           (bytes-per-pixel (il:image-bytes-per-pixel))
           (header (read-sheet-header))
           (frame-width (getf header :frame-width))
           (frame-height (getf header :frame-height))
           (frame-count (getf header :frame-count))
           (fps (getf header :fps))
;           (looping (getf header :looping))
           (sprite-vector (make-array frame-count))
           (images-left frame-count))
      (tagbody
        (loop for y from (1- (truncate (/ height frame-height))) downto 0
              do (dotimes (x (truncate (/ width frame-width)))
                   (if (= images-left 0)
                       (go end-loops)
                       (il:with-images (image)
                         (il:bind-image image)
                         (il:tex-image frame-width frame-height 0
                                       bytes-per-pixel image-format
                                       image-type (cffi:null-pointer))
                         (blit sheet 0 0 0 (* x frame-width) (* y frame-height)
                               0 frame-width frame-height 0)
                         (ilu:flip-image)
                         (setf (aref sprite-vector (- frame-count images-left))
                               (image-to-sprite))
                         (decf images-left)))))
       end-loops)
      (make-instance 'animation-sprite
                     :sprite-vector sprite-vector
                     :fps fps
                     :height frame-height
                     :width frame-width))))