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
  (write-pixel-header image 
                      `(2 ,frame-width) `(2 ,frame-height) 
                      `(2 ,frame-count) `(1 ,fps)
                      `(1 ,(if looping #x0001 #x0000))))

(internal read-sheet-header)
(defun read-sheet-header (&optional (image :current-image))
  (let ((values (read-pixel-header image 2 2 2 1 1)))
    (list* :frame-width (pop values)
         :frame-height (pop values)
         :frame-count (pop values)
         :fps (pop values)
         (let ((flags (pop values)))
           (list :looping (logbitp 0 flags))))))
           

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
        (il:clear-colour 0 0 0 0)
        (il:clear-image)
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
                         (setf (aref sprite-vector (- frame-count images-left))
                               (image-to-sprite))
                         (decf images-left)))))
       end-loops)
      (make-instance 'animation-sprite
                     :sprite-vector sprite-vector
                     :fps fps
                     :height frame-height
                     :width frame-width))))
