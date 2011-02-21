; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :click)

(cffi:defcfun ("memcpy" memcpy) :pointer
  (dest :pointer) (src :pointer) (size :unsigned-int))

(define-condition pixel-index-error (error)
  ((message :initarg :message
            :initform (error "must specify message")))
  (:report (lambda (condition stream)
             (with-slots (message) condition
               (princ message stream)))))

(defmacro assert-pixel-index-cond (condition message &optional (places '()))
  `(assert ,condition ,places 'pixel-index-error 
           :message ,message))

(define-condition image-sequence-error (error)
  ((path :initarg :path
         :initform (error "must specify path")
         :reader path)
   (properties :initarg :properties
               :initform (error "must specify properties")
               :reader properties)
   (requirements :initarg :requirements
                 :initform (error "must specify requirements")
                 :reader requirements))
  (:report (lambda (condition stream)
             (with-slots (path properties requirements) condition
               (format stream 
                       "the image ~a must have the properties: ~
                       (~{~s~#[~:; ~]~}) but has the properties: ~
                       (~{~s~#[~:; ~]~})"
                       path requirements properties)))))

(defun format-image ()
  (case (il:image-format)
    ((:rgb :bgr) (il:convert-image :rgb :unsigned-byte))
    ((:rgba :bgra) (il:convert-image :rgba :unsigned-byte))
    (otherwise (error "image format ~s not supported" (il:image-format))))
  (il:check-error))

(defun image-to-sprite (&optional (image :current-image))
  (il:with-bound-image image
    (let ((texture (car (gl:gen-textures 1)))
          (format (il:image-format))
          (width (il:image-width))
          (height (il:image-height)))
      (assert (eq (il:image-type) :unsigned-byte) ()
              "currently, only :unsigned-byte image types are supported")
      (gl:bind-texture :texture-2d texture)
      (gl:tex-parameter :texture-2d :texture-min-filter :linear)
      (gl:tex-image-2d :texture-2d 0 format width height 0
                       format :unsigned-byte (il:get-data))
      (make-instance 'texture-sprite :texture texture :width width
                     :height height))))
  

(defun load-texture-sprite (file)
  (il:with-images (image)
    (il:with-bound-image (setf image (il:gen-image))
      (il:load-image (namestring file))
      (il:check-error)
      (format-image)
      (image-to-sprite))))

(defun list-image-file-sequence (sequence-path)
  (let ((regex (let* ((file-name (file-namestring sequence-path)) regex-str)
                 (setf regex-str (ppcre:regex-replace "\." file-name "\\.")
                       regex-str (ppcre:regex-replace "\\*" file-name "\\d+"))
                 (ppcre:parse-string regex-str))))
    (sort (loop for path in (fad:list-directory
                             (directory-namestring sequence-path))
                when (and (fad:file-exists-p path)
                          (ppcre:scan regex (file-namestring path)))
                  collect path)
           #'string< :key #'namestring)))

(defun open-image-sequence (path-list)
  (flet ((image-info ()
           (list :width (il:image-width)
                 :height (il:image-height)
                 :data-format (il:image-format))))
    (loop with required-info and images = '()
          for path in path-list
          for image = (il:gen-image)
          do (progn
               (assert (fad:file-exists-p path))
               (il:bind-image image)
               (il:load-image path)
               (il:check-error)
               (if (null required-info)
                   (setf required-info (image-info))
                   (unless (equal required-info (image-info))
                     (apply #'il:delete-images images)
                     (error 'image-sequence-error
                            :path path
                            :properties (image-info)
                            :requirements required-info)))
               (push image images))
          finally (return (reverse images)))))

(defun image-data-pos (x y &optional (image :current-image))
  (il:with-bound-image image
    (let ((width (il:image-width))
          (height (il:image-height)))
      (assert-pixel-index-cond (and (<= 0 x width) (<= 0 y height))
                               (format nil "invalid index: (~d, ~d) ~
                                            image width: ~d, ~
                                            image height: ~d"
                                       x y width height)
                               (x y))
      (cffi:inc-pointer (il:get-data) (* (+ x (* y width))
                                         (il:image-bytes-per-pixel))))))

(defun blit (source dest-x dest-y dest-z src-x src-y src-z width height depth
             &key (allow-clipping t))
  (declare (ignore dest-z src-z depth))
  (let* ((dest-width (il:image-width))
         (dest-height (il:image-height))
         (data-format (il:image-format))
         (data-type (il:image-type))
         (bytes-per-pixel (il:image-bytes-per-pixel))
         (src-width (il:image-width source))
         (src-height (il:image-height source))
         (src-row-size (* (min width (- dest-width dest-x))
                          bytes-per-pixel)))
    (assert-pixel-index-cond (>= src-width width) 
                             "width argument too large")
    (assert-pixel-index-cond (>= src-height height) 
                             "height argument too large")
    (unless allow-clipping
      (assert-pixel-index-cond (>= (- dest-width dest-x) width)
                               "clipping not alowed, x axis is clipped")
      (assert-pixel-index-cond (>= (- dest-height dest-y) height)
                               "clipping not alowed, y axis is clipped"))
    (il:with-images (source-clone)
      (let ((image (if (and (eq (il:image-type source) data-type)
                            (eq (il:image-format source) data-format))
                       source
                       (il:with-bound-image source-clone
                         (il:copy-image source)
                         (il:convert-image data-format data-type)
                         (il:check-error)
                         source-clone))))
        (loop for y from 0 upto (1- (min (- dest-height dest-y) height))
           do (memcpy (image-data-pos dest-x (+ y dest-y))
                      (image-data-pos src-x (+ (- height 1 y) src-y) image)
                      src-row-size))))))

(defun overlay-image (source x y z &key (allow-clipping t))
  (blit source x y z 0 0 0 (il:image-width source) (il:image-height source) 1
        :allow-clipping allow-clipping))

(defmacro with-image-sequence ((variable sequence) &body body)
  `(let ((,variable (open-image-sequence ,sequence)))
     (unwind-protect (progn ,@body)
       (apply #'il:delete-images ,variable))))

(defun build-sprite-sheet (sequence-path fps &key (max-columns 5)
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
        (case pixel-format
          (:rgb (il:clear-image 0 0 0))
          (:rgba (il:clear-image 0 0 0 0)))
        (let ((row (image-data-pos 0 (1- height))))
          (setf (cffi:mem-aref row :uint8) frame-width)
          (cffi:incf-pointer row 1)
          (setf (cffi:mem-aref row :uint16) frame-height)
          (cffi:incf-pointer row 2)
          (setf (cffi:mem-aref row :uint16) frame-count)
          (cffi:incf-pointer row 2)
          (setf (cffi:mem-aref row :uint8) fps))
        (let ((sequence-pointer sequence))
          (loop for sequence-y from (1- (ceiling (/ frame-count max-columns)))
                    downto 0
                do (dotimes (sequence-x (min max-columns frame-count))
                     (overlay-image (pop sequence-pointer)
                                    (* sequence-x frame-width)
                                    (* sequence-y frame-height)
                                    0 :allow-clipping nil))))
        (when (null sheet-file-name)
          (setf sheet-file-name 
                (ppcre:regex-replace "[-_ ]?\\*"
                                     (namestring sequence-path) ".ss")))
        (if file-overwrite
            (il:enable :file-overwrite)
            (il:disable :file-overwrite))
        (il:save-image sheet-file-name)))))