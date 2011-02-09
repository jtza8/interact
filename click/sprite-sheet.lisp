; Copyright 2011 Jens Thiede. All rights reserved.
; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :click)

(cffi:defcfun ("memset" memset) :pointer
  (dest :pointer) (value :int) (size :unsigned-int))

(cffi:defcfun ("memcpy" memcpy) :pointer
  (dest :pointer) (src :pointer) (size :unsigned-int))

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

; Now at an acceptable speed:
(defun clear-image (&rest colour)
  (let* ((pointer (il:get-data))
         (data pointer)
         (bytes-per-pixel (il:get-integer :image-bytes-per-pixel))
         (width (il:get-integer :image-width))
         (height (il:get-integer :image-height))
         (data-type (ecase (cffi:foreign-enum-keyword 
                            'il::data-type (il:get-integer :image-type))
                      (:byte :int8)
                      (:double :double)
                      (:float :float)
                      (:int :int)
                      (:short :short)
                      (:unsigned-int :unsigned-int)
                      (:unsigned-byte :uint8)
                      (:unsigned-short :unsigned-short)))
         (data-type-size (cffi:foreign-type-size data-type))
         (row-size (* width bytes-per-pixel)))
    (assert (= (* data-type-size (length colour)) bytes-per-pixel) ()
            "incorrect channel count")
    (if (apply #'= (cons 0 colour))
        (memset data 0 (* width height bytes-per-pixel))
        (cffi:with-foreign-object (colour-pixel :uint8 bytes-per-pixel)
          (let ((pointer colour-pixel))
            (dolist (channel colour)
              (setf (cffi:mem-aref pointer data-type)
                    channel)
              (cffi:incf-pointer pointer data-type-size)))
          (dotimes (i width)
            (memcpy pointer colour-pixel bytes-per-pixel)
            (cffi:incf-pointer pointer bytes-per-pixel))
          ; ~83x faster than pixel per pixel with 500x500 RGBA.
          (dotimes (i (1- height)) 
            (memcpy pointer data row-size)
            (cffi:incf-pointer pointer row-size))))))

(defun open-image-sequence (path-list)
  (flet ((image-info ()
           (list :width (il:get-integer :image-width)
                 :height (il:get-integer :image-height)
                 :data-format (cffi:foreign-enum-keyword 
                               'il::data-format 
                               (il:get-integer :image-format)))))
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
          finally (return images))))

(defun overlay-image (source x y)
  (let ((dest-width (il:get-integer :image-width))
        (dest-height (il:get-integer :image-height))
        (dest-data (il:get-data))
        (data-format (il:get-integer :image-format))
        (data-type (il:get-integer :image-type))
        (bytes-per-pixel (il:get-integer :image-bytes-per-pixel))
        src-height src-width src-data src-row-size)
    (il:with-bound-image source
      (il:convert-image data-format data-type)
      (il:check-error)
      (setf src-width (il:get-integer :image-width)
            src-height (il:get-integer :image-height)
            src-data (il:get-data)
            src-row-size (* src-width bytes-per-pixel)))
    (flet ((get-src-pos (x y)
             (cffi:inc-pointer src-data (* (+ x (* y src-width))
                                           bytes-per-pixel)))
           (get-dest-pos (x y)
             (cffi:inc-pointer dest-data (* (+ x (* y dest-width))
                                            bytes-per-pixel))))
      (loop for src-y below src-height
            for dest-y upfrom y
            do (memcpy (get-dest-pos x dest-y)
                       (get-src-pos 0 src-y)
                       src-row-size)))))

(defmacro with-image-sequence ((variable sequence) &body body)
  `(let ((,variable (open-image-sequence ,sequence)))
     (unwind-protect (progn ,@body)
       (apply #'il:delete-images ,variable))))

(defun build-sprite-sheet (sequence-path fps &key (max-columns 10)
                           sheet-file-name (file-overwrite nil))
  (with-image-sequence (sequence (list-image-file-sequence sequence-path))
    (let* ((height (1+ (* (il:height-of (car sequence))
                          (ceiling (/ (length sequence) max-columns)))))
           (width (* max-columns (il:width-of (car sequence))))
           (bytes-per-pixel (il:bytes-per-pixel-of (car sequence)))
           (data-size (* height width bytes-per-pixel))
           (data-type (cffi:foreign-enum-keyword
                       'il::data-type (il:get-integer :image-type)))
           (frame-width (il:width-of (car sequence)))
           (frame-height (il:height-of (car sequence)))
           (frame-count (length sequence))
           (pixel-format (il:pixel-format-of (car sequence))))
      (cffi:with-foreign-object (data :uint8 data-size)
        (memset data 0 data-size)
        (let ((row (cffi:inc-pointer data (* (1- height) width
                                             bytes-per-pixel))))
          (setf (cffi:mem-aref row :uint8) frame-width)
          (cffi:incf-pointer row 1)
          (setf (cffi:mem-aref row :uint16) frame-height)
          (cffi:incf-pointer row 2)
          (setf (cffi:mem-aref row :uint16) frame-count)
          (cffi:incf-pointer row 2)
          (setf (cffi:mem-aref row :uint8) fps))
        (il:with-images (sprite-sheet)
          (il:bind-image sprite-sheet)
          (il:tex-image width height 1 bytes-per-pixel pixel-format
                        data-type data)
          (let ((sequence-ref sequence))
            (dotimes (sequence-x (min max-columns (length sequence)))
              (dotimes (sequence-y (ceiling (/ frame-count max-columns)))
                (overlay-image (pop sequence-ref)
                               (* frame-width sequence-x)
                               (- (1- height) (* sequence-y frame-height))))))
          (when (null sheet-file-name)
            (setf sheet-file-name 
                  (ppcre:regex-replace "[-_]?\\*"
                                       (namestring sequence-path) ".ss")))
          (if file-overwrite
            (il:enable :file-overwrite)
            (il:disable :file-overwrite))
          (il:save-image sheet-file-name))))))