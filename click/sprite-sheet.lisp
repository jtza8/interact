; Copyright 2011 Jens Thiede. All rights reserved.
; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :click)

(cffi:defcfun ("memset" memset) :pointer
  (dest :pointer) (value :int) (size :unsigned-int))

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
               (push image images)
               (if (null required-info)
                   (setf required-info (image-info))
                   (unless (equal required-info (image-info))
                     (apply #'il:delete-images images)
                     (error 'image-sequence-error
                            :path path
                            :properties (image-info)
                            :requirements required-info))))
          finally (return images))))

(defmacro with-image-sequence ((variable sequence) &body body)
  `(let ((,variable (open-image-sequence ,sequence)))
     (unwind-protect (progn ,@body)
       (apply #'il:delete-images ,variable))))

(defun build-sprite-sheet (sequence-path fps &key (max-columns 10)
                           sheet-file-name)
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
          (setf (cffi:mem-aref row :uint8) fps)
          (il:with-images (sprite-sheet)
            (il:bind-image sprite-sheet)
            (il:tex-image width height 1 bytes-per-pixel pixel-format
                          data-type data)
            (let ((sequence-ref sequence))
              (dotimes (sequence-x (min max-columns (length sequence)))
                (dotimes (sequence-y (ceiling (/ frame-count max-columns)))
                  (il:blit (pop sequence-ref) (* frame-width sequence-x)
                           (- (1- height) (* sequence-y frame-height)) 0 
                           0 0 0 frame-width frame-height 1))))
            (when (null sheet-file-name)
              (setf sheet-file-name 
                    (ppcre:regex-replace "[-_]?\\*"
                                         (namestring sequence-path) ".ss")))
            (il:save-image sheet-file-name)))))))