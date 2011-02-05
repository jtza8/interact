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
               (format stream "image ~a properties: ~s required properties: ~s"
                       path properties requirements)))))

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
                            :requirements required-info)))))))

;(defun build-sprite-sheet (sequence-path fps &key (columns 10) sheet-file-name)
;  (il:with-bound-image (il:gen-image)
;    ; data-size = (* width (1+ height))
;    (cffi:with-foreign-object (data :uint8 data-size)
;      (let ((i (- data-size width)))
;        (setf (cffi:mem-ref data :uint16 i) frame-width
;              (cffi:mem-ref data :uint16 (incf i 16)) frame-height
;              (cffi:mem-ref data :uint8 (incf i 16)) frame-count
;              (cffi:mem-ref data :uint8 (incf i 8)) fps)
;        (mem-set (cffi-sys:inc-pointer data ()