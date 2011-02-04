; Copyright 2011 Jens Thiede. All rights reserved.
; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :click)

(cffi:defcfun ("memcpy" memcpy) :pointer
  (dest :pointer) (src :pointer) (size :unsigned-int))

(cffi:defcfun ("memset" memset) :pointer
  (dest :pointer) (value :int) (size :unsigned-int))

(defun list-image-file-sequence (sequence-path)
  (let ((regex (let* ((file-name (file-namestring sequence-path))
                      (regex-str (ppcre:regex-replace "\\*" file-name "\\d+")))
                 (ppcre:parse-string regex-str))))
    (loop for path in (fad:list-directory (directory-namestring sequence-path))
          when (and (fad:file-exists-p path)
                    (ppcre:scan regex (file-namestring path)))
               collect path into image-file-list
          finally (return (sort image-file-list #'string< :key #'namestring)))))

(defun open-image-sequence (path-list)
  (loop for path in path-list
        for image = (il:gen-image)
        do (progn (il:bind-image image)
                  (il:load-image path)
                  (il:check-error))
        collect image into images
        collect (il:get-data) into data-pointers
        finally (return (values data-pointers images))))

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