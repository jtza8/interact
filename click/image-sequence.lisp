; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :click)

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
  (flet ((image-dimensions ()
           (list :width (il:image-width)
                 :height (il:image-height))))
    (loop with required-dimensions and images = '()
          for path in path-list
          for image = (il:gen-image)
          do (progn
               (assert (fad:file-exists-p path))
               (il:bind-image image)
               (il:load-image path)
               (il:check-error)
               (format-image)
               (if (null required-dimensions)
                   (setf required-dimensions (image-dimensions))
                   (unless (equal required-dimensions (image-dimensions))
                     (apply #'il:delete-images images)
                     (error 'image-dimensions-error
                            :expected-dimensions required-dimensions
                            :actual-dimensions (image-dimensions)
                            :addendum (format nil "file ~a" path))))
               (push image images))
          finally (return (reverse images)))))

(defmacro with-image-sequence ((variable sequence) &body body)
  `(let ((,variable (open-image-sequence ,sequence)))
     (unwind-protect (progn ,@body)
       (apply #'il:delete-images ,variable))))