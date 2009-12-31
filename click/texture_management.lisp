; Copyright 2009 Jens Thiede. All rights reserved.
; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :click)

(defparameter *theme-texture-tree* nil)

(defun load-texture (file)
  file)

(defun make-texture-tree (base-dir)
  (flet ((make-keyword (string)
           (intern (nsubstitute #\- #\_ (string-upcase string))
                   "KEYWORD")))
    (loop
       for item in (cl-fad:list-directory base-dir)
       when (cl-fad:directory-exists-p item)
         collect (make-keyword (car (last (pathname-directory item))))
         and collect (make-texture-tree item)
       when (and (cl-fad:file-exists-p item)
                 (let ((type (pathname-type item)))
                   (and type (string-equal (string-downcase type) "png"))))
         collect (make-keyword (pathname-name item))
         and collect (load-texture item))))

(defun fetch-texture (&rest texture-path)
  (loop
     with pointer = *theme-texture-tree*
     for keyword in texture-path
       do (setf pointer (getf pointer keyword))
     finally (return pointer)))

;(defun bind-texture (&rest texture-path)
;  ())