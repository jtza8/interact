; Copyright 2009 Jens Thiede. All rights reserved.
; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :click)

(define-condition invalid-sprite-node (error)
  ((invalid-node :initarg :invalid-node
                 :reader invalid-node))
  (:report (lambda (condition stream)
            (format stream "Invalid node: ~S" (invalid-node condition)))))

(defun load-image-sprite (file)
  (il:with-bound-image (il:gen-image)
    (il:load-image (namestring file))
    (il:convert-image :rgba :unsigned-byte)
    (il:check-error)
    (let ((img-mode (cffi:foreign-enum-keyword 
                     'il::data-format 
                     (il:get-integer :image-format)))
          (width (il:get-integer :image-width))
          (height (il:get-integer :image-height))
          (texture (car (gl:gen-textures 1))))
      (gl:bind-texture :texture-2d texture)
      (gl:tex-parameter :texture-2d :texture-min-filter :linear)
      (gl:tex-image-2d :texture-2d 0 :rgba width height 0 img-mode
                       :unsigned-byte (il:get-data))
      (il:check-error)
      (make-instance 'image-sprite :texture texture :width width
                     :height height))))

(defun make-sprite-tree (base-dir)
  (flet ((make-keyword (string)
           (intern (nsubstitute #\- #\_ (string-upcase string))
                   "KEYWORD")))
    (loop for item in (cl-fad:list-directory base-dir)
          when (cl-fad:directory-exists-p item)
            collect (make-keyword (car (last (pathname-directory item))))
            and collect (make-sprite-tree item)
          when (and (cl-fad:file-exists-p item)
                    (let ((type (pathname-type item)))
                      (and type (string-equal (string-downcase type) "png"))))
            collect (make-keyword (pathname-name item))
            and collect (load-image-sprite item))))

(defun fetch-sprite-node (path &optional (tree *sprite-tree*))
  (let ((node (loop with pointer = tree
                    for keyword in path
                      do (setf pointer (getf pointer keyword))
                    finally (return pointer))))
      (assert (not (null node)) () 'invalid-sprite-node :invalid-node path)
    node))

(defmacro with-node-sprites (path sprites &body body)
  (let ((sprite-node (gensym "SPRITE-NODE")))
    `(let* ((,sprite-node (fetch-sprite-node ',path))
            ,@(loop for sprite in sprites collect
                   (list sprite
                         `(fetch-sprite-node '(,(intern (symbol-name sprite)
                                                        "KEYWORD"))
                                             ,sprite-node))))
       ,@body)))

;; (il:init)
