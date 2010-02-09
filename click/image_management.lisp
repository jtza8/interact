; Copyright 2009 Jens Thiede. All rights reserved.
; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :click)

(define-condition invalid-image-node (error)
  ((invalid-node :initarg :invalid-node
                 :reader invalid-node))
  (:report (lambda (condition stream)
            (format stream "Invalid node: ~S" (invalid-node condition)))))

(defun file-to-texture (file)
  (let ((texture (car (gl:gen-textures 1))))
    (sdl:with-surface (surface (sdl-image:load-image file))
      (gl:bind-texture :texture-2d texture)
      (gl:tex-parameter :texture-2d :texture-min-filter :linear)
      (sdl-base::with-pixel (pix (sdl:fp surface))
        (let ((texture-format (ecase (sdl-base::pixel-bpp pix)
                                (3 :rgb)
                                (4 :rgba))))
          (assert (and (= (sdl-base::pixel-pitch pix)
                          (* (sdl:width surface) (sdl-base::pixel-bpp pix)))
                       (zerop (rem (sdl-base::pixel-pitch pix) 4))))
          (gl:tex-image-2d :texture-2d 0 :rgba
                           (sdl:width surface) (sdl:height surface)
                           0
                           texture-format
                           :unsigned-byte (sdl-base::pixel-data pix))))
      (make-instance 'image
                     :texture texture
                     :width (sdl:width surface)
                     :height (sdl:height surface)))))


(defun make-image-tree (base-dir)
  (flet ((make-keyword (string)
           (intern (nsubstitute #\- #\_ (string-upcase string))
                   "KEYWORD")))
    (loop
       for item in (cl-fad:list-directory base-dir)
       when (cl-fad:directory-exists-p item)
         collect (make-keyword (car (last (pathname-directory item))))
         and collect (make-image-tree item)
       when (and (cl-fad:file-exists-p item)
                 (let ((type (pathname-type item)))
                   (and type (string-equal (string-downcase type) "png"))))
         collect (make-keyword (pathname-name item))
         and collect (file-to-texture item))))

(defun fetch-image-node (&rest path)
  (let ((node (loop
                 with pointer = *theme-image-tree*
                 for keyword in path
                   do (setf pointer (getf pointer keyword))
                 finally (return pointer))))
      (assert (not (null node)) () 'invalid-image-node :invalid-node path)
    node))

(defun fetch-from-image-node (sub-tree node-tag)
  (let ((node (funcall #'getf sub-tree node-tag)))
    (assert (not (null node)) ()
            'invalid-image-node :invalid-node node-tag)
    node))

(defmacro with-node-images (path images &body body)
  (let ((image-node (gensym "IMAGE-NODE")))
    `(let* ((,image-node (fetch-image-node ,@path))
            ,@(loop for image in images collect
                   (list image
                         `(fetch-from-image-node ,image-node
                                                 ,(intern (symbol-name image)
                                                         "KEYWORD")))))
       ,@body)))