; Copyright 2009 Jens Thiede. All rights reserved.
; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :click)

(defun load-image (file)
  (let ((texture (car (gl:gen-textures 1)))
        (image (sdl-image:load-image file)))
    (gl:bind-texture :texture-2d texture)
    (gl:tex-parameter :texture-2d :texture-min-filter :linear)
    (sdl-base::with-pixel (pix (sdl:fp image))
      (let ((texture-format (ecase (sdl-base::pixel-bpp pix)
                              (3 :rgb)
                              (4 :rgba))))
        (assert (and (= (sdl-base::pixel-pitch pix)
                        (* (sdl:width image) (sdl-base::pixel-bpp pix)))
                     (zerop (rem (sdl-base::pixel-pitch pix) 4))))
        (gl:tex-image-2d :texture-2d 0 :rgba
                         (sdl:width image) (sdl:height image)
                         0
                         texture-format
                         :unsigned-byte (sdl-base::pixel-data pix))))
    (make-instance 'image
                   :texture texture
                   :width (sdl:width image)
                   :height (sdl:height image))))

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
         and collect (load-image item))))

(defun free-image-tree (&optional (tree *theme-image-tree*))
  (loop
     for (nil item) on tree by #'cddr
     if (consp item)
       do (free-image-tree item)
     else
       do (gl:delete-textures (list (texture item)))))

(defun fetch-image-node (&rest path)
  (loop
     with pointer = *theme-image-tree*
     for keyword in path
       do (setf pointer (getf pointer keyword))
     finally (return pointer)))

(defmacro with-node-images (images form &body body)
  (let ((image-node (gensym "IMAGE-NODE")))
    `(let* ((,image-node ,form)
             ,@(loop
                  for image in images collect
                    (list image
                          `(getf ,image-node
                                 (intern ,(symbol-name image) "KEYWORD")))))
       ,@body)))