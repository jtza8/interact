; Copyright 2009 Jens Thiede. All rights reserved.
; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :click)

(defun surface-to-texture (surface)
  (let ((texture (car (gl:gen-textures 1))))
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
    texture))

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
         and collect (sdl-image:load-image item))))

(defun fetch-image-node (&rest path)
  (let ((node (loop
                 with pointer = *theme-image-tree*
                 for keyword in path
                   do (setf pointer (getf pointer keyword))
                 finally (return pointer))))
    (when (null node)
      (error "Invalid image node."))
    node))

(defmacro with-node-images (images form &body body)
  (let ((image-node (gensym "IMAGE-NODE")))
    `(let* ((,image-node ,form)
             ,@(loop
                  for image in images collect
                    (list image
                          `(getf ,image-node
                                 (intern ,(symbol-name image) "KEYWORD")))))
       ,@body)))

(defmethod tile-for ((source sdl:surface) &key
                     (width (sdl:width source))
                     (height (sdl:height source)))
  (sdl:with-surface (dest (sdl:create-surface
                           width height
                           :pixel-alpha (sdl:pixel-alpha-enabled-p source))
                          nil)
    (sdl:enable-alpha (sdl:alpha-enabled-p source))
    (dotimes (column (ceiling (/ width (sdl:width source))) dest)
      (dotimes (row (ceiling (/ height (sdl:height source))))
        (setf (sdl:x source) (* column (sdl:width source))
              (sdl:y source) (* row (sdl:height source)))
        (sdl:blit-surface source dest)))))

(defmacro with-auto-free (free marker-name &body body)
  (let ((collection (gensym)))
    `(let ((,collection '()))
       (flet ((,marker-name (surface)
                (car (push surface ,collection))))
         (unwind-protect (progn ,@body)
           (dolist (item ,collection)
             (,free item)))))))