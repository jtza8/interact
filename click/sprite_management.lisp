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
      (make-instance 'image-sprite
                     :texture texture
                     :width (sdl:width surface)
                     :height (sdl:height surface)))))


(defun make-sprite-tree (base-dir)
  (flet ((make-keyword (string)
           (intern (nsubstitute #\- #\_ (string-upcase string))
                   "KEYWORD")))
    (loop
       for item in (cl-fad:list-directory base-dir)
       when (cl-fad:directory-exists-p item)
         collect (make-keyword (car (last (pathname-directory item))))
         and collect (make-sprite-tree item)
       when (and (cl-fad:file-exists-p item)
                 (let ((type (pathname-type item)))
                   (and type (string-equal (string-downcase type) "png"))))
         collect (make-keyword (pathname-name item))
         and collect (file-to-texture item))))

(defun fetch-sprite-node (&rest path)
  (let ((node (loop
                 with pointer = *sprite-tree*
                 for keyword in path
                   do (setf pointer (getf pointer keyword))
                 finally (return pointer))))
      (assert (not (null node)) () 'invalid-sprite-node :invalid-node path)
    node))

(defmacro with-node-sprites (path sprites &body body)
  (let ((sprite-node (gensym "SPRITE-NODE")))
    `(flet ((fetch-from-node (sub-tree node-tag)
              (let ((node (getf sub-tree node-tag)))
                (assert (not (null node)) ()
                        'invalid-sprite-node :invalid-node node-tag)
                node)))
       (let* ((,sprite-node (fetch-sprite-node ,@path))
              ,@(loop for sprite in sprites collect
                     (list sprite
                           `(fetch-from-node ,sprite-node
                                             ,(intern (symbol-name sprite)
                                                      "KEYWORD")))))
       ,@body))))