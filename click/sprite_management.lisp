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

(defun load-texture-sprite (file)
  (il:with-bound-image (il:gen-image)
    (il:load-image (namestring file))
    (il:check-error)
    (let ((img-mode (cffi:foreign-enum-keyword 
                     'il::data-format 
                     (il:get-integer :image-format)))
          (width (il:get-integer :image-width))
          (height (il:get-integer :image-height))
          (texture (car (gl:gen-textures 1))))
      (if (eq img-mode :rgba)
        (il:convert-image :rgba :unsigned-byte)
        (il:convert-image :rgb :unsigned-byte))
      (il:check-error)
      (gl:bind-texture :texture-2d texture)
      (gl:tex-parameter :texture-2d :texture-min-filter :linear)
      (gl:tex-image-2d :texture-2d 0 img-mode width height 0 img-mode
                       :unsigned-byte (il:get-data))
      (make-instance 'texture-sprite :texture texture :width width
                     :height height))))

(defun parse-color-sprite (entry)
  (assert (>= (length entry) 5) () "invalid entry")
  (let ((path (car entry))
        (color (subseq entry 1 5))
        (rest (subseq entry 5)))
    (print path)
    (print color)
    (print rest)))

(defun parse-sprites (base-dir)
  (let ((sprites-conf-path (merge-pathnames "sprites.conf" base-dir)))
    (unless (cl-fad:file-exists-p sprites-conf-path)
      (return-from parse-sprites))
    (with-open-file (file sprites-conf-path)
      (loop for line = (read file nil 'end-of-file)
            until (eq line 'end-of-file)
            do (parse-color-sprite line)))))
      

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
            and collect (load-texture-sprite item))))

(defun sprite-node (&rest path)
  (apply #'sprite-node-from *sprite-tree* path))

(defun sprite-node-from (tree &rest path)
  (let ((node (loop with pointer = tree
                    for keyword in path
                      do (setf pointer (getf pointer keyword))
                    finally (return pointer))))
      (assert (not (null node)) () 'invalid-sprite-node :invalid-node path)
    node))

(defmacro with-sprites (sprites sprite-node &body body)
  (let ((sprite-branch (gensym "SPRITE-NODE")))
    `(let ((,sprite-branch ,sprite-node))
       (let (,@(loop for sprite in sprites collect
                    (list sprite
                          `(sprite-node-from ,sprite-branch
                                             ,(intern (symbol-name sprite)
                                                      "KEYWORD")))))
         ,@body))))

(defun translate (x y)
  (gl:matrix-mode :modelview)
  (gl:push-matrix)
  (gl:translate x y 0))

(defun undo-translate ()
  (gl:matrix-mode :modelview)
  (gl:pop-matrix))

(defmacro with-translate ((x y) &body body)
  `(progn
     (translate ,x ,y)
     ,@body
     (undo-translate)))
