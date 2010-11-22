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
  (ilut:renderer :opengl)
  (ilut:enable :opengl-conv)
  (let ((texture (ilut:gl-load-image (namestring file))) sprite)
    (il:check-error)
    (setf sprite (make-instance 'image-sprite
                                :texture texture
                                :width (il:get-integer :image-width)
                                :height (il:get-integer :image-height)))
    (gl:bind-texture :texture-2d 0)
    (il:bind-image 0)
    (il:check-error)
    sprite))


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
;; Just for the moment, will move to click:init-click.
(il:init)
(ilut:init)
