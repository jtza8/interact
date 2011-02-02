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

(defun sprite-node-from (tree &rest path)
  (loop with pointer = tree
     for keyword in path
     do (setf pointer (getf pointer keyword))
     finally (progn (assert (not (null pointer)) () 'invalid-sprite-node
                            :invalid-node path)
                    (return pointer))))

(defun sprite-node (&rest path)
  (apply #'sprite-node-from *sprite-tree* path))

(defun setf-sprite-node-from (tree &rest rest)
  (let ((new-value (car (last rest)))
        (path (butlast rest)))
    (loop with current = tree
          for keyword in path
          for remaining from (1- (length path)) downto 0
          for next = (getf current keyword)
          if (null next)
            do (if (= remaining 0) 
                   (setf (getf current keyword) new-value)
                   (setf (getf current keyword) next
                         current next))
          else
            do (if (= remaining 0)
                   (setf (getf current keyword) new-value)
                   (setf current next))
          end
          finally (return tree))))

(defun setf-sprite-node (&rest rest)
  (apply #'setf-sprite-node-from *sprite-tree* rest))

(defsetf sprite-node-from setf-sprite-node-from)
(defsetf sprite-node setf-sprite-node)

(defun parse-color-sprite (entry)
  (assert (and (listp entry)
               (eq (car entry) :color)
               (>= (length entry) 3))
          () "invalid entry")
  (let ((path (cadr entry))
        (color (caddr entry))
        (rest (subseq entry 3)))
    (print path)
    (print color)
    (print rest)))

(defun parse-sprites (sprites-conf-path)
  (assert (cl-fad:file-exists-p sprites-conf-path) () 
          "~a doesn't exist or isn't a file." sprites-conf-path)
  (with-open-file (file sprites-conf-path)
    (loop for entry = (read file nil 'end-of-file)
       until (eq entry 'end-of-file)
       do (ecase (car entry)
            (:color (parse-color-sprite entry))))))

(defun make-sprite-tree (base-dir)
  (flet ((make-keyword (string)
           (intern (nsubstitute #\- #\_ (string-upcase string))
                   "KEYWORD")))
    (setf *sprite-tree*
          (loop for item in (cl-fad:list-directory base-dir)
             when (cl-fad:directory-exists-p item)
             collect (make-keyword (car (last (pathname-directory item))))
             and collect (make-sprite-tree item)
             when (and (cl-fad:file-exists-p item)
                       (let ((type (pathname-type item)))
                         (and type (string-equal (string-downcase type)
                                                 "png"))))
             collect (make-keyword (pathname-name item))
             and collect (load-texture-sprite item)))))
  
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
