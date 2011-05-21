; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :click)

(internal parse-colour-sprite)
(defun parse-colour-sprite (entry)
  (assert (and (listp entry)
               (eq (car entry) :colour)
               (>= (length entry) 3))
          () "invalid entry")
  (let ((path (cadr entry))
        (colour (caddr entry))
        (rest (subseq entry 3)))
    (print path)
    (print colour)
    (print rest)))

(internal parse-sprites)
(defun parse-sprites (sprites-conf-path)
  (unless (fad:file-exists-p sprites-conf-path)
    (return-from parse-sprites))
  (with-open-file (file sprites-conf-path)
    (loop for entry = (read file nil 'end-of-file)
       until (eq entry 'end-of-file)
       do (ecase (car entry)
            (:colour (parse-colour-sprite entry))))))

(defun load-sprite (file)
  (let ((file-name (string-downcase (file-namestring file))))
    (unless (find  (pathname-type file-name)
                  '("png" "tga" "tif" "tiff")
                  :test #'string=)
      (return-from load-sprite))
    (cond ((ppcre:scan "\\.ss\\.\\w+$" file-name)
           (load-sprite-sheet file))
          ((ppcre:scan "\\.fnt\\.\\w+$" file-name)
           (load-font-sheet file))
          (t (load-image-sprite file)))))

(internal *sprite-tree*)
(defparameter *sprite-tree*
  (make-instance 'rt:resource-tree 
                 :load-function #'load-sprite
                 :free-function #'free))

(defun sprite-node (&rest path)
  (apply #'rt:node *sprite-tree* path))

(defsetf sprite-node (&rest path) (value)
  `(setf (rt:node *sprite-tree* ,@path) ,value))

(defun load-sprite-path (path &key (recursive t) parent-node-path)
  (rt:load-path *sprite-tree* path 
                :recursive recursive
                :parent-node-path parent-node-path))
