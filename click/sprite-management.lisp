; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :click)

(define-condition invalid-sprite-node (error)
  ((invalid-node :initarg :invalid-node
                 :reader invalid-node))
  (:report (lambda (condition stream)
            (format stream "Invalid node: ~S" (invalid-node condition)))))

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
  (unless (fad:file-exists-p sprites-conf-path)
    (return-from parse-sprites))
  (with-open-file (file sprites-conf-path)
    (loop for entry = (read file nil 'end-of-file)
       until (eq entry 'end-of-file)
       do (ecase (car entry)
            (:color (parse-color-sprite entry))))))

(defun make-node-keyword (path)
  (let* ((file-name (file-namestring path))
         (symbol-name (if (string= file-name "")
                          (ppcre:scan-to-strings "(?!/)[^/]*(?=/$)" 
                                                 (namestring path))
                          (ppcre:scan-to-strings "^.*?(?=\\.)|^[^\\.]*$"
                                                 file-name))))
    (intern (nsubstitute #\- #\_ (string-upcase symbol-name))
            "KEYWORD")))

(defun load-sprite (file)
  (let ((file-name (string-downcase (file-namestring file))))
    (assert (ppcre:scan "\\.(?:png|tga|tif|tiff)$" file-name) (file)
            'file-format-error
            :pattern (ppcre:scan-to-strings "\\.[^\\.]+$|^[^\\.]*$" file-name))
    (cond ((ppcre:scan "\\.ss\\.\\w+$" file-name) (load-sprite-sheet file))
          (t (load-image-sprite file)))))

(defun make-sprite-tree (base-dir)
   (setf *sprite-tree*
         (loop with sprite
               for item in (fad:list-directory base-dir)
               if (fad:directory-exists-p item)
                 collect (make-node-keyword item)
                 and collect (make-sprite-tree item)
               else if (setf sprite (restart-case (load-sprite item)
                                      (ignore-file () nil)))
                 collect (make-node-keyword item)
                 and collect sprite)))
 
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
