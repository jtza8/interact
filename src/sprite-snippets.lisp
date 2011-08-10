; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :interact)

(defun make-sprite-snippets (sprite-names node)
  (make-array (length sprite-names)
              :element-type 'sprite
              :initial-contents
              (loop for name in sprite-names
                    for keyword = (intern (string-upcase name) "KEYWORD")
                    collect (node-of node keyword))))

(defmacro with-sprite-snippets ((snippets snippet-names &optional (prefix ""))
                                &body body)
  (flet ((add-prefix (symbol)
           (intern (format nil "~:@(~a~)~:@(~a~)" prefix symbol))))
    (let ((new-snippet-names (loop for snippet-name in
                                     (if (symbolp snippet-names)
                                         (eval snippet-names)
                                         snippet-names)
                                  collect (add-prefix snippet-name)))
          (snippets-var (gensym "SPRITE-SNIPPETS-"))
          (width (add-prefix 'width))
          (height (add-prefix 'height))
          (x (add-prefix 'x))
          (y (add-prefix 'y))
          (draw-snippet (add-prefix 'draw-snippet))
          (move-to (add-prefix 'move-to))
          (move-with (add-prefix 'move-with))
          (clear-width (add-prefix 'clear-width))
          (clear-height (add-prefix 'clear-height))
          (x-arg (gensym "X-ARG-"))
          (y-arg (gensym "Y-ARG-")))
     `(let ((,snippets-var ,snippets)
            (,width nil)
            (,height nil)
            (,x 0) (,y 0))
        (flet ((,draw-snippet (snippet &rest rest)
                 (apply #'draw-sprite snippet :x ,x :y ,y :width ,width
                        :height ,height rest))
               (,move-to (,x-arg ,y-arg)
                 (setf ,x ,x-arg ,y ,y-arg))
               (,move-with (,x-arg ,y-arg)
                 (incf ,x ,x-arg)
                 (incf ,y ,y-arg))
               (,clear-width ()
                 (setf ,width nil))
               (,clear-height ()
                 (setf ,height nil)))
          (declare (ignorable (function ,move-to) (function ,move-with)
                              (function ,clear-width) (function ,clear-height)))
          (let ,(loop for i upfrom 0
                   for snippet-name in new-snippet-names
                   collect `(,snippet-name (aref ,snippets-var ,i)))
            (declare (ignorable ,@new-snippet-names))
            ,@body))))))