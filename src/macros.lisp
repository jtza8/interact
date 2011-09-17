; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :interact)

(internal with-try-again-restart)
(defmacro with-try-again-restart ((&optional
                                   (description "Re-evaluate relevant code."))
                                  &body body)
  (let ((again (gensym "TAG-")))
    `(tagbody
      ,again
        (restart-case (progn ,@body)
          (try-again () :report ,description (go ,again))))))

(defmacro make-widget (widget-s-exp)
  (labels ((parse-s-exp (s-exp)
             (if (keywordp (car s-exp))
                 `(list ,(car s-exp) ,(parse-s-exp (cadr s-exp)))
                 (let ((widget-symbol (intern (symbol-name (car s-exp)))))
                   `(make-instance ',widget-symbol
                      ,@(loop for (key value) on (cdr s-exp) by #'cddr
                              collect key
                              if (eq key :widgets)
                              collect `(list ,@(mapcar #'parse-s-exp value))
                              else collect value))))))
    (parse-s-exp widget-s-exp)))

(defmacro make-container ((container-type &rest init-args) &body s-exps)
  `(make-instance ',container-type
     :widgets (list ,@(loop for s-exp in s-exps
                            collect `(make-widget ,s-exp)))
     ,@init-args))

(defmacro make-root-widgets (&body s-exps)
  (let ((widget-var (gensym "WIDGET-")))
    `(let (,widget-var)
       ,@(loop for s-exp in s-exps
               collect `(setf ,widget-var (make-widget ,s-exp))
               collect `(if (listp ,widget-var)
                            (add-to-root (cadr ,widget-var)
                                         (car ,widget-var))
                            (add-to-root ,widget-var))))))
