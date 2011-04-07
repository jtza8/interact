; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :click)

(defclass display-functions-state ()
  ((clipping-depth :initarg :clipping-depth
                   :initform 0
                   :accessor clipping-depth)
   (clipping-rect :initarg :clipping-rect
                  :initform #(0 0 0 0)
                  :accessor clipping-rect)
   (translate-stack :initarg :translate-stack
                    :initform '()
                    :accessor translate-stack)))

(defparameter *df-state*
  (make-instance 'display-functions-state))

(defun translate (x y)
  (gl:matrix-mode :modelview)
  (gl:push-matrix)
  (gl:translate x y 0)
  (push (list x y) (translate-stack *df-state*))
  (let ((clipping-rect (clipping-rect *df-state*)))
    (decf (aref clipping-rect 0) x)
    (decf (aref clipping-rect 1) y)
    (print clipping-rect)))

(defun undo-translate ()
  (with-accessors ((translate-stack translate-stack)
                   (clipping-rect clipping-rect))
      *df-state*
    (destructuring-bind (x y) (pop translate-stack)
      (gl:matrix-mode :modelview)
      (gl:pop-matrix)
      (incf (aref clipping-rect 0) x)
      (incf (aref clipping-rect 1) y))))

(defmacro with-translate ((x y) &body body)
  `(progn
     (translate ,x ,y)
     (unwind-protect (progn ,@body)
       (undo-translate))))

(defmacro simple-vector-bind ((&rest variables) src-vector &body body)
  `(let ,(loop for variable in variables
               for i upfrom 0
               collect `(,variable (aref ,src-vector ,i)))
     ,@body))

(defun overlay-rectangles (a b)
  (simple-vector-bind (ax ay aw ah) a
    (simple-vector-bind (bx by bw bh) b
      (let* ((x (max ax bx))
             (y (max ay by))
             (w (max (- (min (+ ax aw) (+ bx bw)) x) 0))
             (h (max (- (min (+ ay ah) (+ by bh)) y) 0)))
        (vector x y w h)))))

(defun clip-display (x y width height)
  (when (= (clipping-depth *df-state*) 0)
    (gl:enable :scissor-test))
  (incf y height)
  (with-accessors ((clipping-depth clipping-depth)
                   (clipping-rect clipping-rect))
      *df-state*
    (simple-vector-bind (x y width height)
        (setf clipping-rect
              (if (> clipping-depth 0)
                  (overlay-rectangles clipping-rect (vector x y width height))
                  (vector x y width height)))
      (gl:push-attrib :scissor-bit)
      (gl:scissor x y width height)
      (incf clipping-depth))))

(defun undo-clipping ()
  (gl:pop-attrib)
  (with-accessors ((clipping-depth clipping-depth)) *df-state*
    (decf clipping-depth)
    (when (= 0 clipping-depth)
      (gl:disable :scissor-test))
    clipping-depth))

(defmacro with-clipping ((x y width height) &body body)
  (let ((make-clip (gensym)))
    `(let ((,make-clip (and (>= ,height 0) (>= ,width 0))))
       (when ,make-clip (clip-display ,x ,y ,width ,height))
       (unwind-protect (progn ,@body)
         (when ,make-clip (undo-clipping))))))
