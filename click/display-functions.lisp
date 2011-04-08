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
  (gl:translate x y 0))

(defun undo-translate ()
  (gl:matrix-mode :modelview)
  (gl:pop-matrix))

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

(declaim (inline viewport))
(defun viewport ()
  (gl:get-integer :viewport))

(defun clip-display (x y width height)
  (with-accessors ((clipping-depth clipping-depth)) *df-state*
    (let ((clipping
           (simple-vector-bind (dx dy dw dh) (viewport)
             (declare (ignore dw))
             (vector x (+ (- dh y height) dy) (+ width dx) (+ height dy)))))
      (simple-vector-bind (x y width height)
          (if (= clipping-depth 0)
              (progn (gl:enable :scissor-test)
                     clipping)
              (overlay-rectangles (gl:get-integer :scissor-box)
                                  clipping))
        (gl:push-attrib :scissor)
        (gl:scissor x y width height)
        (incf clipping-depth)))))

(defun undo-clipping ()
  (with-accessors ((clipping-depth clipping-depth)) *df-state*
    (gl:pop-attrib)
    (decf clipping-depth)
    (when (= clipping-depth 0)
      (gl:disable :scissor-test))))

(defmacro with-clipping ((x y width height) &body body)
  (let ((make-clip (gensym)))
    `(let ((,make-clip (and (>= ,height 0) (>= ,width 0))))
       (when ,make-clip (clip-display ,x ,y ,width ,height))
       (unwind-protect (progn ,@body)
         (when ,make-clip (undo-clipping))))))
