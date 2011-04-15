; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :click)

(defparameter *clipping-depth* 0)
(defparameter *translations* '())
(defparameter *rotations* '())

(defun translate (x y)
  (gl:matrix-mode :modelview)
  (gl:translate x y 0)
  (push (list x y 0) *translations*))

(defun undo-translate ()
  (gl:matrix-mode :modelview)
  (let ((translation (pop *translations*)))
    (when (null translation) (return-from undo-translate))
    (apply #'gl:translate (mapcar #'- translation))))

(defmacro with-translate ((x y) &body body)
  `(progn
     (translate ,x ,y)
     (unwind-protect (progn ,@body)
       (undo-translate))))

(defun rotate (radians)
  (let ((degrees (* (/ radians (* 2 PI)) 360.0)))
    (gl:matrix-mode :modelview)
    (gl:rotate degrees 0 0 1)
    (push degrees *rotations*)))

(defun undo-rotate ()
  (gl:matrix-mode :modelview)
  (let ((rotation (pop *rotations*)))
    (when (null rotation) (return-from undo-rotate))
    (gl:rotate rotation 0 0 -1)))

(defmacro with-rotate (radians &body body)
  `(progn
     (rotate ,radians)
     (unwind-protect (progn ,@body)
       (undo-rotate))))

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
  (let ((clipping
         (simple-vector-bind (dx dy dw dh) (viewport)
           (declare (ignore dw))
           (vector x (+ (- dh y height) dy) (+ width dx) (+ height dy)))))
    (simple-vector-bind (x y width height)
        (if (= *clipping-depth* 0)
            (progn (gl:enable :scissor-test)
                   clipping)
            (overlay-rectangles (gl:get-integer :scissor-box)
                                clipping))
      (gl:push-attrib :scissor)
      (gl:scissor x y width height)
      (incf *clipping-depth*))))

(defun undo-clipping ()
  (gl:pop-attrib)
  (decf *clipping-depth*)
  (when (= *clipping-depth* 0)
    (gl:disable :scissor-test)))

(defmacro with-clipping ((x y width height) &body body)
  `(progn
     (clip-display ,x ,y ,width ,height)
     (unwind-protect (progn ,@body)
       (undo-clipping))))
