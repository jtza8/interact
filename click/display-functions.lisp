; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :click)

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

(defparameter *clipping-stack* '())

(defun overlay-rectangles (&optional a b)
  (when (or (null a) (null b))
    (return-from overlay-rectangles))
  (destructuring-bind (ax ay aw ah) a
    (destructuring-bind (bx by bw bh) b
      (let* ((x (max ax bx))
             (y (max ay by))
             (w (max (- (min (+ ax aw) (+ bx bw)) x) 0))
             (h (max (- (min (+ ay ah) (+ by bh)) y) 0)))
        (list x y w h)))))

(defun clip-display (x y width height)
  (when (= 0 (length *clipping-stack*))
    (gl:enable :scissor-test))
  (push (list x y width height) *clipping-stack*)
  (apply #'gl:scissor (reduce #'overlay-rectangles *clipping-stack*
                              :from-end t)))

(defun undo-clipping ()
  (when (= 1 (length *clipping-stack*))
    (gl:disable :scissor-test))
  (pop *clipping-stack*))

(defmacro with-clipping ((x y width height) &body body)
  (let ((make-clip (gensym)))
    `(let ((,make-clip (and (>= ,height 0) (>= ,width 0))))
       (when ,make-clip (clip-display ,x ,y ,width ,height))
       (unwind-protect (progn ,@body)
         (when ,make-clip (undo-clipping))))))
