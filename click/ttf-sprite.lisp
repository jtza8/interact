; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :click)

(defclass ttf-sprite (vector-sprite)
  ((font-object :initform (error "must specify font-object")
                :initarg :font-object)
   (colour :initform '(0.0 0.0 0.0 1.0)
           :initarg :colour
           :reader colour)
   (text :initform ""
         :initarg :text
         :reader text)
   (size :initform 10
         :initarg :size
         :reader size)
   (width :initform 1)
   (height :initform 1)
   (x-min :reader x-min)
   (x-max :reader x-max)
   (y-min :reader y-min)
   (y-max :reader y-max)))

(define-vector-sprite-writers ttf-sprite
  text size)

(define-instance-maker ttf-sprite)

(defmethod update-texture ((sprite ttf-sprite))
  (with-slots (font-object texture size width height text x-min x-max y-min
               y-max colour) sprite
    (let ((boundry (zpb-ttf:string-bounding-box text font-object))
          (em-size (zpb-ttf:units/em font-object)))
      (setf x-min (* size (/ (aref boundry 0) em-size))
            y-min (* size (/ (aref boundry 1) em-size))
            x-max (* size (/ (aref boundry 2) em-size))
            y-max (* size (/ (aref boundry 3) em-size))
            width (ceiling (+ (- x-min) x-max))
            height (ceiling (+ (- y-min) y-max)))
      (when (or (= width 0) (= height 0))
        (return-from update-texture))
      (with-vecto-canvas-as-texture (width height texture)
        (apply #'vecto:set-rgba-fill colour)
        (vecto:set-font font-object size)
        (vecto:draw-string (- x-min) (- y-min) text)))))

(defmethod diverge ((sprite ttf-sprite) &rest init-args)
  (with-slots (text size colour font-object) sprite
    (apply #'make-instance 'ttf-sprite
           (append init-args
                   (list :font-object font-object :text text :size size
                         :colour colour)))))

(defmethod free :after ((sprite ttf-sprite))
  (with-slots (font-object) sprite
    (zpb-ttf:close-font-loader font-object)))

(defun load-true-type-font (file-name)
  (check-file-existance file-name)
  (make-ttf-sprite :font-object (zpb-ttf:open-font-loader file-name)))
