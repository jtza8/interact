; Copyright 2009 Jens Thiede. All rights reserved.
; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :click)

(defclass window (widget)
  ((title :initarg :title
          :initform "Untitled")
   (visible :initarg :visible
            :initform t)
   (widgets :initform '())
   (tags :initform '())
   (themed :initform (not (null *theme-path*)))))

(defmethod initialize-instance :after ((window window) &key)
  (assert-window-manager-exists)
  (with-slots (left-margin right-margin top-margin bottom-margin) window
    (with-node-images  (corner-left-top corner-right-bottom)
        (fetch-image-node :window :shadow)
      (setf left-margin (sdl:width corner-left-top)
            right-margin (sdl:width corner-right-bottom)
            top-margin (sdl:height corner-left-top)
            bottom-margin (sdl:height corner-right-bottom))))
  (make-texture window)
  (add-window *window-manager* window))

(defmethod make-texture ((window window))
  (with-slots (x y width height left-margin right-margin top-margin
               bottom-margin texture) window
    (with-node-images  (corner-left-top corner-right-top corner-right-bottom
                        corner-left-bottom top-01 top-02 right-01 right-02
                        bottom-01 bottom-02 left-01 left-02)
        (fetch-image-node :window :shadow)
      (let ((surface-width (+ left-margin width right-margin))
            (surface-height (+ top-margin height bottom-margin)))
        (sdl:with-surface 
            (surface (sdl:create-surface surface-width
                                         surface-height
                                         :pixel-alpha t))
          (setf (sdl:x top-01) left-margin
                (sdl:y left-01) top-margin
                (sdl:x corner-right-top) (- surface-width right-margin)
                (sdl:x corner-right-bottom) (- surface-width right-margin)
                (sdl:y corner-right-bottom) (- surface-height bottom-margin)
                (sdl:y corner-left-bottom) (- surface-height bottom-margin))
          (dolist (image (list corner-left-top corner-right-top
                               corner-right-bottom corner-left-bottom
                               top-01 left-01))
            (sdl:blit-surface image))
          (setf texture (surface-to-texture surface)))))))

;        (draw-at top-01 x (- y (height top-01)))
;        (draw-at corner-right-top (+ x width) (- y (height corner-right-top)))))))