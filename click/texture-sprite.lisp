; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :click)

(defclass texture-sprite (sprite)
  ((texture :initarg :texture
            :initform (error "No texture name given.")
            :reader texture)
   (texture-width :initarg :texture-width
                  :initform (error "No texture-width given."))
   (texture-height :initarg :texture-height
                   :initform (error "No texture-height given."))
   ;; (vbo :initarg :vbo
   ;;      :initform (car (gl:gen-buffers 1)))
   ))

;; (defmethod update-vbo ((sprite texture-sprite))
;;   (with-slots (vbo width height texture-width texture-height) sprite
;;     (let* ((entry-type :int)
;;            (entry-size (cffi:foreign-type-size entry-type))
;;            (entries (list 0 0 0 0 width 0 width 0 width height
;;                           width height 0 height 0 height))
;;            (entry-count (length entries)))
;;       (cffi:with-foreign-object (data entry-type entry-count)
;;         (let ((pointer data))
;;           (dolist (entry entries)
;;             (setf (cffi:mem-aref pointer entry-type) entry)
;;             (cffi:incf-pointer pointer entry-size)))
;;         (gl:bind-buffer :array-buffer vbo)
;;         (%gl:buffer-data :array-buffer (* entry-size entry-count) data
;;                          :static-draw)
;;         (%gl:tex-coord-pointer 2 entry-type (* entry-size 4)
;;                                (cffi:null-pointer))
;;         (%gl:vertex-pointer 2 entry-type (* entry-size 4)
;;                             (cffi:null-pointer))
;;         (gl:bind-buffer :array-buffer 0)))))

;; (defmethod initialize-instance :after ((sprite texture-sprite) &key)
;;   (when (use-vbo)
;;     (update-vbo sprite)))

(defmethod draw-sprite ((sprite texture-sprite) &key (x 0) (y 0) width height
                        (mode :tile))
  (declare (ignore mode width height))
  (with-slots (texture texture-width texture-height width height vbo) sprite
    (gl:bind-texture :texture-2d texture)
    (gl:tex-parameter :texture-2d :texture-wrap-s :repeat)
    (gl:tex-parameter :texture-2d :texture-wrap-t :repeat)
    (gl:matrix-mode :texture)
    (gl:with-pushed-matrix
      (gl:scale (/ 1 texture-width) (/ 1 texture-height) 1)
      (rectangle x y width height
                 :tex-coords (list 0 0 width 0 width height 0 height))))
  (gl:matrix-mode :modelview))

(defmethod free ((sprite texture-sprite))
  (with-slots (vbo texture) sprite
    (gl:delete-buffers (list vbo))
    (gl:delete-textures (list texture))))
