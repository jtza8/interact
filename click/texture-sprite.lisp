; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :click)

(defclass texture-sprite (sprite)
  ((texture :initarg :texture
            :initform (error "No texture name given.")
            :reader texture)
   (vbo :initarg :vbo
        :initform (car (gl:gen-buffers 1)))))

(defmethod update-vbo ((sprite texture-sprite))
  (with-slots (vbo width height) sprite
    (let* ((entry-type :int)
           (entry-size (cffi:foreign-type-size entry-type))
           (vertex-count 4)
           (uv-count 4)
           (entries (list 0 0 0 0 1 0 width 0 1 1 width height 0 1 0 height))
           (entry-count (length entries)))
      (cffi:with-foreign-object (data entry-type entry-count)
        (let ((pointer data))
          (dolist (entry entries)
            (setf (cffi:mem-aref pointer entry-type) entry)
            (cffi:incf-pointer pointer entry-size)))
        (gl:bind-buffer :array-buffer vbo)
        (%gl:buffer-data :array-buffer (* entry-size entry-count) data
                         :static-draw)
        (%gl:tex-coord-pointer 2 entry-type (* entry-size 4)
                               (cffi:null-pointer))
        (%gl:vertex-pointer 2 :int (* entry-size 4)
                            (cffi:make-pointer (* entry-size 2)))
        (gl:bind-buffer :array-buffer 0)))))

(defmethod initialize-instance :after ((sprite texture-sprite) &key)
  (update-vbo sprite))

(defmethod draw-sprite ((sprite texture-sprite) &key (x 0) (y 0) width height
                        (mode :tile))
  (declare (ignore mode x y width height))
  (with-slots ((normal-width width) (normal-height height) texture vbo) sprite
    ;; (gl:scale texture-width texture-height 1)
    (gl:bind-texture :texture-2d texture)
    (gl:tex-parameter :texture-2d :texture-wrap-s :repeat)
    (gl:tex-parameter :texture-2d :texture-wrap-t :repeat)
    (when (null width) (setf width normal-width))
    (when (null height) (setf height normal-height))
    (gl:bind-buffer :array-buffer vbo)
    (gl:enable-client-state :texture-coord-array)
    (gl:enable-client-state :vertex-array)
    (%gl:draw-arrays :quads 0 4)
    (gl:disable-client-state :vertex-array)
    (gl:disable-client-state :texture-coord-array)
    ;; (rectangle x y width height
    ;;            :tex-coords (list 0 0
    ;;                              (/ width normal-width) 0
    ;;                              (/ width normal-width) (/ height normal-height)
    ;;                              0 (/ height normal-height)))
    (gl:bind-buffer :array-buffer 0)
    ))

(defmethod free ((sprite texture-sprite))
  (gl:delete-textures (list (texture sprite))))
