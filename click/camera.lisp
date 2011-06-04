; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :click)

(internal *cameras*)
(defparameter *cameras* '())

(defclass camera (igo)
  ((offset-x :initform 0
             :initarg :offset-x)
   (offset-y :initform 0
             :initarg :offset-y)
   (fbo :initform (car (gl:gen-framebuffers-ext 1)))
   (texture :initform (car (gl:gen-textures 1)))
   (root :initform nil
         :initarg :root
         :accessor root)
   filters tex-u tex-v tex-height tex-width))

(define-instance-maker camera)

(defmethod initialize-instance :after ((camera camera) &key)
  (flet ((nearest-2^n-size (n)
           (expt 2 (ceiling (log n 2)))))
    (with-slots (width height fbo texture tex-u tex-v tex-width tex-height)
        camera
      (setf tex-width (nearest-2^n-size width)
            tex-height (nearest-2^n-size height)
            tex-u (float (/ width tex-width))
            tex-v (float (/ height tex-height)))
      (gl:bind-texture :texture-2d texture)
      (gl:tex-image-2d :texture-2d 0 :rgb tex-width tex-height 0 :rgb
                       :unsigned-byte (cffi:null-pointer))
      (gl:tex-parameter :texture-2d :texture-wrap-s :clamp-to-edge)
      (gl:tex-parameter :texture-2d :texture-wrap-t :clamp-to-edge)
      (gl:tex-parameter :texture-2d :texture-mag-filter :linear)
      (gl:tex-parameter :texture-2d :texture-min-filter :linear)
      (gl:bind-texture :texture-2d 0)
      (gl:bind-framebuffer-ext :framebuffer-ext fbo)
      (gl:framebuffer-texture-2d :framebuffer-ext :color-attachment0-ext
                                 :texture-2d texture 0)
      (assert (gl::enum= (gl:check-framebuffer-status-ext :framebuffer-ext)
                         :framebuffer-complete-ext) ()
              "Framebuffer ~s (camera ~s) is incomplete." fbo camera)
      (gl:bind-framebuffer-ext :framebuffer-ext 0)
      (pushnew camera *cameras*))))

(defmethod free ((camera camera))
  (with-slots (texture fbo) camera
    (gl:delete-textures `(,texture))
    (gl:delete-framebuffers-ext `(,fbo))))

;; (defmethod add-filter ((camera camera) (filter filter))
;;   (shader filter))

(defmethod activate ((camera camera))
  (with-slots (fbo offset-x offset-y tex-width tex-height) camera
    (gl:bind-framebuffer-ext :framebuffer-ext fbo)
    (gl:matrix-mode :projection)
    (gl:load-identity)
    (gl:viewport offset-x offset-y tex-width tex-height)
    (gl:ortho 0 tex-width 0 tex-height 0 1)
    (gl:matrix-mode :modelview)
    (gl:load-identity)
    (gl:clear :color-buffer-bit)))

(defmethod deactivate ((camera camera))
  (declare (ignore camera))
  (gl:bind-framebuffer-ext :framebuffer-ext 0)
  (update-display-gl))

(defmacro with-active-camera (camera &body body)
  `(unwind-protect (progn (activate ,camera) ,@body)
     (deactivate camera)))

(defmethod draw ((camera camera))
  (with-slots (root x y texture tex-u tex-v width height) camera
    (unless (null root)
      (with-active-camera camera
        (draw root))
      (gl:with-pushed-matrix
        (gl:translate x y 0)
        (gl:bind-texture :texture-2d texture)
        (gl:with-primitives :quads
          (gl:tex-coord 0.0 0.0)
          (gl:vertex 0 0)
          (gl:tex-coord tex-u 0.0)
          (gl:vertex width 0)
          (gl:tex-coord tex-u tex-v)
          (gl:vertex width height)
          (gl:tex-coord 0.0 tex-v)
          (gl:vertex 0 height))))))

(defun delete-all-cameras ()
  (map nil #'free *cameras*)
  (setf *cameras* '()))
