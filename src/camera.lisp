; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :interact)

(internal *cameras*)
(defparameter *cameras* '())

(defclass camera (widget)
  ((x-offset :initform 0
             :initarg :x-offset)
   (y-offset :initform 0
             :initarg :y-offset)
   (fbo :initform (car (gl:gen-framebuffers-ext 1)))
   (texture :initform (car (gl:gen-textures 1))
            :reader texture)
   (root :initform nil
         :initarg :root
         :reader root)
   (filter :initform nil
           :initarg :filter
           :accessor filter)
   shaders tex-u tex-v tex-height tex-width))

(defmethod initialize-instance :after ((camera camera) &key root)
  (unless (null root)
    (setf (root camera) root))
  (flet ((nearest-2^n-size (n)
           (expt 2 (ceiling (log n 2)))))
    (with-slots (width height fbo texture tex-u tex-v tex-width tex-height)
        camera
      (setf tex-width (nearest-2^n-size width)
            tex-height (nearest-2^n-size height)
            tex-u (float (/ width tex-width))
            tex-v (float (/ height tex-height)))
      (gl:bind-texture :texture-2d texture)
      (gl:tex-image-2d :texture-2d 0 :rgba tex-width tex-height 0 :rgba
                       :unsigned-byte (cffi:null-pointer))
      (gl:tex-parameter :texture-2d :texture-wrap-s :clamp-to-edge)
      (gl:tex-parameter :texture-2d :texture-wrap-t :clamp-to-edge)
      (gl:tex-parameter :texture-2d :texture-mag-filter :linear)
      (gl:tex-parameter :texture-2d :texture-min-filter :linear)
      (gl:bind-texture :texture-2d 0)
      (when (zerop fbo)
        (error 'camera-error :reason :creation))
      (gl:bind-framebuffer-ext :framebuffer-ext fbo)
      (gl:framebuffer-texture-2d :framebuffer-ext :color-attachment0-ext
                                 :texture-2d texture 0)
      (unless (gl::enum= (gl:check-framebuffer-status-ext :framebuffer-ext)
                         :framebuffer-complete-ext)
        (error 'camera-error :reason :incomplete-framebuffer))
      (gl:bind-framebuffer-ext :framebuffer-ext 0)
      (pushnew camera *cameras*))))

(defmethod free ((camera camera))
  (with-slots (texture fbo) camera
    (gl:delete-textures `(,texture))
    (gl:delete-framebuffers-ext `(,fbo))))

(defmethod (setf root) (value (camera camera))
  (with-slots (root) camera
    (setf root value)))

(defmethod subscription-request :before ((camera camera) (listener listener)
                                      event-type)
  (when (null event-type)
    (with-slots (parent root) camera
      (assert (not (null root)) ()
              'camera-error :reason :listening-without-root)
      (subscribe parent root))))

(defmethod unsubscription-notice :before ((camera camera) (listener listener)
                                            event-type)
  (when (null event-type)
    (with-slots (parent root) camera
      (unsubscribe parent root))))

(defmethod activate ((camera camera))
  (with-slots (fbo x-offset y-offset tex-width tex-height) camera
    (gl:bind-framebuffer-ext :framebuffer-ext fbo)
    (gl:matrix-mode :projection)
    (gl:load-identity)
    (gl:viewport x-offset y-offset tex-width tex-height)
    (gl:ortho 0 tex-width 0 tex-height 0 1)
    (gl:matrix-mode :modelview)
    (gl:load-identity)
    (gl:with-pushed-attrib (:color-buffer-bit)
      (gl:clear-color 0.0 0.0 0.0 0.0)
      (gl:clear :color-buffer-bit))))

(defmethod deactivate ((camera camera))
  (declare (ignore camera))
  (gl:bind-framebuffer-ext :framebuffer-ext 0)
  (update-display-gl))

(defmacro with-active-camera (camera &body body)
  `(unwind-protect (progn (activate ,camera) ,@body)
     (deactivate camera)))

(defmethod draw ((camera camera))
  (with-slots (root filter x y texture tex-u tex-v tex-width 
               tex-height width height x-offset y-offset) camera
    (unless (null root)
      (with-active-camera camera
        (draw root))
      (gl:with-pushed-matrix
        (gl:translate x y 0)
        (gl:bind-texture :texture-2d texture)
        (unwind-protect
             (progn
               (when (typep filter 'filter)
                 (activate filter)
                 (gl:active-texture :texture0)
                 (set-uniform filter "clk_scene" :int 0)
                 (set-uniform filter "clk_scale"
                              :vec `#(,tex-width ,tex-height))
                 (set-uniform filter "clk_offset" :vec `#(,x-offset ,y-offset)))
               (rectangle 0 0 width height
                          :tex-coords (list 0 0 tex-u 0 tex-u tex-v 0 tex-v)))
          (when (typep filter 'filter)
            (deactivate filter)))))))

(internal delete-all-cameras)
(defun delete-all-cameras ()
  (map nil #'free *cameras*)
  (setf *cameras* '()))
