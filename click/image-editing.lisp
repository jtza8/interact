; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :click)

(internal memcpy)
(cffi:defcfun ("memcpy" memcpy) :pointer
  (dest :pointer) (src :pointer) (size :unsigned-int))

(internal power-size)
(defun power-size (value base)
  (expt base (ceiling (log value base))))

(defun natural-power-p (value base)
  (= (rem (log value base) 1) 0))

(internal image-to-sprite)
(defun image-to-sprite (&optional destructive (image :current-image))
  (il:with-bound-image image
    (let* ((texture (car (gl:gen-textures 1)))
           (format (il:image-format))
           (type (il:image-type))
           (width (il:image-width))
           (height (il:image-height))
           (ideal-width (power-size width 2))
           (ideal-height (power-size height 2))
           (image-copy (il:gen-image)))
      (assert (eq type :unsigned-byte) () 'image-type-error :actual-type type)
      (ilu:flip-image)
      (unless (and (natural-power-p width 2) (natural-power-p height 2))
        (if destructive
            (progn
              (ilu:image-parameter :placement :lower-left)
              (ilu:enlarge-canvas ideal-width ideal-height 1))
            (progn
              (let ((original-image (il:get-integer :cur-image)))
                (il:bind-image image-copy)
                (il:tex-image ideal-width ideal-height 1
                              (il:image-bytes-per-pixel original-image)
                              (il:image-format original-image)
                              (il:image-type original-image)
                              (cffi:null-pointer))
                (overlay-image original-image 0 0 0)))))
      (gl:bind-texture :texture-2d texture)
      (gl:tex-parameter :texture-2d :texture-min-filter :linear)
      (gl:tex-image-2d :texture-2d 0 format ideal-width ideal-height 0
                       format :unsigned-byte (il:get-data))
      (il:delete-images image-copy)
      (make-instance 'texture-sprite :texture texture
                     :width width :height height
                     :texture-width ideal-width
                     :texture-height ideal-height))))

(defun load-image-sprite (file)
  (il:with-images (image)
    (il:with-bound-image (setf image (il:gen-image))
      (il:enable :origin-set)
      (il:origin-func :origin-lower-left)
      (il:load-image (namestring file))
      (case (il:image-format)
        ((:rgb :bgr) (il:convert-image :rgb :unsigned-byte))
        ((:rgba :bgra) (il:convert-image :rgba :unsigned-byte))
        ((:luminance) (il:convert-image :luminance :unsigned-byte))
        ((:luminance-alpha) (il:convert-image :luminance-alpha :unsigned-byte))
        (otherwise (error 'image-format-error 
                          :actual-format (il:image-format))))
      (image-to-sprite t))))

(internal image-data-pos)
(defun image-data-pos (x y &optional (image :current-image))
  (il:with-bound-image image
    (let ((width (il:image-width))
          (height (il:image-height)))
      (check-pixel-index (and (<= 0 x width) (<= 0 y height))
                         (format nil "invalid index: (~d, ~d) ~
                                      image width: ~d, ~
                                      image height: ~d"
                                 x y width height)
                         (x y))
      (cffi:inc-pointer (il:get-data) (* (+ x (* y width))
                                         (il:image-bytes-per-pixel))))))

(internal blit)
(defun blit (source dest-x dest-y dest-z src-x src-y src-z width height depth
             &key (allow-clipping t))
  (declare (ignore dest-z src-z depth))
  (let* ((dest-width (il:image-width))
         (dest-height (il:image-height))
         (data-format (il:image-format))
         (data-type (il:image-type))
         (bytes-per-pixel (il:image-bytes-per-pixel))
         (src-width (il:image-width source))
         (src-height (il:image-height source))
         (src-row-size (* (min width (- dest-width dest-x))
                          bytes-per-pixel)))
    (check-pixel-index (>= src-width width) "width argument too large")
    (check-pixel-index (>= src-height height) "height argument too large")
    (unless allow-clipping
      (check-pixel-index (>= (- dest-width dest-x) width)
                         "clipping not alowed, x axis is clipped")
      (check-pixel-index (>= (- dest-height dest-y) height)
                         "clipping not alowed, y axis is clipped"))
    (il:enable :origin-set)
    (il:origin-func (il:image-origin))
    (il:with-images (source-clone)
      (let* ((image (if (and (eq (il:image-type source) data-type)
                             (eq (il:image-format source) data-format)
                             (eq (il:image-origin source) (il:image-origin)))
                        source
                        (il:with-bound-image source-clone
                          (il:copy-image source)
                          (il:convert-image data-format data-type)
                          source-clone))))
        (loop for y from 0 upto (1- (min (- dest-height dest-y) height))
           do (memcpy (image-data-pos dest-x (+ y dest-y))
                      (image-data-pos src-x (+ y src-y) image)
                      src-row-size))))))

(internal overlay-image)
(defun overlay-image (source x y z &key (allow-clipping t))
  (blit source x y z 0 0 0 (il:image-width source) (il:image-height source) 1
        :allow-clipping allow-clipping))

(internal write-pixel-header)
(defun write-pixel-header (image &rest arguments)
  (il:with-bound-image image
    (let* ((width (il:image-width))
           (height (il:image-height))
           (bytes-per-pixel (il:image-bytes-per-pixel))
           (header-byte-size (* (reduce #'+ arguments :key 
                                        (lambda (x)
                                          (cffi:foreign-type-size (car x))))
                                bytes-per-pixel))
           (pixel-count (ceiling (/ header-byte-size bytes-per-pixel)))
           (header-pixel-size (* pixel-count bytes-per-pixel))
           (header-row-size (* width bytes-per-pixel))
           (pointer (ecase (il:image-origin)
                      (:origin-lower-left (image-data-pos 0 (1- height)))
                      (:origin-upper-left (image-data-pos 0 0)))))
      (assert (>= header-row-size header-pixel-size) ()
              "too little space to write header in")
      (loop for (type value) in arguments
            do (progn
                 (setf (cffi:mem-aref pointer type) value)
                 (cffi:incf-pointer pointer
                                    (* (ceiling (/ (cffi:foreign-type-size type)
                                                   bytes-per-pixel))
                                       bytes-per-pixel)))))))

(internal read-pixel-header)
(defun read-pixel-header (image &rest arguments)
  (il:with-bound-image image
    (let* ((height (il:image-height))
           (bytes-per-pixel (il:image-bytes-per-pixel))
           (pointer (ecase (il:image-origin)
                      (:origin-lower-left (image-data-pos 0 (1- height)))
                      (:origin-upper-left (image-data-pos 0 0)))))
      (loop for type in arguments
            collect (let ((value (cffi:mem-aref pointer type)))
                      (cffi:incf-pointer pointer 
                                         (* (ceiling
                                             (/ (cffi:foreign-type-size type)
                                                bytes-per-pixel))
                                            bytes-per-pixel))
                      value)))))