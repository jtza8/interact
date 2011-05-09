; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :click)

(internal memcpy)
(cffi:defcfun ("memcpy" memcpy) :pointer
  (dest :pointer) (src :pointer) (size :unsigned-int))

(internal image-to-sprite)
(defun image-to-sprite (&optional (image :current-image))
  (il:with-bound-image image
    (let ((texture (car (gl:gen-textures 1)))
          (format (il:image-format))
          (type (il:image-type))
          (width (il:image-width))
          (height (il:image-height)))
      (assert (eq type :unsigned-byte) () 'image-type-error :actual-type type)
      (gl:bind-texture :texture-2d texture)
      (gl:tex-parameter :texture-2d :texture-min-filter :linear)
      (gl:tex-image-2d :texture-2d 0 format width height 0
                       format :unsigned-byte (il:get-data))
      (make-instance 'texture-sprite :texture texture :width width
                     :height height))))

(defun load-image-sprite (file)
  (il:with-images (image)
    (il:with-bound-image (setf image (il:gen-image))
      (il:load-image (namestring file))
      (case (il:image-format)
        ((:rgb :bgr) (il:convert-image :rgb :unsigned-byte))
        ((:rgba :bgra) (il:convert-image :rgba :unsigned-byte))
        ((:luminance) (il:convert-image :luminance :unsigned-byte))
        ((:luminance-alpha) (il:convert-image :luminance-alpha :unsigned-byte))
        (otherwise (error 'image-format-error 
                          :actual-format (il:image-format))))
      (ilu:flip-image)
      (image-to-sprite))))

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
                          (il:check-error)
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
           (header-byte-size (* (reduce #'+ arguments :key #'car)
                                bytes-per-pixel))
           (pixel-count (ceiling (/ header-byte-size bytes-per-pixel)))
           (header-pixel-size (* pixel-count bytes-per-pixel))
           (header-row-size (* width bytes-per-pixel))
           (pointer (ecase (il:image-origin)
                      (:origin-lower-left (image-data-pos 0 (1- height)))
                      (:origin-upper-left (image-data-pos 0 0)))))
      (assert (>= header-row-size header-pixel-size) ()
              "too little space to write header in")
      (memset pointer #xff header-row-size)
      (loop for (size value) in arguments
            do (destructuring-bind (type subtrahend)
                   (ecase size
                     (1 '(:uint8 #xff))
                     (2 '(:uint16 #xffff))
                     (4 '(:uint32 #xffffffff)))
                 (setf (cffi:mem-aref pointer type) (- subtrahend value))
                 (cffi:incf-pointer pointer
                                    (* (ceiling (/ size bytes-per-pixel))
                                       bytes-per-pixel)))))))

(internal read-pixel-header)
(defun read-pixel-header (image &rest arguments)
  (il:with-bound-image image
    (let* ((height (il:image-height))
           (bytes-per-pixel (il:image-bytes-per-pixel))
           (pointer (ecase (il:image-origin)
                      (:origin-lower-left (image-data-pos 0 (1- height)))
                      (:origin-upper-left (image-data-pos 0 0)))))
      (loop for size in arguments
            collect (destructuring-bind (type subtrahend)
                        (ecase size
                          (1 '(:uint8 #xff))
                          (2 '(:uint16 #xffff))
                          (4 '(:uint32 #xffffffff)))
                      (let ((value (- subtrahend (cffi:mem-aref pointer type))))
                        (cffi:incf-pointer pointer 
                                           (* (ceiling (/ size bytes-per-pixel))
                                              bytes-per-pixel))
                        value))))))