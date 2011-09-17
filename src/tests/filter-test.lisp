; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :interact)

(defparameter *shader-test-code*
  "void main()
  {
    gl_FragColor = vec4(1.0, 0.0, 0.0, 1.0);
  }")

(defclass filter-test (test-case)
  ())

(def-test-method test-filter ((test filter-test))
  (with-display-system ()
    (let ((shader (make-instance 'shader))
          (filter (make-instance 'filter)))
      (assert-condition 'filter-error (add-shader filter shader))
      (setf (source-code shader) *shader-test-code*)
      (add-shader filter shader)
      (link-filter filter)
      (setf *quit-state* t))))

(defun test-filter-manually ()
  (with-display-system ()
    (let ((shader (make-instance 'shader :source-code *shader-test-code*))
          (filter (make-instance 'filter))
          (red (make-instance 'painter :sprite (make-instance 'polygon-sprite :width 256 :height 256
                                                          :points #(#(0 0) #(100 0)
                                                                    #(0 100)))))
          (camera (make-instance 'camera :width 256 :height 256)))
      (setf (source-code shader) *shader-test-code*)
      (add-shader filter shader)
      (link-filter filter)
      (setf (root camera) red
            (filter camera) filter)
      (add-to-root camera))))