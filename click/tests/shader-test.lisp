; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :click)

(defclass shader-test (test-case)
  ())

(def-test-method test-shader ((test shader-test))
  (assert-condition 'shader-error (make-shader))
  (with-display-system (screen-width 640
                        screen-height 480)
    (let* ((target (make-simple-igo :sprite
                     (make-colour-sprite :colour '(1.0 0.0 0.0 1.0)
                                         :width 100
                                         :height 100)))
           (camera (make-camera :root target :width 640 :height 480
                                :y (- (screen-height) 110)
                                :x 10))
           (shader (make-shader :source-code "void foo() {}")))
      (compile-shader shader)
      (setf (source-code shader) "void bogus(#) {}")
      (assert-condition 'shader-error (compile-shader shader))
      (add-to-root camera))
    (sdl:push-quit-event)))