; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :click-examples)

(defun shader-demo ()
  (with-display-system (screen-width 800
                        screen-height 600)
    (load-sprite-path (asdf:system-relative-pathname
                       :click-examples "shaders/sprites/"))
    (let* ((background (make-painter :sprite (sprite-node :checker)
                                     :width (screen-width)
                                     :height (screen-height)))
           (camera (make-camera :root background
                                :width (screen-width)
                                :height (screen-height)))
           (shader (make-instance 'warp-shader))
           (filter (make-filter)))
      (add-shader filter shader)
      (link-filter filter)
      (set-uniform filter "click_scene" :int 0)
      (set-uniform filter "warps[0]" :vec #(10.3 10.3 3.0))
      (setf (filter camera) filter)
      (add-to-root camera))))
