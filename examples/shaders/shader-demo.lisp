; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :interact-examples)

(defun shader-demo ()
  (with-display-system (screen-width 640
                        screen-height 480)
    (load-sprite-path (asdf:system-relative-pathname
                       :interact-examples "shaders/sprites/"))
    (let* ((background (make-painter :sprite (sprite-node :checker)
                                     :width (screen-width)
                                     :height (screen-height)))
           (camera (make-camera :root background
                                :width (screen-width)
                                :height (screen-height)))
           (shader (make-instance 'warp-shader :warp-count 3))
           (filter (make-filter)))
      (add-shader filter shader)
      (link-filter filter)
      (set-uniform filter "warps[0]" :vec #(320 240 64))
      (set-uniform filter "warps[1]" :vec #(240 240 64))
      (set-uniform filter "warps[2]" :vec #(400 240 64))
      (setf (filter camera) filter)
      (add-to-root camera))))
