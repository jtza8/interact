; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package #:interact-examples)

(export 'sprite-zoo)
(defun sprite-zoo ()
  (with-display-system (:width 800 :height 600
                        :title "Sprite Zoo"
                        :clear-colour '(0.2 0.2 0.2 1.0))
    (load-asdf-sprite-path :interact-examples "sprite-zoo/sprites/")
    (let ((label-text (clone (sprite-node :fonts :vera)
                             :colour '(0.5 0.5 0.5 1.0)
                             :size 12))
          (subtitle-text (clone (sprite-node :fonts :vera)
                             :colour '(0.8 0.8 0.8 1.0)
                             :size 20)))
     (make-root-widgets
       (painter :x 20 :y 20
                :sprite (clone subtitle-text :text "Static Sprites"))
       (painter :x 58 :y 58 :sprite (sprite-node :images :flower-rgba))
       (painter :x 98 :y 196 :sprite (clone label-text :text "RGBA"))
       (painter :x 243 :y 58 :sprite (sprite-node :images :flower-rgb))
       (painter :x 294 :y 196 :sprite (clone label-text :text "RGB"))
       (painter :x 429 :y 58 :sprite (sprite-node :images :flower-grey))
       (painter :x 462 :y 196 :sprite (clone label-text :text "Greyscale"))
       (painter :x 614 :y 58 :sprite (sprite-node :images :flower-bw))
       (painter :x 638 :y 196
                :sprite (clone label-text :text "Black & White"))
       (painter :x 20 :y 216
                :sprite (clone subtitle-text :text "Vector Sprites"))
       (painter :x 20 :y 246
                :sprite (make-instance 'polygon-sprite
                          :width 32
                          :height 32
                          :fill-colour '(0.5 0.5 0.5 1.0)
                          :line-colour '(0.0 0.0 0.0 0.0)
                          :points #(#(15 0) #(30 30) #(0 30))))))
    (do-event-loop t
      (basic-event-handler)
      (update-display-system))))