; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :interact-examples)

(defun asteroids ()
  (with-display-system (:clear-colour '(0.0 0.0 0.0 1)
                        :title "Asteroids Demo"
                        :width 800
                        :height 600)
    (load-sprite-path 
     (asdf:system-relative-pathname :interact-examples #p"asteroids/sprites/"))
    (add-root-listener (make-event-assistant :quit-key :escape
                                             :fullscreen-key :F12))
    (let* ((asteroid (make-asteroid :x 100 :y 200))
           (font (diverge (sprite-node :fonts :8x16)))
           (fps-counter (make-fps-counter :font-sprite font
                                          :x 10 :y (- (screen-height)
                                                      (* (glyph-height font) 2)
                                                      10)))
           (container (make-asteroid-container :height (screen-height)
                                               :width (screen-width)))
           (controller (make-instance 'event-converter 
                                      :mappable-events '(:asteroid-explosion))))
      (desire-events controller :key-down #'handle-event)
      (map-input controller
                 (key-down-handler :e `(:asteroid-explosion :source ,asteroid)))
      (subscribe container controller)
      (subscribe controller container)
      (add-widget container asteroid)
      (add-to-root container)
      (add-to-root fps-counter))
    (with-event-loop (update-display-system))))
