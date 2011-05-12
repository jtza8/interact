; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :click-examples)

(defun asteroids ()
  (with-display-system (screen-bg-color '(0 0 0 1)
                        window-title "Asteroids Demo"
                        screen-width 1280 screen-height 800)
    (load-sprite-path 
     (asdf:system-relative-pathname :click-examples #p"asteroids/sprites"))
    (add-root-listener
     (make-instance 'event-assistant
                    :quit-key :escape
                    :fullscreen-key :F12))
    (let ((asteroid (make-instance 'asteroid :x 100 :y 200))
          (container (make-instance 'asteroid-container
                                    :height (screen-height)
                                    :width (screen-width)))
          (controller (make-instance 'event-converter
                                     :mappable-events '(:asteroid-explosion))))
      (desire-events controller :key-down #'handle-event)
      (map-input controller
                 (key-down-handler :e `(:asteroid-explosion :source ,asteroid)))
      (add-listener container controller)
      (add-listener controller container)
      (add-igo container asteroid)
      (add-root-igo container))))