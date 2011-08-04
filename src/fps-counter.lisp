; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :interact)

(defclass fps-counter (widget)
  ((timer :initform (make-instance 'watch))
   (display-frames :initform 0)
   (backstage-frames :initform 0)
   display-fps-line
   backstage-fps-line))

(define-instance-maker fps-counter)

(defmethod initialize-instance :after ((widget fps-counter) &key font-sprite)
  (with-slots (display-fps-line backstage-fps-line) widget
    (when (null font-sprite)
      (error "must specify font-sprite"))
    (setf display-fps-line (diverge font-sprite)
          backstage-fps-line (diverge font-sprite))
    (desire-events widget
                   :display-update #'display-update-event
                   :before-frame #'before-frame-event)))

(internal display-update-event)
(defmethod display-update-event ((widget fps-counter) event)
  (declare (ignore event))
  (with-slots (timer display-fps-line backstage-fps-line display-frames
               backstage-frames) widget
    (incf display-frames)
    (if (= (lap timer) 0)
        (start timer)
        (if (>= (lap timer) 500000000)
            (progn
              (setf (text display-fps-line)
                    (format nil "FPS: ~,2f"
                            (* display-frames 2 (/ 500000000 (lap timer))))
                    display-frames 0
                    (text backstage-fps-line)
                    (format nil "IPS: ~,2f"
                            (* backstage-frames 2 (/ 500000000 (lap timer))))
                    backstage-frames 0)
              (reset timer t))))))

(internal before-frame-event)
(defmethod before-frame-event ((widget fps-counter) event)
  (incf (slot-value widget 'backstage-frames)))

(defmethod draw ((widget fps-counter))
  (with-slots (display-fps-line backstage-fps-line) widget
    (draw-sprite display-fps-line)
    (draw-sprite backstage-fps-line :y (glyph-height display-fps-line))))
