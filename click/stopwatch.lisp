; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :click)

(unless (= internal-time-units-per-second 1000)
  (error (format nil
                 "your CL implementation doesn't use an ~
                  INTERNAL-TIME-UNITS-PER-SECOND of 1000. ~
                  thus, your implementation isn't supported ~
                  yet.")))

(defclass stopwatch ()
  ((time-marker :initform nil)
   (run-time :initform 0)
   (running :initform nil)))

(defmethod start ((watch stopwatch))
  (with-slots (time-marker run-time running) watch
    (if running
        nil
        (setf time-marker (- (get-internal-real-time) run-time)
              running t))))

(defmethod stop ((watch stopwatch))
  (with-slots (time-marker run-time running) watch
    (if running
        (progn (setf run-time (- (get-internal-real-time) time-marker)
                     running nil)
               t)
        nil)))

(defmethod lap ((watch stopwatch))
  (with-slots (time-marker running run-time) watch
    (if running
        (- (get-internal-real-time) time-marker)
        run-time)))

(defmethod reset ((watch stopwatch))
  (with-slots (time-marker running run-time) watch
    (setf time-marker nil
          run-time 0
          running nil)))
