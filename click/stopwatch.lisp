; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :click)

(defvar *global-stopwatch*)

(unless (= internal-time-units-per-second 1000)
  (error (format nil
                 "your CL implementation doesn't use an ~
                  INTERNAL-TIME-UNITS-PER-SECOND of 1000. ~
                  thus, your implementation isn't supported ~
                  yet.")))

(defclass stopwatch ()
  ((time-marker :initform nil)
   (run-time :initform 0)
   (running :initform nil)
   (time-func :initform nil)))

(defmethod initialize-instance :after ((watch stopwatch) &key
                                       (mode :subjective))
  (with-slots (time-func) watch
    (setf time-func (case mode
                      (:subjective (lambda () (lap *global-stopwatch*)))
                      (:objective #'get-internal-real-time)
                      (otherwise (error 'program-error
                                        "incorrect time mode"))))))

(defmethod start ((watch stopwatch))
  (with-slots (time-marker time-func run-time running) watch
    (if running
        nil
        (setf time-marker (- (funcall time-func) run-time)
              running t))))

(defmethod stop ((watch stopwatch))
  (with-slots (time-marker time-func run-time running) watch
    (if running
        (progn (setf run-time (- (funcall time-func) time-marker)
                     running nil)
               t)
        nil)))

(defmethod lap ((watch stopwatch))
  (with-slots (time-marker time-func running run-time) watch
    (if running
        (- (funcall time-func) time-marker)
        run-time)))

(defmethod reset ((watch stopwatch))
  (with-slots (time-marker running run-time) watch
    (setf time-marker nil
          run-time 0
          running nil)))

(setf *global-stopwatch* (make-instance 'stopwatch :mode :objective))
