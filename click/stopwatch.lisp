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
   (running :initform nil :reader running-p)
   (time-func :initform nil)))

(define-instance-maker stopwatch)

(defmethod initialize-instance :after ((watch stopwatch) &key
                                       (mode :local))
  (with-slots (time-func) watch
    (setf time-func (case mode
                      (:local (lambda () (lap *global-stopwatch*)))
                      (:global #'get-internal-real-time)
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

(defmethod reset ((watch stopwatch) &optional (auto-start nil))
  (with-slots (time-marker running run-time) watch
    (setf time-marker nil
          run-time 0
          running nil))
  (when auto-start
    (start watch)))

(internal *global-stopwatch*)
(setf *global-stopwatch* (make-instance 'stopwatch :mode :global))

(internal *frame-stopwatch*)
(defparameter *frame-stopwatch* (make-stopwatch))
(declaim (inline frame-time))
(defun frame-time () (lap *frame-stopwatch*))