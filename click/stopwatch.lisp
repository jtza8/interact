; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :click)

(cffi:defcfun ("gettimeofday" %get-time-of-day) :int)
(cffi:defcstruct timeval-cstruct 
  (seconds time-ctype)
  (useconds suseconds-ctype))

(defun get-ticks ()
  (cffi:with-foreign-object (timeval 'timeval-cstruct)
    (cffi:with-foreign-slots ((seconds useconds) timeval timeval-cstruct)
      (+ (* seconds 1000000) useconds))))

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
