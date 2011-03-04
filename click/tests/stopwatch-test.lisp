; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :click)

(defclass stopwatch-test (test-case)
  ())

(defmethod tear-down ((test stopwatch-test))
  (reset *global-stopwatch*))

(defmethod start-up ((test stopwatch-test))
  (reset *global-stopwatch*))

(def-test-method test-stopwatch ((test stopwatch-test))
  (flet ((almost-equal (a b &optional (delta 1))
           (<= (abs (- a b)) delta)))
    (let ((watch (make-instance 'stopwatch :mode :objective))
          (time 0))
      (assert-equal 0 (lap watch))
      (setf time (lap watch))
      (start watch)
      (sleep 0.01)
      (setf time (lap watch))
      (assert-true (almost-equal 10 time))
      (stop watch)
      (sleep 0.01)
      (assert-equal time (lap watch))
      (start watch)
      (sleep 0.01)
      (assert-true (> (lap watch) time))
      (reset watch)
      (assert-equal 0 (lap watch))
      (setf watch (make-instance 'stopwatch :mode :subjective))
      (start watch)
      (sleep 0.01)
      (assert-equal 0 (lap watch))
      (start *global-stopwatch*)
      (sleep 0.01)
      (assert-true (almost-equal 10 (lap watch))))))
