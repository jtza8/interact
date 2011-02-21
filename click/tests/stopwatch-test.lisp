; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :click)

(defclass stopwatch-test (test-case)
  ())

(def-test-method test-stopwatch ((test stopwatch-test))
  (let ((watch (make-instance 'stopwatch))
        (time 0))
    (assert-equal 0 (lap watch))
    (setf time (lap watch))
    (start watch)
    (sleep 1)
    (setf time (lap watch))
    (let ((delta (abs (- 1000 time))))
      (assert-true (< delta 2)
                   (format nil "delta is ~a, expected to be less than 2"
                           delta)))
    (stop watch)
    (assert-equal time (lap watch))
    (start watch)
    (sleep 0.01)
    (assert-true (> (lap watch) time))
    (reset watch)
    (assert-equal 0 (lap watch))))