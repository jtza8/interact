; Copyright 2009 Jens Thiede. All rights reserved.
; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :click)

(defclass listening-test (test-case)
  ())

(def-test-method test-listenable ((test listening-test))
  (let ((dummy (make-instance 'dummy-widget))
        (listener-1 (make-instance 'dummy-listener))
        (listener-2 (make-instance 'dummy-listener)))
    (assert-equal (listeners dummy) '())
    (add-listener dummy listener-1 :dummy-event)
    (assert-equal `(:dummy-event ,(list listener-1))
                  (listeners dummy))
    (assert-condition 'invalid-event-type
                      (add-listener dummy listener-1 :nonexistent-event))
    (add-listener dummy listener-2 :dummy-event)
    (assert-equal `(:dummy-event (,listener-2 ,listener-1))
                  (listeners dummy))
    (assert-condition 'invalid-event
                      (add-listener dummy listener-1 :bogus-event))
    (remove-listener dummy listener-1 :dummy-event)
    (assert-equal `(:dummy-event (,listener-2)) (listeners dummy))
    (assert-condition 'invalid-event
                      (remove-listener dummy listener-1 :dumy-event))
    (assert-equal `(:dummy-event (,listener-2)) (listeners dummy))
    (add-listener dummy listener-1 :dummy-event)
    (send-event dummy '(:dummy-event 1 4 2))
    (assert-equal '(:dummy-event 1 4 2) (latest-event listener-1))
    (assert-equal '(:dummy-event 1 4 2) (latest-event listener-2))))