; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :click)

(defclass listening-test (test-case)
  ())

(def-test-method test-listenable ((test listening-test))
  (let ((dummy (make-instance 'dummy-igo))
        (listener-1 (make-instance 'dummy-listener))
        (listener-2 (make-instance 'dummy-listener)))
    (flet ((set-up-variables ()
             (setf dummy (make-instance 'dummy-igo)
                   listener-1 (make-instance 'dummy-listener)
                   listener-2 (make-instance 'dummy-listener))))
      (set-up-variables)
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
      (remove-listener dummy listener-1 :dummy-event)
      (assert-equal `(:dummy-event (,listener-2)) (listeners dummy))
      (add-listener dummy listener-1 :dummy-event)
      (send-event dummy '(:dummy-event 1 4 2))
      (assert-equal '(:dummy-event 1 4 2) (latest-event listener-1))
      (assert-equal '(:dummy-event 1 4 2) (latest-event listener-2))
      (set-up-variables)
      (add-listener dummy listener-1)
      (send-event dummy #1='(:dummy-event 1 3 5)) 
      (assert-equal #1# (latest-event listener-1))
      (assert-false (equal #1# (latest-event listener-2)))
      (assert-true (find listener-1 (getf (listeners dummy) :dummy-event)))
      (remove-listener dummy listener-1)
      (assert-false (find listener-1 (getf (listeners dummy) :dummy-event))))))
