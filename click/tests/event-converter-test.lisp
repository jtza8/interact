; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :click)

(defclass event-converter-test (test-case)
  ())

(def-test-method input-test ((test event-converter-test))
  (let ((controller (make-instance 'event-converter 
                                   :mappable-events '(:jump)))
        (listener (make-instance 'dummy-listener
                                 :desired-events '(:jump))))
    (map-input controller (key-down-handler #\a #1='(:jump)))
    (subscribe controller listener)
    (handle-event controller '(:key-down :key #\b))
    (assert-equal nil (latest-event listener))
    (handle-event controller '(:key-down :key #\a))
    (assert-equal #1# (latest-event listener))))
