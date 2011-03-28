; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :click)

(defclass peripheral-controller (listenable)
  ((predicates :initform (make-array 20 :adjustable t 
                                     :fill-pointer 0
                                     :element-type 'function))))

(defmethod initialize-instance :after ((controller peripheral-controller)
                                       &key mappable-events)
  (apply #'provide-events controller mappable-events))

(defmethod map-input ((controller peripheral-controller) event-predicate)
  (with-slots (predicates) controller
    (vector-push-extend event-predicate predicates)))

(defmethod handle-event ((controller peripheral-controller) event)
  (with-slots (predicates) controller
    (loop for predicate across predicates
          for result = (funcall predicate event)
          unless (null result)
            do (send-event controller result)
            and return nil)))

(defun key-up-handler (key event)
  (lambda (input)
    (when (and (eq (event-type input) :key-down)
               (string= (getf (event-data input) :key) key))
      event)))
