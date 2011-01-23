; Copyright 2009 Jens Thiede. All rights reserved.
; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :click)

(init-screen-system)
(sdl:quit-video)

(defclass sprite-management-test (test-case)
  ())

(defmethod set-up ((test sprite-management-test))
  (set-test-sprite-path))

(def-test-method test-make-sprite-tree ((test sprite-management-test))
  (let ((tree  *sprite-tree*))
    (assert-true (not (null (getf tree :1))))
    (assert-true (not (null (getf (getf tree :1) :2))))
    (assert-true 
     (not (null (getf (getf (getf tree :1) :2) :b))))))

(def-test-method test-fetch-sprite-node ((test sprite-management-test))
  (assert-true (not (null (fetch-sprite-node '(:1 :2 :b)))))
  (assert-condition 'invalid-sprite-node
                    (fetch-sprite-node '(:1 :2 :blah)))
  (let ((tree (fetch-sprite-node '(:1 :2))))
    (assert-true (typep (fetch-sprite-node '(:b) tree) 'texture-sprite)
                 (format nil "~a~%~s"
                         "FETCH-SPRITE-NODE failed with the following tree:"
                         tree))))

(def-test-method test-with-sprites ((test sprite-management-test))
  (assert-condition
   'invalid-sprite-node
   (with-sprites (:1 :no-such-node) (blah)
     (declare (ignore blah))))
  (assert-condition
   'invalid-sprite-node
   (with-sprites (:1 :2) (blah)
     (declare (ignore blah))))
  (with-sprites (:1 :2) (b)
    (assert-eql b (fetch-sprite-node '(:1 :2 :b))))
  (with-sprites () (target)
    (assert-eql target (fetch-sprite-node '(:target)))))
