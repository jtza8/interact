; Copyright 2009 Jens Thiede. All rights reserved.
; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :click)

(init-screen-system)
(sdl:quit-video)

(defparameter *test-sprites-path* (asdf:system-relative-pathname
                                 :click "tests/test_sprites"))

(defclass sprite-management-test (test-case)
  ())

(defmethod set-up ((test sprite-management-test))
  (init-click :settings (list :theme-path *test-sprites-path*)))

(def-test-method test-make-sprite-tree ((test sprite-management-test))
  (let ((tree (make-sprite-tree *test-sprites-path*)))
    (assert-true (not (null (getf tree :window))))
    (assert-true (not (null (getf (getf tree :window) :shadow))))
    (assert-true 
     (not (null (getf (getf (getf tree :window) :shadow) :left-01))))))

(def-test-method test-fetch-sprite-node ((test sprite-management-test))
  (assert-true (not (null (fetch-sprite-node '(:window :shadow :left-01)))))
  (assert-condition 'invalid-sprite-node
                    (fetch-sprite-node '(:window :shadow :blah)))
  (let ((tree (fetch-sprite-node '(:window :shadow))))
    (assert-true (typep (fetch-sprite-node '(:left-01) tree) 'texture-sprite)
                 (format nil "~a~%~s"
                         "FETCH-SPRITE-NODE failed with the following tree:"
                         tree))))

(def-test-method test-with-node-sprites ((test sprite-management-test))
  (make-sprite-tree *test-sprites-path*)
  (assert-condition
   'invalid-sprite-node
   (with-node-sprites (:window :no-such-node) (blah)
     (declare (ignore blah))))
  (assert-condition
   'invalid-sprite-node
   (with-node-sprites (:window :shadow) (blah)
     (declare (ignore blah))))
  (with-node-sprites (:window :shadow) (left-01)
    (let ((left (fetch-sprite-node '(:window :shadow :left-01))))
      (assert-eql left left-01))))