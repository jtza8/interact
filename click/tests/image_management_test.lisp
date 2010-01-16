; Copyright 2009 Jens Thiede. All rights reserved.
; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :click)

(defparameter *test-theme-path* (asdf:system-relative-pathname
                                 :click "tests/test_theme"))

(defclass image-management-test (test-case)
  ())

(defmethod initialize-instance :after ((test image-management-test) &key)
  (sdl:init-sdl)
  (sdl:with-init (sdl:sdl-init-video)
    (setf *theme-image-tree* (make-image-tree *test-theme-path*))))

(defmethod set-up ((test image-management-test))
  (sdl:init-video))

(defmethod tear-down ((test image-management-test))
  (sdl:quit-video))

(def-test-method test-make-image-tree ((test image-management-test))
  (let ((tree (make-image-tree *test-theme-path*)))
    (assert-true (not (null (getf tree :window))))
    (assert-true (not (null (getf (getf tree :window) :shadow))))
    (assert-true 
     (not (null (getf (getf (getf tree :window) :shadow) :left-01))))))

(def-test-method test-fetch-image-node ((test image-management-test))
  (assert-true (not (null (fetch-image-node :window :shadow :left-01))))
  (assert-condition 'invalid-image-node
                    (fetch-image-node :window :shadow :blah)))

(def-test-method test-fetch-image-node ((test image-management-test))
  (let ((tree (fetch-image-node :window :shadow)))
    (assert-condition 'invalid-image-node
                      (fetch-from-image-node tree :blah :foo))
    (assert-equal (find-class 'image)
                  (class-of (fetch-from-image-node tree
                                                   :left-01)))))

(def-test-method test-with-node-images ((test image-management-test))
  (assert-condition
   'invalid-image-node
   (with-node-images (:window :no-such-node) (blah)
     (declare (ignore blah))))
  (assert-condition
   'invalid-image-node
   (with-node-images (:window :shadow) (blah)
     (declare (ignore blah)))))