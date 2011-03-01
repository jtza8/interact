; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :click)

(defclass sprite-management-test (test-case)
  ())

(defmethod initialize-instance :after ((test sprite-management-test) &key)
  (init-click *test-sprite-path*))

(def-test-method test-make-node-keyword ((test sprite-management-test))
  (assert-equal :bar (make-node-keyword #p"/one/two/bar/"))
  (assert-equal :foo (make-node-keyword #p"/one/two/bar/foo"))
  (assert-equal :baz (make-node-keyword #p"/one/two/bar/baz.ss.png")))

(def-test-method test-make-sprite-tree ((test sprite-management-test))
  (let ((tree  *sprite-tree*))
    (assert-true (not (null (getf tree :1))))
    (assert-true (not (null (getf (getf tree :1) :2))))
    (assert-true 
     (not (null (getf (getf (getf tree :1) :2) :b))))))

(def-test-method test-sprite-node-from ((test sprite-management-test))
  (assert-true (not (null (sprite-node :1 :2 :b))))
  (assert-condition 'invalid-sprite-node
                    (sprite-node :1 :2 :blah))
  (let ((tree (sprite-node :1 :2)))
    (assert-true (typep (sprite-node-from tree :b) 'texture-sprite)
                 (format nil "~a~%~s"
                         "SPRITE-NODE-FROM failed with the following tree:"
                         tree))))

(def-test-method test-with-sprites ((test sprite-management-test))
  (assert-condition
   'invalid-sprite-node
   (with-sprites (blah) (sprite-node :1 :no-such-node)
     (declare (ignore blah))))
  (assert-condition
   'invalid-sprite-node
   (with-sprites (blah) (sprite-node :1 :2)
     (declare (ignore blah))))
  (with-sprites (b) (sprite-node :1 :2)
    (assert-eql b (sprite-node :1 :2 :b)))
  (with-sprites (target) (sprite-node)
    (assert-eql target (sprite-node :target))))
