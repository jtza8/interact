; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :click)

(defclass sprite-tree-test (test-case)
  ())

(defmethod initialize-instance :after ((test sprite-tree-test) &key)
  (load-sprite-path *test-sprite-path*))
