; Copyright 2010 Jens Thiede. All rights reserved.
; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :click)

; This is a collection of miscellaneous simple functions.

(defun flatten-tree (tree)
  (loop for item in tree
     if (consp item) append (flatten-tree item)
     else collect item))