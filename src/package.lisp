; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(defpackage #:interact
  (:use #:common-lisp #:resource-tree #:events #:watch)
  (:import-from #:meta-package #:internal #:auto-export)
  (:export #:draw-set-sprite))

(il:init)
(ilu:init)
