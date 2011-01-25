; Copyright 2011 Jens Thiede. All rights reserved.
; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(defpackage #:click-gui
  (:use #:common-lisp)
  (:import-from #:click
                ; Accessors:
                #:active-screens
                #:background
                #:desired-events
                #:event
                #:height
                #:invalid-node
                #:listeners
                #:provided-events
                #:reason
                #:screens
                #:texture
                #:widgets
                #:width
                #:windows
                #:x
                #:y))