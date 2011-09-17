; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(defpackage #:interact
  (:use #:common-lisp #:resource-tree #:events #:watch)
  (:import-from #:meta-package #:internal #:auto-export)
  (:export #:draw-set-sprite))

(in-package #:interact)
(defparameter *quit-state* nil)
(defparameter *global-watch* (make-instance 'watch))
(defparameter *frame-watch* (make-instance 'watch :parent *global-watch*))
(defparameter *iter-watch* (make-instance 'watch :parent *global-watch*))

(defclass screen-settings ()
  ((width :initform 800
          :accessor width)
   (height :initform 600
           :accessor height)
   (full-screen :initform nil
                :accessor full-screen)
   (title :initform "Lisp"
          :accessor title)
   (clear-colour :initform '(0.5 0.5 0.5 1.0)
                 :accessor clear-colour)))
(defparameter *screen* (make-instance 'screen-settings))

(il:init)
(ilu:init)
