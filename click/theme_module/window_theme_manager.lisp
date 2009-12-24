; Copyright 2009 Jens Thiede. All rights reserved.
; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :click)

(defclass window-theme-manager ()
  ((theme-manager :initarg :theme-manager
                  :initform (error "Theme manager needed."))
   (shadows-path :initarg :shadows-path
                 :initform nil
                 :reader shadows-path)
   (title-bar-path :initarg :title-bar-path
                   :initform nil
                   :reader title-bar-path)))

(defmethod initialize-instance :after ((manager theme-manager) &key)
  ())
