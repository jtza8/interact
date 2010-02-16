; Copyright 2009 Jens Thiede. All rights reserved.
; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :click)

(defclass click-test (test-case)
  ())

(defmacro with-stand-in (file-name backup-name &body body)
  (let ((file-exists (gensym)))
    `(let ((,file-exists (cl-fad:file-exists-p ,file-name)))
       (when ,file-exists
         (cl-fad:copy-file ,file-name ,backup-name :overwrite t))
       (unwind-protect (progn ,@body)
         (when ,file-exists
           (cl-fad:copy-file ,backup-name ,file-name :overwrite t)
           (delete-file ,backup-name))))))

(defun forge-file (out-file data)
  (with-open-file (out-stream out-file
                   :direction :output :if-exists :supersede)
    (format out-stream data)))

(def-test-method test-merge-plists ((test click-test))
  (assert-equal '(:c 30 :a 10 :b 20)
		(merge-plists '(:a 10 :b 20) '(:a 72 :c 30))))

(def-test-method test-load-settings ((test click-test))
  (let ((clickrc-file (merge-pathnames ".clickrc" (user-homedir-pathname)))
        (backup-file #+unix #P"/tmp/za.jens.clickrc"
                     #+windows #P"c:\\za.jens.clickrc")
	theme-path)
    (with-stand-in clickrc-file backup-file
      (forge-file clickrc-file
                  "(:screen-size (1024 768))")
      (reset-settings)
      (setf theme-path (getf *settings* :theme-path))
      (assert-equal '(800 600) (getf *settings* :screen-size))
      (load-settings)
      (assert-equal '(1024 768) (getf *settings* :screen-size))
      (assert-equal theme-path (getf *settings* :theme-path))
      (reset-settings))))