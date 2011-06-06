; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :click)

(internal *shaders*)
(defparameter *shaders* '())

(defclass shader ()
  ((source-code :initform ""
                :initarg :source-code
                :accessor source-code)
   shader-id))

(define-instance-maker shader)

(defmethod initialize-instance :after ((shader shader) &key)
  (push shader *shaders*)
  (with-slots (shader-id) shader
    (with-try-again-restart ()
      (setf shader-id (gl:create-shader :fragment-shader))
      (when (zerop shader-id)
        (error 'shader-error :reason :creation)))))

(defmethod compile-shader ((shader shader))
  (with-slots (shader-id source-code) shader
    (gl:shader-source shader-id source-code)
    (gl:compile-shader shader-id)
    (unless (gl:get-shader shader-id :compile-status)
      (error 'shader-error :reason :compilation :shader shader-id))))

(defmethod load-shader ((shader shader) file-name)
  (with-slots (shader-id source-code) shader
    (with-try-again-restart ("Re-read and compile shader source code.")
      (with-open-file (stream file-name :if-does-not-exist :error)
        (setf source-code (make-array (file-length stream)
                                      :element-type 'character))
        (read-sequence source-code stream)
        (compile-shader shader-id)))))

(defmethod free ((shader shader))
  (with-slots (shader-id) shader
    (gl:delete-shader shader-id)))

(defun delete-all-shaders ()
  (dolist (shader *shaders*)
    (free shader))
  (setf *shaders* '()))