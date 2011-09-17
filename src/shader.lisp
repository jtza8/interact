; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :interact)

(internal *shaders*)
(defparameter *shaders* '())

(defclass shader ()
  ((source-code :initform ""
                :initarg :source-code
                :reader source-code)
   (compiled-p :initform nil
               :reader compiled-p)
   (shader-id :reader id)))

(defmethod initialize-instance :after ((shader shader) &key)
  (push shader *shaders*)
  (with-slots (shader-id source-code) shader
    (with-try-again-restart ()
      (setf shader-id (gl:create-shader :fragment-shader))
      (when (zerop shader-id)
        (error 'shader-error :reason :creation)))
    (with-simple-restart (continue "Skip shader compilation for now.")
      (unless (string= source-code "")
        (compile-shader shader)))))

(defmethod compile-shader ((shader shader))
  (with-slots (shader-id source-code compiled-p) shader
    (gl:shader-source shader-id source-code)
    (gl:compile-shader shader-id)
    (unless (gl:get-shader shader-id :compile-status)
      (setf compiled-p nil)
      (error 'shader-error :reason :compilation :shader shader))
    (setf compiled-p t)))

(defmethod load-shader ((shader shader) file-name)
  (with-slots (shader-id source-code) shader
    (with-try-again-restart ("Re-read and compile shader source code.")
      (with-open-file (stream file-name :if-does-not-exist :error)
        (setf source-code (make-array (file-length stream)
                                      :element-type 'character))
        (read-sequence source-code stream)
        (compile-shader shader-id)))))

(defmethod (setf source-code) (value (shader shader))
  (with-slots (source-code) shader
    (setf source-code value)
    (compile-shader shader)))

(defmethod free ((shader shader))
  (with-slots (shader-id) shader
    (gl:delete-shader shader-id)))

(internal delete-all-shaders)
(defun delete-all-shaders ()
  (map nil #'free *shaders*)
  (setf *shaders* '()))