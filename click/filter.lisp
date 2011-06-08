; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :click)

(defparameter *filters* '())

(defclass filter ()
  ((shaders :initform '())
   (program-id :reader id)))

(define-instance-maker filter)

(defmethod initialize-instance :after ((filter filter) &key)
  (with-slots (program-id) filter
    (setf program-id (gl:create-program))
    (when (zerop program-id)
      (error 'filter-error :reason :creation))
    (push filter *filters*)))

(defmethod add-shader ((filter filter) (shader shader))
  (with-slots (shaders program-id) filter
    (assert (compiled-p shader) ()
            'filter-error :reason :uncompiled-shader :shader shader)
    (gl:attach-shader program-id (id shader))
    (pushnew shader shaders)))

(defmethod remove-shader ((filter filter) (shader shader))
  (with-slots (shaders program-id) filter
    (gl:detach-shader program-id (id shader))
    (setf shaders (delete shader shaders))))

(defmethod link-filter ((filter filter))
  (with-slots (program-id) filter
    (with-try-again-restart ("Try linking program.")
      (gl:link-program program-id)
      (unless (gl:get-program program-id :link-status)
        (error 'filter-error :reason :linkage :filter filter)))))

(defmethod free ((filter filter))
  (with-slots (program-id) filter
    (gl:delete-program program-id)))

(defmethod activate ((filter filter))
  (with-slots (program-id) filter
    (gl:use-program program-id)))

(defmethod deactivate ((filter filter))
  (declare (ignore filter))
  (gl:use-program 0))

(defmacro with-active-filter (filter &body body)
  `(unwind-protect (progn (activate ,filter) ,@body)
     (deactivate ,filter)))

(internal delete-all-filters)
(defun delete-all-filters ()
  (map nil #'free *filters*)
  (setf *filters* '()))