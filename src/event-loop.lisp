; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :interact)

(defun simple-top-level-event-handler (event)
  (case (event-type event)
    (:key-down
     (with-event-keys (key) event
       (case key
         (:escape t)
         (:F12 (toggle-fullscreen) nil))))))

(defmacro with-event-loop (top-level-event-handler &body body)
  (let ((event (gensym "SDL-EVENT-"))
        (quit-var (gensym "QUIT-"))
        (event-var (gensym "EVENT-")))
   `(let ((,event (sdl:new-event)))
      (unwind-protect
           (loop
              with ,quit-var
              until ,quit-var
              do (loop
                    until (= 0 (sdl-cffi::sdl-poll-event ,event))
                    do (progn
                         (when (eq (sdl:get-event-type ,event) :quit-event)
                           (setf ,quit-var t))
                         (let ((,event-var (parse-sdl-event ,event)))
                           ,(if top-level-event-handler
                                `(when (funcall ,top-level-event-handler
                                                ,event-var)
                                   (setf ,quit-var t)))
                           (send-event *root-container*
                                       ,event-var))))
              do (progn ,@body))
        (cffi:foreign-free ,event)))))