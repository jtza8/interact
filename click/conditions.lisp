; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :click)

(define-condition index-error (error)
  ((index :initarg :index
          :initform (error "must specify index")))
  (:report (lambda (condition stream)
             (with-slots (index) condition
               (format stream "invalid index \"~a\"" index)))))

(define-condition bitmap-char-error (index-error)
  ((index :initarg :character))
  (:report (lambda (condition stream)
             (with-slots (index) condition
               (format stream "invalid bitmap character \"~a\"" index)))))

(define-condition coordinate-error (index-error)
  ((index :initarg :component))
  (:report (lambda (condition stream)
             (with-slots (index) condition
               (format stream "coordinate is of a lesser dimension. ~
                               Cannot access \"~a\""
                       index)))))

(internal reason-reporter)
(defmacro reason-reporter (condition stream reason-slot (&rest slots)
                           &body cases)
  `(with-slots (,reason-slot ,@slots) ,condition
     (format ,stream
             (ecase ,reason-slot
               ,@(loop for case in cases
                       if (> (length case) 2)
                         collect `(,(car case)
                                    (format nil ,@(subseq case 1)))
                       else
                         collect case)))))

(define-condition igo-tag-error (error)
  ((fault :initarg :fault
          :initform (error "must specify fault"))
   (tag :initarg :tag
        :initform nil)
   (igo :initarg :igo
        :initform nil))
  (:report (lambda (condition stream)
             (reason-reporter condition stream fault (tag igo)
               (:duplicate-tag "Tag ~s for igo ~s must be unique" tag igo)
               (:double-tag "IGO ~s already has tag ~s" igo tag)
               (:tag-not-found "Couldn't find tag ~s" tag)))))

(internal pixel-index-error)
(define-condition pixel-index-error (error)
  ((message :initarg :message
            :initform (error "must specify message")))
  (:report (lambda (condition stream)
             (with-slots (message) condition
               (princ message stream)))))

(internal check-pixel-index)
(defmacro check-pixel-index (condition message &optional (places '()))
  `(assert ,condition ,places 'pixel-index-error 
           :message ,message))

(define-condition image-error (error)
  ()
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (format stream "an image has incorrect properties"))))

(macrolet ((define-property-error (property unsupported-message)
             (flet ((format-symbol (control-string &rest format-arguments)
                      (intern (string-upcase (apply #'format nil control-string
                                                    format-arguments))
                              (loop for (key value) on format-arguments
                                 when (eq key :package) return value
                                 finally (return 'click)))))
               (let ((condition-name (format-symbol "image-~a-error" property))
                     (expected-property (format-symbol "expected-~a" property))
                     (expected-initarg (format-symbol "expected-~a" property
                                                      :package 'keyword))
                     (actual-property (format-symbol "actual-~a" property))
                     (actual-initarg (format-symbol "actual-~a" property
                                                    :package 'keyword))
                     (actual-err-string (format nil "must specify actual-~(~a~)"
                                                property)))
                 `(define-condition ,condition-name (image-error)
                    ((,expected-property :initform nil
                                         :initarg ,expected-initarg
                                         :reader ,expected-property)
                     (,actual-property :initform (error 
                                                        ,actual-err-string)
                                       :initarg ,actual-initarg
                                       :reader ,actual-property)
                     (message :initform nil
                               :initarg :message
                               :reader message))
                    (:report (lambda (condition stream)
                               (with-slots (,expected-property ,actual-property
                                            message)
                                   condition
                                 (if (null ,expected-property)
                                     (format stream ,unsupported-message
                                             ,actual-property)
                                     (format stream
                                             "expecting ~(~a~) ~s but got ~s"
                                             ',property
                                             ,expected-property
                                             ,actual-property))
                                 (unless (null message)
                                   (format stream ". ~@(~a~)" message)))))))))
           (define-property-errors (&rest arguments)
             `(progn ,@(loop for (property unsupported-message) on arguments 
                                 by #'cddr
                             collect `(define-property-error
                                          ,property
                                          ,unsupported-message)))))
  (define-property-errors
      format "unsupported format ~s. Supported formats: :RGB :RGBA :BGR :BGRA ~
              :LUMINANCE :LUMINANCE-ALPHA"
      type "unsupported type ~s. Currently only 8-bit unsigned integers are ~
            supported."
      dimensions "unsupported dimensions ~s"))

(define-condition file-existance-error (file-error)
  ()
  (:report (lambda (condition stream)
             (format stream "file \"~a\" does not exist"
                     (file-error-pathname condition)))))

(defmacro check-file-existance (pathname)
  `(assert (fad:file-exists-p ,pathname)
           ,(if (symbolp pathname) (list pathname) nil)
           'file-existance-error
           :pathname ,pathname))

(define-condition shader-error (error)
  ((reason :initarg :reason
           :reader reason)
   (shader :initarg :shader
           :reader shader))
  (:report (lambda (condition stream)
             (reason-reporter condition stream reason (shader)
               (:creation "Couldn't create a new shader object.")
               (:compilation "Couldn't compile shader code:~%~a"
                             (gl:get-shader-info-log (id shader)))))))

(define-condition filter-error (error)
  ((reason :initarg :reason
           :reader reason)
   (filter :initarg :filter
           :reader filter)
   (shader :initarg :shader
           :reader shader))
  (:report (lambda (condition stream)
             (reason-reporter condition stream reason (filter shader)
               (:creation "Couldn't create a new shader program.")
               (:uncompiled-shader "Shader ~s wasn't compiled." shader)
               (:linkage "Couldn't link shader program:~%~a"
                         (gl:get-program-info-log (id filter)))))))

(define-condition camera-error (error)
  ((reason :initarg :reason
           :reader reason)
   (camera :initarg :camera
           :reader camera))
  (:report (lambda (condition stream)
             (reason-reporter condition stream reason (camera)
               (:incomplete-framebuffer
                "Framebuffer incomplete in ~s." camera)
               (:listening-without-root
                "Can not add ~s as listener without root being set." camera)))))
