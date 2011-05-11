; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :click)

(define-condition igo-tag-error (error)
  ((fault :initarg :fault
          :initform (error "must specify fault"))
   (tag :initarg :tag
        :initform nil)
   (igo :initarg :igo
        :initform nil))
  (:report (lambda (condition stream)
             (with-slots (fault tag igo) condition
               (case fault
                 (:duplicate-tag
                  (format stream "Tag ~s for igo ~s must be unique"
                          tag igo))
                 (:double-tag
                  (format stream "IGO ~s already has tag ~s"
                          igo tag))
                 (:tag-not-found (format stream "Couldn't find tag ~s" tag))
                 (otherwise (format stream "Unknown fault: ~s" fault)))))))

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
  (:report (lambda (condition stream) "an image has incorrect properties")))

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
                     (addendum :initform nil
                               :initarg :addendum
                               :reader addendum))
                    (:report (lambda (condition stream)
                               (with-slots (,expected-property ,actual-property
                                            addendum)
                                   condition
                                 (if (null ,expected-property)
                                     (format stream ,unsupported-message
                                             ,actual-property)
                                     (format stream
                                             "expecting ~(~a~) ~s but got ~s"
                                             ',property
                                             ,expected-property
                                             ,actual-property))
                                 (unless (null addendum)
                                   (format stream ". ~@(~a~)" addendum)))))))))
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

;; (define-condition file-extension-error (file-error)
;;   ()
;;   (:report (lambda (condition stream)
;;              (format stream "file extension \".~a\" not supported"
;;                      (pathname-type (file-error-pathname condition))))))
