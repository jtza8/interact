; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :click)

(defclass sprite-sheet-test (test-case)
  ())

(def-test-method test-sheet-header ((test sprite-sheet-test))
  ; Mega-über-macro-simplification... ENGADGE!
  ; Overkill much? =P
  (flet ((assert-header (header message frame-width frame-height
                         frame-count fps looping)
           (flet ((make-msg (attribute)
                    (format nil "~a ~a" attribute message)))
             (macrolet ((expand-assert (name)
                          `(assert-equal ,name (getf header
                                                     ,(intern (symbol-name name)
                                                              'keyword)) 
                                         (make-msg ,(symbol-name name))))
                        (expand-asserts (&rest names)
                          `(progn ,@(loop for name in names
                                          collect `(expand-assert ,name)))))
               (expand-asserts frame-width frame-height 
                               frame-count fps looping)))))
   (il:with-images (test)
      (il:bind-image test)
      (il:tex-image 64 64 1 4 :rgba :unsigned-byte (cffi:null-pointer))
      (il:clear-image 0 0 0 0)
      (write-sheet-header 8 7 60 12 t)
      (assert-header (read-sheet-header) "pre-write assert" 8 7 60 12 t)
      (il:enable :file-overwrite)
      (il:save-image (merge-pathnames #p"header-test.png"
                                      *test-image-path*)))
    (il:with-images (test)
      (il:bind-image test)
      (il:load-image (merge-pathnames #p"header-test.png"
                                      *test-image-path*))
      (assert-header (read-sheet-header) "post-write assert" 8 7 60 12 t))))

(defun test-build-sprite-sheet-manually ()
  (build-sprite-sheet (merge-pathnames #p"sequence*.png"
                                       *test-image-sequence-path*)
                      30
                      :file-overwrite t
                      :sheet-file-name 
                      (merge-pathnames "sequence-test.ss.png"
                                       *test-image-sequence-path*)))

(defun test-load-sprite-sheet-manually ()
  (with-display-system ()
    (load-sprite-path *test-sprite-path*)
    (let* ((sprite (load-sprite-sheet 
                    (merge-pathnames "test-sheet.ss.png" *test-image-path*)))
           (igo (make-instance 'simple-igo :x 10 :y 10 :sprite sprite)))
      (add-root-igo igo :simple-igo))))

(def-test-method test-load-sprites ((test sprite-sheet-test))
  (let ((bogus-jpg-path (merge-pathnames "bogus.jpg" *test-image-path*))
        (test-image-path (merge-pathnames "src-image.png" *test-image-path*)))
    (assert-equal nil (load-sprite "bogus"))
    (assert-true (fad:file-exists-p bogus-jpg-path))
    (assert-equal nil (load-sprite bogus-jpg-path))
    (let ((sprite (load-sprite test-image-path)))
      (assert-true (typep sprite 'texture-sprite))
      (gl:delete-textures (list (texture sprite))))))