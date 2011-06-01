; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :click)

(defclass bitmap-font-sprite-test (test-case)
  ((font-sprite)))

(defmethod set-up ((test bitmap-font-sprite-test))
  (with-slots (font-sprite) test
    (setf font-sprite (load-font-sheet
                         (asdf:system-relative-pathname
                          :click-tests
                          #p"test-fonts/8x16.fnt.png")))))

(defmethod tear-down ((test bitmap-font-sprite-test))
  (with-slots (font-sprite) test
    (free font-sprite)))

(defun test-bitmap-font-manually ()
  (let (font-sprite font-igo)
    (with-display-system (screen-bg-colour '(0.5 0.5 0.5 0))
      (setf font-sprite (load-font-sheet
                         (asdf:system-relative-pathname
                          :click-tests
                          #p"test-fonts/8x16.fnt.png"))
            font-igo (make-instance 'simple-igo :sprite font-sprite
                                    :x 100 :y 100))
      (add-root-igo font-igo)
      (setf (rotation font-igo) 0
            (text font-sprite)
            "The quick-brown fox jumps over the lazy dog.~"))
    (free font-sprite)))

(def-test-method test-fetch-glyph ((test bitmap-font-sprite-test))
  (with-slots (font-sprite) test
    (assert-true (typep (fetch-glyph font-sprite #\!) 'sprite))
    (assert-true (typep (fetch-glyph font-sprite #\~) 'sprite))
    (assert-condition 'bitmap-char-error (fetch-glyph font-sprite #\Newline))))