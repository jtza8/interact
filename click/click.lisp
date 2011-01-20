; Copyright 2009 Jens Thiede. All rights reserved.
; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :click)

(defparameter *settings* nil)
(defparameter *theme-path* nil)
(defparameter *screen-manager* nil)
(defparameter *sprite-tree* nil)

(defun reset-settings ()
  "Sets `*SETTINGS*` to hard-coded defaults."
  (setf *settings* 
        (list 
         :theme-path (asdf:system-relative-pathname :click "default_theme")
         :screen-size '(800 600))))

(defun merge-plists (dominant subserviant)
  "Merges p-lists. A new p-list is returned after collecting all the
pairs in `dominant`, as well as pairs in `subserviant` which have
unique keys (i.e. not used in `dominant`)."
  (let ((default (gensym "DEFAULT"))
	(merged (copy-seq dominant)))
    (loop
       for (key value) on subserviant by #'cddr
       when (eq default (getf merged key default)) do
	 (setf (getf merged key) value)
       finally
	 (return merged))))

(defun load-settings ()
  "Loads the general settings for the Click GUI which override the
hard-coded defaults. However might still be overridden by code using
the Click library. Settings are specifide in a _p-list_ which is
loaded from a file named `.clickrc` in the user's home folder.

### Settable Variables

#### `:THEME-PATH` 

Sets the default theme for the Click GUI. By default the theme path is
relative to the return value of the function:
`(asdf:system-relative-path-name :click \"default_theme\")`.

#### `:SCREEN-SIZE`

Sets the default screen size as a two item list. If not specified, the
hard-coded default is `(800 600)`. For convenience, here are
some other commonly used screen sizes:

* `(1024 768)`
* `(1280 1024)`
* `(1680 1050)`"
  (let ((clickrc-file
         (merge-pathnames ".clickrc" (user-homedir-pathname))))
    (unless (cl-fad:file-exists-p clickrc-file)
      (return-from load-settings))
    (with-open-file (settings clickrc-file)
      (setf *settings* (merge-plists (read settings) *settings*)))))

(defun init-click (&key (settings *settings*))
  "Initialises Click. Sets the `*WINDOW-MANAGER*`
global variable to a new instance of `WINDOW-MANAGER`. Also
sets the `*SPRITE-TREE*` global variable via the
function `MAKE-SPRITE-TREE`."
  (il:init)
  (setf *screen-manager* (make-instance 'screen-manager)
        *sprite-tree* (make-sprite-tree (getf settings :theme-path))))

(reset-settings)
