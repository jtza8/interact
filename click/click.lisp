; Copyright 2009 Jens Thiede. All rights reserved.
; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :click)

(defparameter *settings* nil)
(defparameter *theme-path* nil)
(defparameter *window-manager* nil)
(defparameter *sprite-tree* nil)

(defun reset-settings ()
  "Sets <tt>&#42;SETTINGS&#42;</tt> to hard-coded defaults."
  (setf *settings* 
        (list 
         :theme-path (asdf:system-relative-pathname :click "default_theme")
         :screen-size '(800 600))))

(defun load-settings ()
  "Loads the general settings for the Click GUI which override the
hard-coded defaults. However, these settings are mearly the new
defaults which may stil be overridden by code using the Click
library. Settings are specifide in a _p-list_ format and are loaded
from a file named <tt>.clickrc</tt> in the user's home folder.

h3. Settable Variables

h4. <tt>:THEME-PATH</tt> 

Sets the default theme for the Click GUI. By default the theme path is
relative to the output of the function:
<tt>(asdf:system-relative-path-name :click \"default_theme\")</tt>.

h4. <tt>:SCREEN-SIZE</tt>

Sets the default screen size as a two item list. If not specified, the
hard-coded default is <tt>(800 600)</tt>. For convenience, here are
some other commonly used screen sizes:
* <tt>(1024 768)</tt>
* <tt>(1280 1024)</tt>
* <tt>(1680 1050)</tt>"
  (let ((clickrc-file
         (merge-pathnames ".clickrc" (user-homedir-pathname))))
    (unless (cl-fad:file-exists-p clickrc-file)
      (return-from load-settings))
    (with-open-file (settings clickrc-file)
      (setf *settings* (read settings)))))

(defun init-click (&key (settings *settings*))
  "Initialises Click. Sets the <tt>&#42;WINDOW-MANAGER&#42;</tt>
global variable to a new instance of <tt>WINDOW-MANAGER</tt>. Also
sets the <tt>&#42;SPRITE-TREE&#42;</tt> global variable via the
function <tt>MAKE-SPRITE-TREE</tt>."
  (setf *window-manager* (make-instance 'window-manager)
        *sprite-tree* (make-sprite-tree (getf settings :theme-path))))

(reset-settings)
