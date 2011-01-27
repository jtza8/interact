; Copyright 2011 Jens Thiede. All rights reserved.
; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(defpackage #:click
  (:use #:common-lisp)
  (:export #:*screen-manager*
           #:*settings*
           #:*sprite-path*
           #:*sprite-tree*
           #:add-listener
           #:add-screen
           #:sm-add-screen
           #:activate-screen
           #:sm-activate-sceen
           #:deactivate-screen
           #:sm-deactivate-screen
           #:add-widget
           #:desire-events
           #:draw
           #:draw-at
           #:draw-tiled
           #:event-data
           #:event-type
           #:fetch-sprite-node
           #:init-click
           #:init-screen-system
           #:invalid-event
           #:invalid-event-type
           #:invalid-sprite-node
           #:listenable
           #:listener
           #:load-settings
           #:provide-events
           #:rectangle
           #:remove-listener
           #:remove-screen
           #:sm-remove-screen
           #:remove-tag
           #:remove-widget
           #:render-text
           #:reset-settings
           #:run-screen-system
           #:screen
           #:screen-manager
           #:send-event
           #:sprite
           #:tag-error
           #:tag-of
           #:tag-widget
           #:texture-sprite
           #:theme
           #:translate
           #:undesire-events
           #:undo-translate
           #:widget
           #:widget-of
           #:with-event-keys
           #:with-sprites
           #:with-translate
           #:within

           ; Accessors:
           #:active-screens
           #:background
           #:desired-events
           #:event
           #:height
           #:invalid-node
           #:listeners
           #:provided-events
           #:reason
           #:screens
           #:texture
           #:widgets
           #:width
           #:windows
           #:x
           #:y))
