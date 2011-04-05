; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(defpackage #:click
  (:use #:common-lisp)
  (:import-from #:resource-tree 
                #:invalid-node
                #:free
                #:with-nodes)
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
           #:add-igo
           #:desire-events
           #:draw
           #:draw-at
           #:event-data
           #:event-type
           #:sprite-node
           #:sprite-node-from
           #:init-click
           #:init-display-system
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
           #:remove-igo
           #:render-text
           #:reset-settings
           #:run-display-system
           #:screen
           #:screen-manager
           #:send-event
           #:sprite
           #:tag-error
           #:tag-of
           #:tag-igo
           #:texture-sprite
           #:theme
           #:translate
           #:undesire-events
           #:undo-translate
           #:igo
           #:igo-of
           #:with-event-keys
           #:with-nodes
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
           #:igos
           #:width
           #:windows
           #:absolute-pos
           #:absolute-x
           #:absolute-y
           #:x
           #:y))

(il:init)
(ilu:init)