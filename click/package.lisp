; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(defpackage #:click
  (:use #:common-lisp)
  (:import-from #:resource-tree 
                #:invalid-node
                #:free
                #:with-nodes)
  (:export #:*root-container*
           #:*settings*
           #:*sprite-path*
           #:*sprite-tree*
           #:add-listener
           #:add-container
           #:activate-container
           #:deactivate-container
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
           #:remove-tag
           #:remove-igo
           #:render-text
           #:reset-settings
           #:run-display-system
           #:container
           #:container-manager
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
           #:active-containers
           #:background
           #:desired-events
           #:event
           #:height
           #:invalid-node
           #:listeners
           #:provided-events
           #:reason
           #:containers
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