; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(defpackage #:click
  (:use #:common-lisp)
  (:import-from #:resource-tree 
                #:invalid-node
                #:free
                #:with-nodes)
  (:export 
   ; Variables
   #:*clipping-depth*
   #:*global-stopwatch*
   #:*root-container*
   #:*sprite-tree*

   ; Functions
   #:add-root-igo
   #:add-root-listener
   #:build-sprite-sheet
   #:event-data
   #:event-type
   #:igo-of-root
   #:image-to-sprite
   #:list-image-file-sequence
   #:load-image-sprite
   #:load-sprite
   #:load-sprite-path
   #:load-sprite-sheet
   #:prepare-click
   #:quit-display-system
   #:read-sheet-header
   #:remove-root-igo
   #:remove-root-listener
   #:root-remove-tag
   #:root-tag-igo
   #:root-tag-of
   #:run-display-system
   #:set-up-root-container
   #:set-up-root-container
   #:sprite-node
   #:start-display-system
   #:write-sheet-header

   ; Methods
   #:absolute-pos
   #:absolute-x
   #:absolute-y
   #:add-igo
   #:add-listener
   #:desire-events
   #:diverge
   #:draw
   #:draw-sprite
   #:draw-background
   #:handle-event
   #:igo-of
   #:initialize-instance
   #:lap
   #:map-input
   #:provide-events
   #:remove-igo
   #:remove-listener
   #:remove-tag
   #:reset
   #:select-handler
   #:send-event
   #:start
   #:stop
   #:tag-igo
   #:tag-of
   #:undesire-events
   #:within

   ; Accessors
   #:addendum
   #:background
   #:clipping
   #:color
   #:desired-events
   #:event
   #:fps
   #:height
   #:igos
   #:listeners
   #:parent
   #:pivot-x
   #:pivot-y
   #:provided-events
   #:reason
   #:texture
   #:width
   #:x
   #:y

   ; Macros
   #:with-display-system
   #:with-event-keys
   #:with-image-sequence

   ; Classes
   #:animation-sprite
   #:color-sprite
   #:container
   #:event-assistant
   #:event-converter
   #:igo
   #:listenable
   #:listener
   #:sprite
   #:stopwatch
   #:texture-sprite

   ; Conditions
   #:file-format-error
   #:image-error
   #:invalid-event
   #:invalid-event-type
   #:pixel-index-error
   #:tag-error))

(il:init)
(ilu:init)