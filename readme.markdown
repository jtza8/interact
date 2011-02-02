## About Click

Click is what I call an __I__nteractive __G__raphics __S__ystem (an
"IGS"). Technically you could call it a very low-level GUI -- although
it's meant to fulfill a different need than a traditional GUI. Click
is intended to be used in games as a 2D graphics engine.

### Features

* A sprite management system
  * Seamless handling of many different kinds of sprites.
    * Currently supports:
      * Automatic loading of bitmap sprites (PNGs using RGB and RGBA)
    * Support planned for:
      * Non power-of-two sized bitmaps
      * Simple colour sprites
      * TTF Text sprites
      * Bitmap text sprites
      * Vector sprites
      * OpenGL shader sprites
* An easy to use rendering system
  * Up and running with two function calls.
  * Layers (comming soon).
  * An abstract "`widgets`" class.
  * Uses a higher-level approach to avoid dependencies on lower
    level code (e.g. uses it's own event system).
  * Is built on the following libraries:
    * cl-openGL (for OpenGL)
    * Lispbuilder-SDL (display and events)
    * cl-devIL (image handling)
    * cl-fad (additional file management)

## Installation

1. Use ASDF-Install or manually install the ASDF systems Click depends
   on (Dependencies stated in the features section.). Optionally
   install xlUnit if you'd like to run the unit tests.
2. Manually install click by copying the source code to the
   appropriate directory.
3. On any Unix system, use the following command while in an ASDF
   systems directory:
   <blockquote>
   `find /path/to/click/source -name '*.asd' -exec ln -s '{}' \;`
   </blockquote>
