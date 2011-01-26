## About Click

Click is an "IGS" as I call it. Which means, you may technically call
it a GUI -- although it's meant to fulfill a different need than a
traditional GUI. Click can (and is meant to) be used in games as a 2D
graphics engine.

### Features

* A sprite management system
  * Seamless handling of many different kinds of sprites.
    * Currently supporting:
      * Bitmap sprites (PNGs using RGB and RGBA)
    * Support planned for:
      * Simple colour sprites
      * TTF Text sprites
      * Bitmap text sprites
      * Vector sprites
      * OpenGL shader sprites
* An easy to use rendering system
  * Up and running with two function calls.
  * An abstract class for new "widgets" (for event and sprite
    handling).
  * Uses a higher-level approach to avoid dependencies on lower
    level code.
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
   `find /path/to/click/source -name '*.lisp' -exec ln -s '{}' \;`
   </blockquote>

4. For now, see the tests in the click source for examples. Enjoy (I
   hope) and tell me what you think over at GitHub
   (github.com/jtza8/Click).