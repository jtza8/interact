## About Click

Click is a 2D OpenGL graphics and interaction engine suitable for use
in games.

### Highlights

One of the main features of Click is its simple sprite management
system. Just specify where images are kept, and they are automatically
(or selectively) loaded as sprites into a sprite tree. Sprites are
subsequently accessible globally without polluting any package
name-space; simply call the `SPRITE-NODE` function to retrieve any node
from the tree, or use the `WITH-SPRITES` macro to automatically bind
them to suitable lexically-scoped variables.

Click also has a sprite-sheet system which allows for loss-less,
alpha-capable, single file animations with all relevant data such as
frame-rate and looping encoded. Sprite-sheets can be saved in any
loss-less single image format supported by the DevIL image library. As
such, they are also viewable in any picture viewer supporting your
selected file format.

The actual display is handled with widgets and screens, and is
currently in active development. Although it's already easy to use,
this part of the system is prone to huge amounts of change, but it
seems promising so far.

### Dependencies

* CFFI
* [CL-DevIL](https://github.com/sykopomp/cl-devil "Sykopomp's CL-DevIL")
* CL-FAD
* CL-OpenGL
* CL-PPCRE
* Lispbuilder-SDL
* xlUnit (optional, for unit tests)

## Installation

After installing all dependencies, manually install Click by copying
the source code to the appropriate directory and providing the ASDF
system with the required symbolic links.

__Note:__ To create symbolic links on any Unix system, use the following
command while in the appropriate ASDF systems directory:

`find ../path/to/click/source -name '*.asd' -exec ln -s '{}' \;`
