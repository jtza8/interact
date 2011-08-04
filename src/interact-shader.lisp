; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :interact)

(internal *interact-shader*)
(defparameter *interact-shader* nil)

(defclass interact-shader (shader)
  ())

(defmethod initialize-instance :after ((shader interact-shader) &key)
  (setf (source-code shader)
        "uniform vec2 clk_scale;
         uniform vec2 clk_offset;

         vec2 clk_toClickCoords(in vec2 coords)
         {
           vec2 interact_coords = coords*clk_scale+clk_offset;
//           interact_coords.x = float(int(interact_coords.x));
//           interact_coords.y = float(int(interact_coords.y));
           return interact_coords;
         }

         vec2 clk_toTexCoords(in vec2 coords)
         {
           return (coords-clk_offset)/clk_scale;
         }"))