; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :click)

(internal *click-shader*)
(defparameter *click-shader* nil)

(defclass click-shader (shader)
  ())

(defmethod initialize-instance :after ((shader click-shader) &key)
  (setf (source-code shader)
        "uniform vec2 clk_scale;
         uniform vec2 clk_offset;

         vec2 clk_toClickCoords(in vec2 coords)
         {
           vec2 click_coords = coords*clk_scale+clk_offset;
//           click_coords.x = float(int(click_coords.x));
//           click_coords.y = float(int(click_coords.y));
           return click_coords;
         }

         vec2 clk_toTexCoords(in vec2 coords)
         {
           return (coords-clk_offset)/clk_scale;
         }"))