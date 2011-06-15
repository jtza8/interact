; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :click-examples)

(defclass warp-shader (shader)
  ((warp-count :initarg :warp-count
               :initform 1
               :reader warp-count)))

(defmethod initialize-instance :after ((shader warp-shader) &key)
  (update-source-code shader))

(defmethod update-source-code ((shader warp-shader))
  (with-slots (warp-count) shader
    (setf (source-code shader)
          (format nil
                  "uniform vec2 clk_scale;
                   uniform vec2 clk_offset;
                   uniform sampler2D clk_scene;
                   uniform vec3 warps[~d];

                   vec2 clk_toClickCoords(in vec2 coords)
                   {
                     vec2 click_coords = coords*clk_scale+clk_offset;
                     return click_coords;
                   }

                   vec2 clk_toTexCoords(in vec2 coords)
                   {
                     return (coords-clk_offset)/clk_scale;
                   }
                   
                   vec2 warpFunc(in vec3 warp)
                   {
                     vec2 coords = clk_toClickCoords(gl_TexCoord[0].xy);
                     vec2 offset = coords - warp.xy;
                     float z = sqrt(warp.z*warp.z-
                                    offset.x*offset.x-
                                    offset.y*offset.y);
                     if (sqrt(dot(offset, offset)) < warp.z) {
                       coords -= z*offset/50.0;
                     }
                     return clk_toTexCoords(coords);
                   }
                   
                   void main()
                   {
                     vec2 coords = warpFunc(warps[0]);
                     for (int i=1; i < ~d; i++)
                     {
                       coords += warpFunc(warps[i]);
                     }
                     coords /= ~f;
                     gl_FragColor = gl_Color * texture2D(clk_scene, coords);
                   }"
                  warp-count warp-count warp-count))))