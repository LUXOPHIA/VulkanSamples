unit cube_data;

(*
 * Vulkan Samples
 *
 * Copyright (C) 2015-2016 Valve Corporation
 * Copyright (C) 2015-2016 LunarG, Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *)

//--------------------------------------------------------------------------------------
// Mesh and VertexFormat Data
//--------------------------------------------------------------------------------------

interface //#################################################################### ■

uses LUX, LUX.Code.C;

type //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【型】

     //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【レコード】

     T_Vertex = record
       posX, posY, posZ, posW :T_float;  // Position data
       r, g, b, a             :T_float;  // Color
     end;

     T_VertexUV = record
       posX, posY, posZ, posW :T_float;  // Position data
       u, v                   :T_float;  // texture u,v
     end;

const //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【定数】

      g_vbData :array [ 0..36-1 ] of T_Vertex = (
          ( posX:-1; posY:-1; posZ:-1; posW:+1; r:0; g:0; b:0; a:1 ), ( posX:+1; posY:-1; posZ:-1; posW:+1; r:1; g:0; b:0; a:1 ), ( posX:-1; posY:+1; posZ:-1; posW:+1; r:0; g:1; b:0; a:1 ),
          ( posX:-1; posY:+1; posZ:-1; posW:+1; r:0; g:1; b:0; a:1 ), ( posX:+1; posY:-1; posZ:-1; posW:+1; r:1; g:0; b:0; a:1 ), ( posX:+1; posY:+1; posZ:-1; posW:+1; r:1; g:1; b:0; a:1 ),

          ( posX:-1; posY:-1; posZ:+1; posW:+1; r:0; g:0; b:1; a:1 ), ( posX:-1; posY:+1; posZ:+1; posW:+1; r:0; g:1; b:1; a:1 ), ( posX:+1; posY:-1; posZ:+1; posW:+1; r:1; g:0; b:1; a:1 ),
          ( posX:+1; posY:-1; posZ:+1; posW:+1; r:1; g:0; b:1; a:1 ), ( posX:-1; posY:+1; posZ:+1; posW:+1; r:0; g:1; b:1; a:1 ), ( posX:+1; posY:+1; posZ:+1; posW:+1; r:1; g:1; b:1; a:1 ),

          ( posX:+1; posY:+1; posZ:+1; posW:+1; r:1; g:1; b:1; a:1 ), ( posX:+1; posY:+1; posZ:-1; posW:+1; r:1; g:1; b:0; a:1 ), ( posX:+1; posY:-1; posZ:+1; posW:+1; r:1; g:0; b:1; a:1 ),
          ( posX:+1; posY:-1; posZ:+1; posW:+1; r:1; g:0; b:1; a:1 ), ( posX:+1; posY:+1; posZ:-1; posW:+1; r:1; g:1; b:0; a:1 ), ( posX:+1; posY:-1; posZ:-1; posW:+1; r:1; g:0; b:0; a:1 ),

          ( posX:-1; posY:+1; posZ:+1; posW:+1; r:0; g:1; b:1; a:1 ), ( posX:-1; posY:-1; posZ:+1; posW:+1; r:0; g:0; b:1; a:1 ), ( posX:-1; posY:+1; posZ:-1; posW:+1; r:0; g:1; b:0; a:1 ),
          ( posX:-1; posY:+1; posZ:-1; posW:+1; r:0; g:1; b:0; a:1 ), ( posX:-1; posY:-1; posZ:+1; posW:+1; r:0; g:0; b:1; a:1 ), ( posX:-1; posY:-1; posZ:-1; posW:+1; r:0; g:0; b:0; a:1 ),

          ( posX:+1; posY:+1; posZ:+1; posW:+1; r:1; g:1; b:1; a:1 ), ( posX:-1; posY:+1; posZ:+1; posW:+1; r:0; g:1; b:1; a:1 ), ( posX:+1; posY:+1; posZ:-1; posW:+1; r:1; g:1; b:0; a:1 ),
          ( posX:+1; posY:+1; posZ:-1; posW:+1; r:1; g:1; b:0; a:1 ), ( posX:-1; posY:+1; posZ:+1; posW:+1; r:0; g:1; b:1; a:1 ), ( posX:-1; posY:+1; posZ:-1; posW:+1; r:0; g:1; b:0; a:1 ),

          ( posX:+1; posY:-1; posZ:+1; posW:+1; r:1; g:0; b:1; a:1 ), ( posX:+1; posY:-1; posZ:-1; posW:+1; r:1; g:0; b:0; a:1 ), ( posX:-1; posY:-1; posZ:+1; posW:+1; r:0; g:0; b:1; a:1 ),
          ( posX:-1; posY:-1; posZ:+1; posW:+1; r:0; g:0; b:1; a:1 ), ( posX:+1; posY:-1; posZ:-1; posW:+1; r:1; g:0; b:0; a:1 ), ( posX:-1; posY:-1; posZ:-1; posW:+1; r:0; g:0; b:0; a:1 ) );

     g_vb_solid_face_colors_Data :array [ 0..36-1 ] of T_Vertex = (
          // red face
          ( posX:-1; posY:-1; posZ:+1; posW:+1; r:1; g:0; b:0; a:1 ),
          ( posX:-1; posY:+1; posZ:+1; posW:+1; r:1; g:0; b:0; a:1 ),
          ( posX:+1; posY:-1; posZ:+1; posW:+1; r:1; g:0; b:0; a:1 ),
          ( posX:+1; posY:-1; posZ:+1; posW:+1; r:1; g:0; b:0; a:1 ),
          ( posX:-1; posY:+1; posZ:+1; posW:+1; r:1; g:0; b:0; a:1 ),
          ( posX:+1; posY:+1; posZ:+1; posW:+1; r:1; g:0; b:0; a:1 ),
          // green face
          ( posX:-1; posY:-1; posZ:-1; posW:+1; r:0; g:1; b:0; a:1 ),
          ( posX:+1; posY:-1; posZ:-1; posW:+1; r:0; g:1; b:0; a:1 ),
          ( posX:-1; posY:+1; posZ:-1; posW:+1; r:0; g:1; b:0; a:1 ),
          ( posX:-1; posY:+1; posZ:-1; posW:+1; r:0; g:1; b:0; a:1 ),
          ( posX:+1; posY:-1; posZ:-1; posW:+1; r:0; g:1; b:0; a:1 ),
          ( posX:+1; posY:+1; posZ:-1; posW:+1; r:0; g:1; b:0; a:1 ),
          // blue face
          ( posX:-1; posY:+1; posZ:+1; posW:+1; r:0; g:0; b:1; a:1 ),
          ( posX:-1; posY:-1; posZ:+1; posW:+1; r:0; g:0; b:1; a:1 ),
          ( posX:-1; posY:+1; posZ:-1; posW:+1; r:0; g:0; b:1; a:1 ),
          ( posX:-1; posY:+1; posZ:-1; posW:+1; r:0; g:0; b:1; a:1 ),
          ( posX:-1; posY:-1; posZ:+1; posW:+1; r:0; g:0; b:1; a:1 ),
          ( posX:-1; posY:-1; posZ:-1; posW:+1; r:0; g:0; b:1; a:1 ),
          // yellow face
          ( posX:+1; posY:+1; posZ:+1; posW:+1; r:1; g:1; b:0; a:1 ),
          ( posX:+1; posY:+1; posZ:-1; posW:+1; r:1; g:1; b:0; a:1 ),
          ( posX:+1; posY:-1; posZ:+1; posW:+1; r:1; g:1; b:0; a:1 ),
          ( posX:+1; posY:-1; posZ:+1; posW:+1; r:1; g:1; b:0; a:1 ),
          ( posX:+1; posY:+1; posZ:-1; posW:+1; r:1; g:1; b:0; a:1 ),
          ( posX:+1; posY:-1; posZ:-1; posW:+1; r:1; g:1; b:0; a:1 ),
          // magenta face
          ( posX:+1; posY:+1; posZ:+1; posW:+1; r:1; g:0; b:1; a:1 ),
          ( posX:-1; posY:+1; posZ:+1; posW:+1; r:1; g:0; b:1; a:1 ),
          ( posX:+1; posY:+1; posZ:-1; posW:+1; r:1; g:0; b:1; a:1 ),
          ( posX:+1; posY:+1; posZ:-1; posW:+1; r:1; g:0; b:1; a:1 ),
          ( posX:-1; posY:+1; posZ:+1; posW:+1; r:1; g:0; b:1; a:1 ),
          ( posX:-1; posY:+1; posZ:-1; posW:+1; r:1; g:0; b:1; a:1 ),
          // cyan face
          ( posX:+1; posY:-1; posZ:+1; posW:+1; r:0; g:1; b:1; a:1 ),
          ( posX:+1; posY:-1; posZ:-1; posW:+1; r:0; g:1; b:1; a:1 ),
          ( posX:-1; posY:-1; posZ:+1; posW:+1; r:0; g:1; b:1; a:1 ),
          ( posX:-1; posY:-1; posZ:+1; posW:+1; r:0; g:1; b:1; a:1 ),
          ( posX:+1; posY:-1; posZ:-1; posW:+1; r:0; g:1; b:1; a:1 ),
          ( posX:-1; posY:-1; posZ:-1; posW:+1; r:0; g:1; b:1; a:1 ) );

     g_vb_texture_Data :array [ 0..36-1 ] of T_VertexUV = (
          // left face
          ( posX:-1; posY:-1; posZ:-1; posW:+1; u:1; v:0 ),    // lft-top-front
          ( posX:-1; posY:+1; posZ:+1; posW:+1; u:0; v:1 ),    // lft-btm-back
          ( posX:-1; posY:-1; posZ:+1; posW:+1; u:0; v:0 ),    // lft-top-back
          ( posX:-1; posY:+1; posZ:+1; posW:+1; u:0; v:1 ),    // lft-btm-back
          ( posX:-1; posY:-1; posZ:-1; posW:+1; u:1; v:0 ),    // lft-top-front
          ( posX:-1; posY:+1; posZ:-1; posW:+1; u:1; v:1 ),    // lft-btm-front
          // front face
          ( posX:-1; posY:-1; posZ:-1; posW:+1; u:0; v:0 ),    // lft-top-front
          ( posX:+1; posY:-1; posZ:-1; posW:+1; u:1; v:0 ),    // rgt-top-front
          ( posX:+1; posY:+1; posZ:-1; posW:+1; u:1; v:1 ),    // rgt-btm-front
          ( posX:-1; posY:-1; posZ:-1; posW:+1; u:0; v:0 ),    // lft-top-front
          ( posX:+1; posY:+1; posZ:-1; posW:+1; u:1; v:1 ),    // rgt-btm-front
          ( posX:-1; posY:+1; posZ:-1; posW:+1; u:0; v:1 ),    // lft-btm-front
          // top face
          ( posX:-1; posY:-1; posZ:-1; posW:+1; u:0; v:1 ),    // lft-top-front
          ( posX:+1; posY:-1; posZ:+1; posW:+1; u:1; v:0 ),    // rgt-top-back
          ( posX:+1; posY:-1; posZ:-1; posW:+1; u:1; v:1 ),    // rgt-top-front
          ( posX:-1; posY:-1; posZ:-1; posW:+1; u:0; v:1 ),    // lft-top-front
          ( posX:-1; posY:-1; posZ:+1; posW:+1; u:0; v:0 ),    // lft-top-back
          ( posX:+1; posY:-1; posZ:+1; posW:+1; u:1; v:0 ),    // rgt-top-back
          // bottom face
          ( posX:-1; posY:+1; posZ:-1; posW:+1; u:0; v:0 ),    // lft-btm-front
          ( posX:+1; posY:+1; posZ:+1; posW:+1; u:1; v:1 ),    // rgt-btm-back
          ( posX:-1; posY:+1; posZ:+1; posW:+1; u:0; v:1 ),    // lft-btm-back
          ( posX:-1; posY:+1; posZ:-1; posW:+1; u:0; v:0 ),    // lft-btm-front
          ( posX:+1; posY:+1; posZ:-1; posW:+1; u:1; v:0 ),    // rgt-btm-front
          ( posX:+1; posY:+1; posZ:+1; posW:+1; u:1; v:1 ),    // rgt-btm-back
          // right face
          ( posX:+1; posY:+1; posZ:-1; posW:+1; u:0; v:1 ),    // rgt-btm-front
          ( posX:+1; posY:-1; posZ:+1; posW:+1; u:1; v:0 ),    // rgt-top-back
          ( posX:+1; posY:+1; posZ:+1; posW:+1; u:1; v:1 ),    // rgt-btm-back
          ( posX:+1; posY:-1; posZ:+1; posW:+1; u:1; v:0 ),    // rgt-top-back
          ( posX:+1; posY:+1; posZ:-1; posW:+1; u:0; v:1 ),    // rgt-btm-front
          ( posX:+1; posY:-1; posZ:-1; posW:+1; u:0; v:0 ),    // rgt-top-front
          // back face
          ( posX:-1; posY:+1; posZ:+1; posW:+1; u:1; v:1 ),    // lft-btm-back
          ( posX:+1; posY:+1; posZ:+1; posW:+1; u:0; v:1 ),    // rgt-btm-back
          ( posX:-1; posY:-1; posZ:+1; posW:+1; u:1; v:0 ),    // lft-top-back
          ( posX:-1; posY:-1; posZ:+1; posW:+1; u:1; v:0 ),    // lft-top-back
          ( posX:+1; posY:+1; posZ:+1; posW:+1; u:0; v:1 ),    // rgt-btm-back
          ( posX:+1; posY:-1; posZ:+1; posW:+1; u:0; v:0 ) );  // rgt-top-back

implementation //############################################################### ■

end. //######################################################################### ■
