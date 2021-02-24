unit Main;

{ https://github.com/LunarG/VulkanSamples/tree/master/API-Samples/07-init_uniform_buffer }

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

(*
VULKAN_SAMPLE_SHORT_DESCRIPTION
Create Uniform Buffer
*)

(* This is part of the draw cube progression *)

interface //#################################################################### ■

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Memo.Types, FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo,
  vulkan_core, vulkan_win32,
  vulkan.util, vulkan.util_init,
  LUX, LUX.Code.C,
  LUX.D1, LUX.D2, LUX.D3, LUX.D4, LUX.D4x4;

type
  TForm1 = class(TForm)
    Memo1: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { private 宣言 }
    const sample_title = 'Uniform Buffer Sample';
  public
    { public 宣言 }
    info :T_sample_info;
  end;

var
  Form1: TForm1;

implementation //############################################################### ■

{$R *.fmx}

uses System.Math;

procedure TForm1.FormCreate(Sender: TObject);
var
   res        :VkResult;
   pass       :T_bool;
   buf_info   :VkBufferCreateInfo;
   mem_reqs   :VkMemoryRequirements;
   alloc_info :VkMemoryAllocateInfo;
   pData      :P_uint8_t;
begin
     init_global_layer_properties( info );
     init_instance( info, sample_title );
     init_enumerate_device( info );
     init_queue_family_index( info );
     init_device( info );
     init_window_size( info, 500, 500 );

     info.Projection := TSingleM4.ProjPersH( DegToRad( 45 ), 1, 0.1, 100 );

     info.View := TSingleM4.LookAt( TSingle3D.Create( -5, +3, -10 ),    // Camera is at (-5,3,-10), in World Space
                                    TSingle3D.Create(  0,  0,   0 ),    // and looks at the origin
                                    TSingle3D.Create(  0, -1,   0 ) );  // Head is up (set to 0,-1,0 to look upside-down)

     info.Model := TSingleM4.Identity;

     // Vulkan clip space has inverted Y and half Z.
     // clang-format off
     info.Clip := TSingleM4.Create( +1.0,  0.0,  0.0,  0.0,
                                     0.0, -1.0,  0.0,  0.0,
                                     0.0,  0.0, +0.5,  0.0,
                                     0.0,  0.0, +0.5, +1.0 );
     // clang-format on
     info.MVP := info.Clip * info.Projection * info.View * info.Model;

     (* VULKAN_KEY_START *)
     buf_info.sType                 := VK_STRUCTURE_TYPE_BUFFER_CREATE_INFO;
     buf_info.pNext                 := nil;
     buf_info.usage                 := VkBufferUsageFlags( VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT );
     buf_info.size                  := SizeOf( info.MVP );
     buf_info.queueFamilyIndexCount := 0;
     buf_info.pQueueFamilyIndices   := nil;
     buf_info.sharingMode           := VK_SHARING_MODE_EXCLUSIVE;
     buf_info.flags                 := 0;
     res := vkCreateBuffer( info.device, @buf_info, nil, @info.uniform_data.buf );
     Assert( res = VK_SUCCESS );

     vkGetBufferMemoryRequirements( info.device, info.uniform_data.buf, @mem_reqs );

     alloc_info.sType           := VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO;
     alloc_info.pNext           := nil;
     alloc_info.memoryTypeIndex := 0;

     alloc_info.allocationSize := mem_reqs.size;
     pass := memory_type_from_properties( info, mem_reqs.memoryTypeBits,
                                          VkFlags( VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT ) or VkFlags( VK_MEMORY_PROPERTY_HOST_COHERENT_BIT ),
                                          alloc_info.memoryTypeIndex );
     Assert( pass, 'No mappable, coherent memory' );

     res := vkAllocateMemory( info.device, @alloc_info, nil, @info.uniform_data.mem );
     Assert( res = VK_SUCCESS );

     res := vkMapMemory( info.device, info.uniform_data.mem, 0, mem_reqs.size, 0, @pData );
     Assert( res = VK_SUCCESS );

     Move( pData^, info.MVP, SizeOf( info.MVP ) );

     vkUnmapMemory( info.device, info.uniform_data.mem );

     res := vkBindBufferMemory( info.device, info.uniform_data.buf, info.uniform_data.mem, 0 );
     Assert( res = VK_SUCCESS );

     info.uniform_data.buffer_info.buffer := info.uniform_data.buf;
     info.uniform_data.buffer_info.offset := 0;
     info.uniform_data.buffer_info.range  := SizeOf( info.MVP );
     (* VULKAN_KEY_END *)
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
     vkDestroyBuffer( info.device, info.uniform_data.buf, nil );
     vkFreeMemory( info.device, info.uniform_data.mem, nil );
     destroy_device( info );
     destroy_instance( info );
end;

end. //######################################################################### ■
