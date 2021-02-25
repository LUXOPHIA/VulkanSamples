unit Main;

{ https://github.com/LunarG/VulkanSamples/tree/master/API-Samples/13-init_vertex_buffer }

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
Create Vertex Buffer
*)

(* This is part of the draw cube progression *)

interface //#################################################################### ■

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Memo.Types, FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo,
  vulkan_core,
  vulkan.util, vulkan.util_init,
  LUX, LUX.Code.C;

type
  TForm1 = class(TForm)
    Memo1: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { private 宣言 }
    const sample_title = 'Vertex Buffer Sample';
  public
    { public 宣言 }
    info                   :T_sample_info;
    imageAcquiredSemaphore :VkSemaphore;
  end;

var
  Form1: TForm1;

implementation //############################################################### ■

{$R *.fmx}

uses WinApi.Windows,
     cube_data;

procedure TForm1.FormCreate(Sender: TObject);
const
     depthPresent :T_bool = True;
var
   res                              :VkResult;
   pass                             :T_bool;
   buf_info                         :VkBufferCreateInfo;
   mem_reqs                         :VkMemoryRequirements;
   alloc_info                       :VkMemoryAllocateInfo;
   pData                            :P_uint8_t;
   offsets                          :array [ 0..1-1 ] of VkDeviceSize;
   clear_values                     :array [ 0..2-1 ] of VkClearValue;
   imageAcquiredSemaphoreCreateInfo :VkSemaphoreCreateInfo;
   rp_begin                         :VkRenderPassBeginInfo;
begin
     init_global_layer_properties( info );
     init_instance_extension_names( info );
     init_device_extension_names( info );
     init_instance( info, sample_title );
     init_enumerate_device( info );
     init_window_size( info, 500, 500 );
     init_connection( info );
     init_window( info );
     init_swapchain_extension( info );
     init_device( info );
     init_command_pool( info );
     init_command_buffer( info );
     execute_begin_command_buffer( info );
     init_device_queue( info );
     init_swap_chain( info );
     init_depth_buffer( info );
     init_renderpass( info, depthPresent );
     init_framebuffers( info, depthPresent );

     (* VULKAN_KEY_START *)
     (*
     * Set up a vertex buffer:
     * - Create a buffer
     * - Map it and write the vertex data into it
     * - Bind it using vkCmdBindVertexBuffers
     * - Later, at pipeline creation,
     * -      fill in vertex input part of the pipeline with relevent data
     *)

     buf_info.sType                 := VK_STRUCTURE_TYPE_BUFFER_CREATE_INFO;
     buf_info.pNext                 := nil;
     buf_info.usage                 := Ord( VK_BUFFER_USAGE_VERTEX_BUFFER_BIT );
     buf_info.size                  := SizeOf( T_Vertex ) * Length( g_vb_solid_face_colors_Data );
     buf_info.queueFamilyIndexCount := 0;
     buf_info.pQueueFamilyIndices   := nil;
     buf_info.sharingMode           := VK_SHARING_MODE_EXCLUSIVE;
     buf_info.flags                 := 0;
     res := vkCreateBuffer( info.device, @buf_info, nil, @info.vertex_buffer.buf );
     Assert( res = VK_SUCCESS, 'vkCreateBuffer' );

     vkGetBufferMemoryRequirements( info.device, info.vertex_buffer.buf, @mem_reqs );

     alloc_info.sType := VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO;
     alloc_info.pNext := nil;
     alloc_info.memoryTypeIndex := 0;

     alloc_info.allocationSize := mem_reqs.size;
     pass := memory_type_from_properties( info, mem_reqs.memoryTypeBits,
                                          Ord( VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT ) or Ord( VK_MEMORY_PROPERTY_HOST_COHERENT_BIT ),
                                          alloc_info.memoryTypeIndex );
     Assert( pass, 'No mappable, coherent memory' );

     res := vkAllocateMemory( info.device, @alloc_info, nil, @info.vertex_buffer.mem );
     Assert( res = VK_SUCCESS );

     res := vkMapMemory( info.device, info.vertex_buffer.mem, 0, mem_reqs.size, 0, @pData );
     Assert( res = VK_SUCCESS, 'vkMapMemory' );

     Move( g_vb_solid_face_colors_Data[0], pData^, SizeOf( T_Vertex ) * Length( g_vb_solid_face_colors_Data ) );

     vkUnmapMemory( info.device, info.vertex_buffer.mem );

     res := vkBindBufferMemory( info.device, info.vertex_buffer.buf, info.vertex_buffer.mem, 0 );
     Assert( res = VK_SUCCESS, 'vkBindBufferMemory' );

     (* We won't use these here, but we will need this info when creating the
     * pipeline *)
     info.vi_binding.binding   := 0;
     info.vi_binding.inputRate := VK_VERTEX_INPUT_RATE_VERTEX;
     info.vi_binding.stride    := SizeOf( T_Vertex ) * Length( g_vb_solid_face_colors_Data );

     info.vi_attribs[0].binding  := 0;
     info.vi_attribs[0].location := 0;
     info.vi_attribs[0].format   := VK_FORMAT_R32G32B32A32_SFLOAT;
     info.vi_attribs[0].offset   := 0;
     info.vi_attribs[1].binding  := 0;
     info.vi_attribs[1].location := 1;
     info.vi_attribs[1].format   := VK_FORMAT_R32G32B32A32_SFLOAT;
     info.vi_attribs[1].offset   := 16;

     offsets[0] := 0;

     (* We cannot bind the vertex buffer until we begin a renderpass *)
     clear_values[0].color.float32[0]     := 0.2;
     clear_values[0].color.float32[1]     := 0.2;
     clear_values[0].color.float32[2]     := 0.2;
     clear_values[0].color.float32[3]     := 0.2;
     clear_values[1].depthStencil.depth   := 1.0;
     clear_values[1].depthStencil.stencil := 0;

     imageAcquiredSemaphoreCreateInfo.sType := VK_STRUCTURE_TYPE_SEMAPHORE_CREATE_INFO;
     imageAcquiredSemaphoreCreateInfo.pNext := nil;
     imageAcquiredSemaphoreCreateInfo.flags := 0;

     res := vkCreateSemaphore( info.device, @imageAcquiredSemaphoreCreateInfo, nil, @imageAcquiredSemaphore );
     Assert( res = VK_SUCCESS, 'vkCreateSemaphore' );

     // Get the index of the next available swapchain image:
     res := vkAcquireNextImageKHR( info.device, info.swap_chain, UINT64_MAX, imageAcquiredSemaphore, VK_NULL_HANDLE,
                                   @info.current_buffer );
     // TODO: Deal with the VK_SUBOPTIMAL_KHR and VK_ERROR_OUT_OF_DATE_KHR
     // return codes
     Assert( res = VK_SUCCESS, 'vkAcquireNextImageKHR' );

     rp_begin.sType                    := VK_STRUCTURE_TYPE_RENDER_PASS_BEGIN_INFO;
     rp_begin.pNext                    := nil;
     rp_begin.renderPass               := info.render_pass;
     rp_begin.framebuffer              := info.framebuffers[info.current_buffer];
     rp_begin.renderArea.offset.x      := 0;
     rp_begin.renderArea.offset.y      := 0;
     rp_begin.renderArea.extent.width  := info.width;
     rp_begin.renderArea.extent.height := info.height;
     rp_begin.clearValueCount          := 2;
     rp_begin.pClearValues             := @clear_values[0];

     vkCmdBeginRenderPass( info.cmd, @rp_begin, VK_SUBPASS_CONTENTS_INLINE );   { TODO : ACCESS_VIOLATION @ nvoglv64.dll }

     vkCmdBindVertexBuffers( info.cmd, 0,              (* Start Binding *)
                             1,                        (* Binding Count *)
                             @info.vertex_buffer.buf,  (* pBuffers      *)
                             @offsets[0] );            (* pOffsets      *)

     vkCmdEndRenderPass( info.cmd );
     execute_end_command_buffer( info );
     execute_queue_command_buffer( info );
     (* VULKAN_KEY_END *)
end;

procedure TForm1.FormDestroy( Sender: TObject );
begin
     vkDestroySemaphore( info.device, imageAcquiredSemaphore, nil );
     vkDestroyBuffer( info.device, info.vertex_buffer.buf, nil );
     vkFreeMemory( info.device, info.vertex_buffer.mem, nil );
     destroy_framebuffers( info );
     destroy_renderpass( info );
     destroy_depth_buffer( info );
     destroy_swap_chain( info );
     destroy_command_buffer( info );
     destroy_command_pool( info );
     destroy_device( info );
     destroy_window( info );
     destroy_instance( info );
end;

end. //######################################################################### ■
