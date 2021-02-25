unit Main;

{ https://github.com/LunarG/VulkanSamples/tree/master/API-Samples/15-draw_cube }

(*
 * Vulkan Samples
 *
 * Copyright (C) 2015-2020 Valve Corporation
 * Copyright (C) 2015-2020 LunarG, Inc.
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
Draw Cube
*)

(* This is part of the draw cube progression *)

(* We've setup cmake to process 15-draw_cube.vert and 15-draw_cube.frag                   *)
(* files containing the glsl shader code for this sample.  The generate-spirv script uses *)
(* glslangValidator to compile the glsl into spir-v and places the spir-v into a struct   *)
(* into a generated header file                                                           *)

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
    const sample_title = 'Draw Cube';
  public
    { public 宣言 }
    info                   :T_sample_info;
    imageAcquiredSemaphore :VkSemaphore;
    drawFence              :VkFence;
  end;

var
  Form1: TForm1;

implementation //############################################################### ■

{$R *.fmx}

uses cube_data;

procedure TForm1.FormCreate(Sender: TObject);
const
     depthPresent :T_bool = True;
     offsets      :array [ 0..1-1 ] of VkDeviceSize = ( 0 );
var
   res                              :VkResult;
   __draw_cube_vert                 :TMemoryStream;
   __draw_cube_frag                 :TMemoryStream;
   vert_info                        :VkShaderModuleCreateInfo;
   frag_info                        :VkShaderModuleCreateInfo;
   clear_values                     :array [ 0..2-1 ] of VkClearValue;
   imageAcquiredSemaphoreCreateInfo :VkSemaphoreCreateInfo;
   rp_begin                         :VkRenderPassBeginInfo;
   cmd_bufs                         :array [ 0..1-1 ] of VkCommandBuffer;
   fenceInfo                        :VkFenceCreateInfo;
   pipe_stage_flags                 :VkPipelineStageFlags;
   submit_info                      :array [ 0..1-1 ] of VkSubmitInfo;
   present                          :VkPresentInfoKHR;
begin
     __draw_cube_vert := TMemoryStream.Create;
     __draw_cube_frag := TMemoryStream.Create;

     process_command_line_args( info );
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
     init_uniform_buffer( info );
     init_descriptor_and_pipeline_layouts( info, false );
     init_renderpass( info, depthPresent );
     __draw_cube_vert.LoadFromFile( '../../_DATA/15-draw_cube.vert' );
     __draw_cube_frag.LoadFromFile( '../../_DATA/15-draw_cube.frag' );
     vert_info.sType    := VK_STRUCTURE_TYPE_SHADER_MODULE_CREATE_INFO;
     frag_info.sType    := VK_STRUCTURE_TYPE_SHADER_MODULE_CREATE_INFO;
     vert_info.codeSize := __draw_cube_vert.Size;
     vert_info.pCode    := __draw_cube_vert.Memory;
     frag_info.codeSize := __draw_cube_frag.Size;
     frag_info.pCode    := __draw_cube_frag.Memory;
     init_shaders( info, @vert_info, @frag_info );
     init_framebuffers( info, depthPresent );
     init_vertex_buffer( info, @g_vb_solid_face_colors_Data[0], SizeOf( T_Vertex ) * Length( g_vb_solid_face_colors_Data ),
                         SizeOf( T_Vertex ), False );
     init_descriptor_pool( info, false );
     init_descriptor_set( info, false );
     init_pipeline_cache( info );
     if depthPresent then init_pipeline( info, 1 )
                     else init_pipeline( info, 0 );

     (* VULKAN_KEY_START *)

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
     Assert( res = VK_SUCCESS );

     // Get the index of the next available swapchain image:
     res := vkAcquireNextImageKHR( info.device, info.swap_chain, UINT64_MAX, imageAcquiredSemaphore, VK_NULL_HANDLE,
                                   @info.current_buffer );
     // TODO: Deal with the VK_SUBOPTIMAL_KHR and VK_ERROR_OUT_OF_DATE_KHR
     // return codes
     Assert( res = VK_SUCCESS );

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

     vkCmdBindPipeline( info.cmd, VK_PIPELINE_BIND_POINT_GRAPHICS, info.pipeline );
     vkCmdBindDescriptorSets( info.cmd, VK_PIPELINE_BIND_POINT_GRAPHICS, info.pipeline_layout, 0, NUM_DESCRIPTOR_SETS,
                              @info.desc_set[0], 0, nil );

     vkCmdBindVertexBuffers( info.cmd, 0, 1, @info.vertex_buffer.buf, @offsets[0] );

     init_viewports( info );
     init_scissors( info );

     vkCmdDraw( info.cmd, 12 * 3, 1, 0, 0 );
     vkCmdEndRenderPass( info.cmd );
     res := vkEndCommandBuffer( info.cmd );
     cmd_bufs[0] := info.cmd;
     fenceInfo.sType := VK_STRUCTURE_TYPE_FENCE_CREATE_INFO;
     fenceInfo.pNext := nil;
     fenceInfo.flags := 0;
     vkCreateFence( info.device, @fenceInfo, nil, @drawFence );

     pipe_stage_flags := Ord( VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT );
     submit_info[0].pNext                := nil;
     submit_info[0].sType                := VK_STRUCTURE_TYPE_SUBMIT_INFO;
     submit_info[0].waitSemaphoreCount   := 1;
     submit_info[0].pWaitSemaphores      := @imageAcquiredSemaphore;
     submit_info[0].pWaitDstStageMask    := @pipe_stage_flags;
     submit_info[0].commandBufferCount   := 1;
     submit_info[0].pCommandBuffers      := @cmd_bufs[0];
     submit_info[0].signalSemaphoreCount := 0;
     submit_info[0].pSignalSemaphores    := nil;

     (* Queue the command buffer for execution *)
     res := vkQueueSubmit( info.graphics_queue, 1, @submit_info[0], drawFence );
     Assert( res = VK_SUCCESS );

     (* Now present the image in the window *)

     present.sType              := VK_STRUCTURE_TYPE_PRESENT_INFO_KHR;
     present.pNext              := nil;
     present.swapchainCount     := 1;
     present.pSwapchains        := @info.swap_chain;
     present.pImageIndices      := @info.current_buffer;
     present.pWaitSemaphores    := nil;
     present.waitSemaphoreCount := 0;
     present.pResults           := nil;

     (* Make sure command buffer is finished before presenting *)
     repeat
           res := vkWaitForFences( info.device, 1, @drawFence, VK_TRUE, FENCE_TIMEOUT );

     until res <> VK_TIMEOUT;

     Assert( res = VK_SUCCESS );
     res := vkQueuePresentKHR( info.present_queue, @present );
     Assert( res = VK_SUCCESS );

     wait_seconds( 1 );
     (* VULKAN_KEY_END *)
     if info.save_images then write_ppm( info, '15-draw_cube' );

     __draw_cube_vert.Free;
     __draw_cube_frag.Free;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
     vkDestroySemaphore( info.device, imageAcquiredSemaphore, nil );
     vkDestroyFence( info.device, drawFence, nil );
     destroy_pipeline( info );
     destroy_pipeline_cache( info );
     destroy_descriptor_pool( info );
     destroy_vertex_buffer( info );
     destroy_framebuffers( info );
     destroy_shaders( info );
     destroy_renderpass( info );
     destroy_descriptor_and_pipeline_layouts( info );
     destroy_uniform_buffer( info );
     destroy_depth_buffer( info );
     destroy_swap_chain( info );
     destroy_command_buffer( info );
     destroy_command_pool( info );
     destroy_device( info );
     destroy_window( info );
     destroy_instance( info );
end;

end. //######################################################################### ■
