unit Main;

{ https://github.com/LunarG/VulkanSamples/tree/master/API-Samples/10-init_render_pass }

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
Initialize Render Pass
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
    const sample_title = 'Renderpass Sample';
  public
    { public 宣言 }
    info                   :T_sample_info;
    imageAcquiredSemaphore :VkSemaphore;
  end;

var
  Form1: TForm1;

implementation //############################################################### ■

{$R *.fmx}

uses System.Math;

procedure TForm1.FormCreate(Sender: TObject);
var
   res                              :VkResult;
   imageAcquiredSemaphoreCreateInfo :VkSemaphoreCreateInfo;
   attachments                      :array [ 0..2-1 ] of VkAttachmentDescription;
   color_reference                  :VkAttachmentReference;
   depth_reference                  :VkAttachmentReference;
   subpass                          :VkSubpassDescription;
   subpass_dependency               :VkSubpassDependency;
   rp_info                          :VkRenderPassCreateInfo;
begin
     init_global_layer_properties( info );
     init_instance_extension_names( info );
     init_device_extension_names( info );
     init_instance( info, sample_title );
     init_enumerate_device( info );
     init_connection( info );
     init_window_size( info, 500, 500 );
     init_window( info );
     init_swapchain_extension( info );
     init_device(info);
     init_device_queue( info );
     init_swap_chain( info );
     init_depth_buffer( info );

     (* VULKAN_KEY_START *)

     // A semaphore (or fence) is required in order to acquire a
     // swapchain image to prepare it for use in a render pass.
     // The semaphore is normally used to hold back the rendering
     // operation until the image is actually available.
     // But since this sample does not render, the semaphore
     // ends up being unused.

     imageAcquiredSemaphoreCreateInfo.sType := VK_STRUCTURE_TYPE_SEMAPHORE_CREATE_INFO;
     imageAcquiredSemaphoreCreateInfo.pNext := nil;
     imageAcquiredSemaphoreCreateInfo.flags := 0;

     res := vkCreateSemaphore( info.device, @imageAcquiredSemaphoreCreateInfo, nil, @imageAcquiredSemaphore );
     assert( res = VK_SUCCESS );

     // Acquire the swapchain image in order to set its layout
     res := vkAcquireNextImageKHR( info.device, info.swap_chain, UINT64_MAX, imageAcquiredSemaphore, VK_NULL_HANDLE, @info.current_buffer );
     Assert( res >= VK_SUCCESS );

     // The initial layout for the color and depth attachments will be
     // LAYOUT_UNDEFINED because at the start of the renderpass, we don't
     // care about their contents. At the start of the subpass, the color
     // attachment's layout will be transitioned to LAYOUT_COLOR_ATTACHMENT_OPTIMAL
     // and the depth stencil attachment's layout will be transitioned to
     // LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL.  At the end of the renderpass,
     // the color attachment's layout will be transitioned to
     // LAYOUT_PRESENT_SRC_KHR to be ready to present.  This is all done as part
     // of the renderpass, no barriers are necessary.
     attachments[0].format         := info.format;
     attachments[0].samples        := NUM_SAMPLES;
     attachments[0].loadOp         := VK_ATTACHMENT_LOAD_OP_CLEAR;
     attachments[0].storeOp        := VK_ATTACHMENT_STORE_OP_STORE;
     attachments[0].stencilLoadOp  := VK_ATTACHMENT_LOAD_OP_DONT_CARE;
     attachments[0].stencilStoreOp := VK_ATTACHMENT_STORE_OP_DONT_CARE;
     attachments[0].initialLayout  := VK_IMAGE_LAYOUT_UNDEFINED;
     attachments[0].finalLayout    := VK_IMAGE_LAYOUT_PRESENT_SRC_KHR;
     attachments[0].flags          := 0;

     attachments[1].format         := info.depth.format;
     attachments[1].samples        := NUM_SAMPLES;
     attachments[1].loadOp         := VK_ATTACHMENT_LOAD_OP_CLEAR;
     attachments[1].storeOp        := VK_ATTACHMENT_STORE_OP_DONT_CARE;
     attachments[1].stencilLoadOp  := VK_ATTACHMENT_LOAD_OP_DONT_CARE;
     attachments[1].stencilStoreOp := VK_ATTACHMENT_STORE_OP_DONT_CARE;
     attachments[1].initialLayout  := VK_IMAGE_LAYOUT_UNDEFINED;
     attachments[1].finalLayout    := VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL;
     attachments[1].flags          := 0;

     color_reference.attachment := 0;
     color_reference.layout     := VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL;

     depth_reference.attachment := 1;
     depth_reference.layout     := VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL;

     subpass.pipelineBindPoint       := VK_PIPELINE_BIND_POINT_GRAPHICS;
     subpass.flags                   := 0;
     subpass.inputAttachmentCount    := 0;
     subpass.pInputAttachments       := nil;
     subpass.colorAttachmentCount    := 1;
     subpass.pColorAttachments       := @color_reference;
     subpass.pResolveAttachments     := nil;
     subpass.pDepthStencilAttachment := @depth_reference;
     subpass.preserveAttachmentCount := 0;
     subpass.pPreserveAttachments    := nil;

     // Subpass dependency to wait for wsi image acquired semaphore before starting layout transition
     subpass_dependency.srcSubpass      := VK_SUBPASS_EXTERNAL;
     subpass_dependency.dstSubpass      := 0;
     subpass_dependency.srcStageMask    := VkPipelineStageFlags( VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT );
     subpass_dependency.dstStageMask    := VkPipelineStageFlags( VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT );
     subpass_dependency.srcAccessMask   := 0;
     subpass_dependency.dstAccessMask   := VkAccessFlags( VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT );
     subpass_dependency.dependencyFlags := 0;

     rp_info.sType           := VK_STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO;
     rp_info.pNext           := nil;
     rp_info.attachmentCount := 2;
     rp_info.pAttachments    := @attachments[0];
     rp_info.subpassCount    := 1;
     rp_info.pSubpasses      := @subpass;
     rp_info.dependencyCount := 1;
     rp_info.pDependencies   := @subpass_dependency;

     res := vkCreateRenderPass( info.device, @rp_info, nil, @info.render_pass );
     Assert( res = VK_SUCCESS );
     (* VULKAN_KEY_END *)
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
     vkDestroyRenderPass( info.device, info.render_pass, nil );
     vkDestroySemaphore( info.device, imageAcquiredSemaphore, nil );
     destroy_depth_buffer( info );
     destroy_swap_chain( info );
     destroy_device( info );
     destroy_window( info );
     destroy_instance( info );
end;

end. //######################################################################### ■
