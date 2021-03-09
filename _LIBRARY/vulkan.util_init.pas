unit vulkan.util_init;

(*
 * Vulkan Samples
 *
 * Copyright (C) 2015-2020 Valve Corporation
 * Copyright (C) 2015-2020 LunarG, Inc.
 * Copyright (C) 2015-2020 Google, Inc.
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

interface //#################################################################### ■

uses vulkan_core, vulkan_win32,
     vulkan.util,
     LUX.Code.C,
     LUX.GPU.Vulkan;

//type //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【型】

     //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【レコード】

     //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【クラス】

//const //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【定数】

//var //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【変数】

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【ルーチン】

//////////////////////////////////////////////////////////////////////////////// 01-init_instance

//////////////////////////////////////////////////////////////////////////////// 02-enumerate_devices

//////////////////////////////////////////////////////////////////////////////// 03-init_device

//////////////////////////////////////////////////////////////////////////////// 04-init_command_buffer

//////////////////////////////////////////////////////////////////////////////// 05-init_swapchain

//////////////////////////////////////////////////////////////////////////////// 06-init_depth_buffer

//////////////////////////////////////////////////////////////////////////////// 07-init_uniform_buffer

//////////////////////////////////////////////////////////////////////////////// 08-init_pipeline_layout

//////////////////////////////////////////////////////////////////////////////// 09-init_descriptor_set

procedure init_descriptor_and_pipeline_layouts( const Vulkan_:TVulkan; use_texture_:T_bool; descSetLayoutCreateFlags_:VkDescriptorSetLayoutCreateFlags = 0 );
procedure destroy_descriptor_and_pipeline_layouts( const Vulkan_:TVulkan );

//////////////////////////////////////////////////////////////////////////////// 10-init_render_pass

procedure init_depth_buffer( const Vulkan_:TVulkan );
procedure destroy_depth_buffer( const Vulkan_:TVulkan );

//////////////////////////////////////////////////////////////////////////////// 11-init_shaders

//////////////////////////////////////////////////////////////////////////////// 12-init_frame_buffers

procedure init_renderpass( const Vulkan_:TVulkan;
                           include_depth_:T_bool;
                           clear_:T_bool = True;
                           finalLayout_:VkImageLayout = VK_IMAGE_LAYOUT_PRESENT_SRC_KHR;
                           initialLayout_:VkImageLayout = VK_IMAGE_LAYOUT_UNDEFINED );
procedure execute_queue_command_buffer( const Vulkan_:TVulkan );
procedure destroy_renderpass( const Vulkan_:TVulkan );

//////////////////////////////////////////////////////////////////////////////// 13-init_vertex_buffer

procedure init_framebuffers( const Vulkan_:TVulkan; include_depth:T_bool );
procedure destroy_framebuffers( const Vulkan_:TVulkan );

//////////////////////////////////////////////////////////////////////////////// 14-init_pipeline

procedure init_vertex_buffer( const Vulkan_:TVulkan; const vertexData_:P_void; dataSize_:T_uint32_t; dataStride_:T_uint32_t; use_texture_:T_bool );
procedure init_descriptor_pool( const Vulkan_:TVulkan; use_texture_:T_bool );
procedure init_descriptor_set( const Vulkan_:TVulkan; use_texture_:T_bool );
procedure destroy_descriptor_pool( const Vulkan_:TVulkan );
procedure destroy_vertex_buffer( const Vulkan_:TVulkan );

//////////////////////////////////////////////////////////////////////////////// 15-draw_cube

procedure init_viewports( const Vulkan_:TVulkan );
procedure init_scissors( const Vulkan_:TVulkan );
procedure init_pipeline_cache( const Vulkan_:TVulkan );
procedure destroy_pipeline_cache( const Vulkan_:TVulkan );

//////////////////////////////////////////////////////////////////////////////// draw_textured_cube

procedure init_buffer( const Vulkan_:TVulkan; var texObj_:T_texture_object );
procedure init_image( const Vulkan_:TVulkan; var texObj_:T_texture_object; const textureName_:String; extraUsages_:VkImageUsageFlags; extraFeatures_:VkFormatFeatureFlags );
procedure init_sampler( const Vulkan_:TVulkan; var sampler_:VkSampler );
procedure init_texture( const Vulkan_:TVulkan; const textureName_:String = ''; extraUsages_:VkImageUsageFlags = 0; extraFeatures_:VkFormatFeatureFlags = 0 );
procedure destroy_textures( const Vulkan_:TVulkan );

implementation //############################################################### ■

uses System.Types, System.Math, System.SysUtils,
     FMX.Types,
     Winapi.Windows, Winapi.Messages,
     LUX, LUX.D1, LUX.D2, LUX.D3, LUX.D4, LUX.D4x4;

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【レコード】

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【クラス】

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【ルーチン】

//////////////////////////////////////////////////////////////////////////////// 01-init_instance

//////////////////////////////////////////////////////////////////////////////// 02-enumerate_devices

//////////////////////////////////////////////////////////////////////////////// 03-init_device

//////////////////////////////////////////////////////////////////////////////// 04-init_command_buffer

//////////////////////////////////////////////////////////////////////////////// 05-init_swapchain

//////////////////////////////////////////////////////////////////////////////// 06-init_depth_buffer

//////////////////////////////////////////////////////////////////////////////// 07-init_uniform_buffer

//////////////////////////////////////////////////////////////////////////////// 08-init_pipeline_layout

//////////////////////////////////////////////////////////////////////////////// 09-init_descriptor_set

procedure init_descriptor_and_pipeline_layouts( const Vulkan_:TVulkan; use_texture_:T_bool; descSetLayoutCreateFlags_:VkDescriptorSetLayoutCreateFlags = 0 );
var
   layout_bindings           :array [ 0..2-1 ] of VkDescriptorSetLayoutBinding;
   descriptor_layout         :VkDescriptorSetLayoutCreateInfo;
   res                       :VkResult;
   pPipelineLayoutCreateInfo :VkPipelineLayoutCreateInfo;
begin
     layout_bindings[0].binding            := 0;
     layout_bindings[0].descriptorType     := VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER;
     layout_bindings[0].descriptorCount    := 1;
     layout_bindings[0].stageFlags         := Ord( VK_SHADER_STAGE_VERTEX_BIT );
     layout_bindings[0].pImmutableSamplers := nil;

     if use_texture_ then
     begin
          layout_bindings[1].binding            := 1;
          layout_bindings[1].descriptorType     := VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER;
          layout_bindings[1].descriptorCount    := 1;
          layout_bindings[1].stageFlags         := Ord( VK_SHADER_STAGE_FRAGMENT_BIT );
          layout_bindings[1].pImmutableSamplers := nil;
     end;

     (* Next take layout bindings and use them to create a descriptor set layout
      *)
     descriptor_layout                   := Default( VkDescriptorSetLayoutCreateInfo );
     descriptor_layout.sType             := VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_CREATE_INFO;
     descriptor_layout.pNext             := nil;
     descriptor_layout.flags             := descSetLayoutCreateFlags_;
     if use_texture_
     then descriptor_layout.bindingCount := 2
     else descriptor_layout.bindingCount := 1;
     descriptor_layout.pBindings         := @layout_bindings[0];

     SetLength( Vulkan_.Info.desc_layout, NUM_DESCRIPTOR_SETS );
     res := vkCreateDescriptorSetLayout( Vulkan_.Instans[0].Devices[0].Handle, @descriptor_layout, nil, @Vulkan_.Info.desc_layout[0] );
     Assert( res = VK_SUCCESS );

     (* Now use the descriptor layout to create a pipeline layout *)
     pPipelineLayoutCreateInfo                        := Default( VkPipelineLayoutCreateInfo );
     pPipelineLayoutCreateInfo.sType                  := VK_STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO;
     pPipelineLayoutCreateInfo.pNext                  := nil;
     pPipelineLayoutCreateInfo.pushConstantRangeCount := 0;
     pPipelineLayoutCreateInfo.pPushConstantRanges    := nil;
     pPipelineLayoutCreateInfo.setLayoutCount         := NUM_DESCRIPTOR_SETS;
     pPipelineLayoutCreateInfo.pSetLayouts            := @Vulkan_.Info.desc_layout[0];

     res := vkCreatePipelineLayout( Vulkan_.Instans[0].Devices[0].Handle, @pPipelineLayoutCreateInfo, nil, @Vulkan_.Info.pipeline_layout );
     Assert( res = VK_SUCCESS );
end;

procedure destroy_descriptor_and_pipeline_layouts( const Vulkan_:TVulkan );
var
   i :T_int;
begin
     for i := 0 to NUM_DESCRIPTOR_SETS-1 do vkDestroyDescriptorSetLayout( Vulkan_.Instans[0].Devices[0].Handle, Vulkan_.Info.desc_layout[i], nil );
     vkDestroyPipelineLayout( Vulkan_.Instans[0].Devices[0].Handle, Vulkan_.Info.pipeline_layout, nil );
end;

//////////////////////////////////////////////////////////////////////////////// 10-init_render_pass

procedure init_depth_buffer( const Vulkan_:TVulkan );
var
   res          :VkResult;
   pass         :T_bool;
   image_info   :VkImageCreateInfo;
   props        :VkFormatProperties;
   depth_format :VkFormat;
   mem_alloc    :VkMemoryAllocateInfo;
   view_info    :VkImageViewCreateInfo;
   mem_reqs     :VkMemoryRequirements;
begin
     (* allow custom depth formats *)
     if Vulkan_.Info.depth.format = VK_FORMAT_UNDEFINED then Vulkan_.Info.depth.format := VK_FORMAT_D16_UNORM;

     depth_format := Vulkan_.Info.depth.format;
     vkGetPhysicalDeviceFormatProperties( Vulkan_.Instans[0].Devices[0].Physic, depth_format, @props );
     if ( props.linearTilingFeatures and Ord( VK_FORMAT_FEATURE_DEPTH_STENCIL_ATTACHMENT_BIT ) ) <> 0
     then image_info.tiling := VK_IMAGE_TILING_LINEAR
     else
     if ( props.optimalTilingFeatures and Ord( VK_FORMAT_FEATURE_DEPTH_STENCIL_ATTACHMENT_BIT ) ) <> 0
     then image_info.tiling := VK_IMAGE_TILING_OPTIMAL
     else
     begin
          (* Try other depth formats? *)
          Log.d( 'depth_format ' + Ord( depth_format ).ToString + ' Unsupported.' );
          RunError( 256-1 );
     end;

     image_info                       := Default( VkImageCreateInfo );
     image_info.sType                 := VK_STRUCTURE_TYPE_IMAGE_CREATE_INFO;
     image_info.pNext                 := nil;
     image_info.imageType             := VK_IMAGE_TYPE_2D;
     image_info.format                := depth_format;
     image_info.extent.width          := Vulkan_.Instans[0].Surfacs[0].PxSizeX;
     image_info.extent.height         := Vulkan_.Instans[0].Surfacs[0].PxSizeY;
     image_info.extent.depth          := 1;
     image_info.mipLevels             := 1;
     image_info.arrayLayers           := 1;
     image_info.samples               := NUM_SAMPLES;
     image_info.initialLayout         := VK_IMAGE_LAYOUT_UNDEFINED;
     image_info.queueFamilyIndexCount := 0;
     image_info.pQueueFamilyIndices   := nil;
     image_info.sharingMode           := VK_SHARING_MODE_EXCLUSIVE;
     image_info.usage                 := Ord( VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT );
     image_info.flags                 := 0;

     mem_alloc                 := Default( VkMemoryAllocateInfo );
     mem_alloc.sType           := VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO;
     mem_alloc.pNext           := nil;
     mem_alloc.allocationSize  := 0;
     mem_alloc.memoryTypeIndex := 0;

     view_info                                 := Default( VkImageViewCreateInfo );
     view_info.sType                           := VK_STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO;
     view_info.pNext                           := nil;
     view_info.image                           := VK_NULL_HANDLE;
     view_info.format                          := depth_format;
     view_info.components.r                    := VK_COMPONENT_SWIZZLE_R;
     view_info.components.g                    := VK_COMPONENT_SWIZZLE_G;
     view_info.components.b                    := VK_COMPONENT_SWIZZLE_B;
     view_info.components.a                    := VK_COMPONENT_SWIZZLE_A;
     view_info.subresourceRange.aspectMask     := Ord( VK_IMAGE_ASPECT_DEPTH_BIT );
     view_info.subresourceRange.baseMipLevel   := 0;
     view_info.subresourceRange.levelCount     := 1;
     view_info.subresourceRange.baseArrayLayer := 0;
     view_info.subresourceRange.layerCount     := 1;
     view_info.viewType                        := VK_IMAGE_VIEW_TYPE_2D;
     view_info.flags                           := 0;

     if ( depth_format = VK_FORMAT_D16_UNORM_S8_UINT ) or ( depth_format = VK_FORMAT_D24_UNORM_S8_UINT ) or
        ( depth_format = VK_FORMAT_D32_SFLOAT_S8_UINT )
     then view_info.subresourceRange.aspectMask := view_info.subresourceRange.aspectMask or Ord( VK_IMAGE_ASPECT_STENCIL_BIT );

     (* Create image *)
     res := vkCreateImage( Vulkan_.Instans[0].Devices[0].Handle, @image_info, nil, @Vulkan_.Info.depth.image );
     Assert( res = VK_SUCCESS );

     vkGetImageMemoryRequirements( Vulkan_.Instans[0].Devices[0].Handle, Vulkan_.Info.depth.image, @mem_reqs );

     mem_alloc.allocationSize := mem_reqs.size;
     (* Use the memory properties to determine the type of memory required *)
     pass := Vulkan_.Instans[0].Devices[0].memory_type_from_properties( mem_reqs.memoryTypeBits, Ord( VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT ), mem_alloc.memoryTypeIndex );
     Assert( pass );

     (* Allocate memory *)
     res := vkAllocateMemory( Vulkan_.Instans[0].Devices[0].Handle, @mem_alloc, nil, @Vulkan_.Info.depth.mem );
     Assert( res = VK_SUCCESS );

     (* Bind memory *)
     res := vkBindImageMemory( Vulkan_.Instans[0].Devices[0].Handle, Vulkan_.Info.depth.image, Vulkan_.Info.depth.mem, 0 );
     Assert( res = VK_SUCCESS );

     (* Create image view *)
     view_info.image := Vulkan_.Info.depth.image;
     res := vkCreateImageView( Vulkan_.Instans[0].Devices[0].Handle, @view_info, nil, @Vulkan_.Info.depth.view );
     Assert( res = VK_SUCCESS );
end;

procedure destroy_depth_buffer( const Vulkan_:TVulkan );
begin
     vkDestroyImageView( Vulkan_.Instans[0].Devices[0].Handle, Vulkan_.Info.depth.view, nil );
     vkDestroyImage( Vulkan_.Instans[0].Devices[0].Handle, Vulkan_.Info.depth.image, nil );
     vkFreeMemory( Vulkan_.Instans[0].Devices[0].Handle, Vulkan_.Info.depth.mem, nil );
end;

//////////////////////////////////////////////////////////////////////////////// 11-init_shaders

//////////////////////////////////////////////////////////////////////////////// 12-init_frame_buffers

procedure init_renderpass( const Vulkan_:TVulkan;
                           include_depth_:T_bool;
                           clear_:T_bool = True;
                           finalLayout_:VkImageLayout = VK_IMAGE_LAYOUT_PRESENT_SRC_KHR;
                           initialLayout_:VkImageLayout = VK_IMAGE_LAYOUT_UNDEFINED );
var
   res                :VkResult;
   attachments        :array [ 0..2-1 ] of VkAttachmentDescription;
   color_reference    :VkAttachmentReference;
   depth_reference    :VkAttachmentReference;
   subpass            :VkSubpassDescription;
   subpass_dependency :VkSubpassDependency;
   rp_info            :VkRenderPassCreateInfo;
begin
     (* DEPENDS on init_swap_chain() and init_depth_buffer() *)

     Assert( clear_ or ( initialLayout_ <> VK_IMAGE_LAYOUT_UNDEFINED ) );

     (* Need attachments for render target and depth buffer *)
     attachments[0].format         := Vulkan_.Instans[0].Devices[0].Format;
     attachments[0].samples        := NUM_SAMPLES;
     if clear_
     then attachments[0].loadOp    := VK_ATTACHMENT_LOAD_OP_CLEAR
     else attachments[0].loadOp    := VK_ATTACHMENT_LOAD_OP_LOAD;
     attachments[0].storeOp        := VK_ATTACHMENT_STORE_OP_STORE;
     attachments[0].stencilLoadOp  := VK_ATTACHMENT_LOAD_OP_DONT_CARE;
     attachments[0].stencilStoreOp := VK_ATTACHMENT_STORE_OP_DONT_CARE;
     attachments[0].initialLayout  := initialLayout_;
     attachments[0].finalLayout    := finalLayout_;
     attachments[0].flags          := 0;

     if include_depth_ then
     begin
          attachments[1].format         := Vulkan_.Info.depth.format;
          attachments[1].samples        := NUM_SAMPLES;
          if clear_
          then attachments[1].loadOp    := VK_ATTACHMENT_LOAD_OP_CLEAR
          else attachments[1].loadOp    := VK_ATTACHMENT_LOAD_OP_DONT_CARE;
          attachments[1].storeOp        := VK_ATTACHMENT_STORE_OP_STORE;
          attachments[1].stencilLoadOp  := VK_ATTACHMENT_LOAD_OP_DONT_CARE;
          attachments[1].stencilStoreOp := VK_ATTACHMENT_STORE_OP_STORE;
          attachments[1].initialLayout  := VK_IMAGE_LAYOUT_UNDEFINED;
          attachments[1].finalLayout    := VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL;
          attachments[1].flags          := 0;
     end;

     color_reference            := Default( VkAttachmentReference );
     color_reference.attachment := 0;
     color_reference.layout     := VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL;

     depth_reference            := Default( VkAttachmentReference );
     depth_reference.attachment := 1;
     depth_reference.layout     := VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL;

     subpass                              := Default( VkSubpassDescription );
     subpass.pipelineBindPoint            := VK_PIPELINE_BIND_POINT_GRAPHICS;
     subpass.flags                        := 0;
     subpass.inputAttachmentCount         := 0;
     subpass.pInputAttachments            := nil;
     subpass.colorAttachmentCount         := 1;
     subpass.pColorAttachments            := @color_reference;
     subpass.pResolveAttachments          := nil;
     if include_depth_
     then subpass.pDepthStencilAttachment := @depth_reference
     else subpass.pDepthStencilAttachment := nil;
     subpass.preserveAttachmentCount      := 0;
     subpass.pPreserveAttachments         := nil;

     // Subpass dependency to wait for wsi image acquired semaphore before starting layout transition
     subpass_dependency                 := Default( VkSubpassDependency );
     subpass_dependency.srcSubpass      := VK_SUBPASS_EXTERNAL;
     subpass_dependency.dstSubpass      := 0;
     subpass_dependency.srcStageMask    := Ord( VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT );
     subpass_dependency.dstStageMask    := Ord( VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT );
     subpass_dependency.srcAccessMask   := 0;
     subpass_dependency.dstAccessMask   := Ord( VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT );
     subpass_dependency.dependencyFlags := 0;

     rp_info                      := Default( VkRenderPassCreateInfo );
     rp_info.sType                := VK_STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO;
     rp_info.pNext                := nil;
     if include_depth_
     then rp_info.attachmentCount := 2
     else rp_info.attachmentCount := 1;
     rp_info.pAttachments         := @attachments[0];
     rp_info.subpassCount         := 1;
     rp_info.pSubpasses           := @subpass;
     rp_info.dependencyCount      := 1;
     rp_info.pDependencies        := @subpass_dependency;

     res := vkCreateRenderPass( Vulkan_.Instans[0].Devices[0].Handle, @rp_info, nil, @Vulkan_.Info.render_pass );
     Assert( res = VK_SUCCESS );
end;

procedure execute_queue_command_buffer( const Vulkan_:TVulkan );
type
    T_submit_info = array [ 0..1-1 ] of VkSubmitInfo;
var
   res              :VkResult;
   cmd_bufs         :array [ 0..1-1 ] of VkCommandBuffer;
   fenceInfo        :VkFenceCreateInfo;
   drawFence        :VkFence;
   pipe_stage_flags :VkPipelineStageFlags;
   submit_info      :T_submit_info;
begin
     (* Queue the command buffer for execution *)
     cmd_bufs[0] := Vulkan_.Instans[0].Devices[0].Poolers[0].Commans[0].Handle;
     fenceInfo.sType := VK_STRUCTURE_TYPE_FENCE_CREATE_INFO;
     fenceInfo.pNext := nil;
     fenceInfo.flags := 0;
     vkCreateFence( Vulkan_.Instans[0].Devices[0].Handle, @fenceInfo, nil, @drawFence );

     pipe_stage_flags := Ord( VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT );
     submit_info                         := Default( T_submit_info );
     submit_info[0].pNext                := nil;
     submit_info[0].sType                := VK_STRUCTURE_TYPE_SUBMIT_INFO;
     submit_info[0].waitSemaphoreCount   := 0;
     submit_info[0].pWaitSemaphores      := nil;
     submit_info[0].pWaitDstStageMask    := @pipe_stage_flags;
     submit_info[0].commandBufferCount   := 1;
     submit_info[0].pCommandBuffers      := @cmd_bufs[0];
     submit_info[0].signalSemaphoreCount := 0;
     submit_info[0].pSignalSemaphores    := nil;

     res := vkQueueSubmit( Vulkan_.Instans[0].Devices[0].QueuerG, 1, @submit_info[0], drawFence );
     Assert( res = VK_SUCCESS );

     repeat
           res := vkWaitForFences( Vulkan_.Instans[0].Devices[0].Handle, 1, @drawFence, VK_TRUE, FENCE_TIMEOUT );

     until res <> VK_TIMEOUT;
     Assert( res = VK_SUCCESS );

     vkDestroyFence( Vulkan_.Instans[0].Devices[0].Handle, drawFence, nil );
end;

procedure destroy_renderpass( const Vulkan_:TVulkan );
begin
     vkDestroyRenderPass( Vulkan_.Instans[0].Devices[0].Handle, Vulkan_.Info.render_pass, nil );
end;

//procedure destroy_command_buffer( const Vulkan_:TVulkan );
//var
//   cmd_bufs :array [ 0..1-1 ] of VkCommandBuffer;
//begin
//     cmd_bufs[0] := Vulkan_.Instance.Devices[0].ComPool.ComBufs.cmd;
//     vkFreeCommandBuffers( Vulkan_.Instance.Devices[0].Handle, Vulkan_.Instance.Devices[0].ComPool.cmd_pool, 1, @cmd_bufs[0] );
//end;

//procedure destroy_command_pool( const Vulkan_:TVulkan );
//begin
//     vkDestroyCommandPool( Vulkan_.Instance.Devices[0].Handle, Vulkan_.Instance.Devices[0].ComPool.ComBufs.cmd_pool, nil );
//end;

//////////////////////////////////////////////////////////////////////////////// 13-init_vertex_buffer

procedure init_framebuffers( const Vulkan_:TVulkan; include_depth:T_bool );
var
   res         :VkResult;
   attachments :array [ 0..2-1 ] of VkImageView;
   fb_info     :VkFramebufferCreateInfo;
   i           :T_uint32_t;
begin
     (* DEPENDS on init_depth_buffer(), init_renderpass() and
      * init_swapchain_extension() *)

     attachments[1] := Vulkan_.Info.depth.view;

     fb_info                      := Default( VkFramebufferCreateInfo );
     fb_info.sType                := VK_STRUCTURE_TYPE_FRAMEBUFFER_CREATE_INFO;
     fb_info.pNext                := nil;
     fb_info.renderPass           := Vulkan_.Info.render_pass;
     if include_depth
     then fb_info.attachmentCount := 2
     else fb_info.attachmentCount := 1;
     fb_info.pAttachments         := @attachments[0];
     fb_info.width                := Vulkan_.Instans[0].Surfacs[0].PxSizeX;
     fb_info.height               := Vulkan_.Instans[0].Surfacs[0].PxSizeY;
     fb_info.layers               := 1;

     SetLength( Vulkan_.Info.framebuffers, Vulkan_.Instans[0].Devices[0].Swapchs.Viewers.Count );

     for i := 0 to Vulkan_.Instans[0].Devices[0].Swapchs.Viewers.Count-1 do
     begin
          attachments[0] := Vulkan_.Instans[0].Devices[0].Swapchs.Viewers[i].Handle;
          res := vkCreateFramebuffer( Vulkan_.Instans[0].Devices[0].Handle, @fb_info, nil, @Vulkan_.Info.framebuffers[i] );
          Assert( res = VK_SUCCESS );
     end;
end;

procedure destroy_framebuffers( const Vulkan_:TVulkan );
var
   i :T_uint32_t;
begin
     for i := 0 to Vulkan_.Instans[0].Devices[0].Swapchs.Viewers.Count-1 do vkDestroyFramebuffer( Vulkan_.Instans[0].Devices[0].Handle, Vulkan_.Info.framebuffers[i], nil );
     Vulkan_.Info.framebuffers := nil;
end;

//////////////////////////////////////////////////////////////////////////////// 14-init_pipeline

procedure init_vertex_buffer( const Vulkan_:TVulkan; const vertexData_:P_void; dataSize_:T_uint32_t; dataStride_:T_uint32_t; use_texture_:T_bool );
var
   res        :VkResult;
   pass       :T_bool;
   buf_info   :VkBufferCreateInfo;
   mem_reqs   :VkMemoryRequirements;
   alloc_info :VkMemoryAllocateInfo;
   pData      :P_uint8_t;
begin
     buf_info                       := Default( VkBufferCreateInfo );
     buf_info.sType                 := VK_STRUCTURE_TYPE_BUFFER_CREATE_INFO;
     buf_info.pNext                 := nil;
     buf_info.usage                 := Ord( VK_BUFFER_USAGE_VERTEX_BUFFER_BIT );
     buf_info.size                  := dataSize_;
     buf_info.queueFamilyIndexCount := 0;
     buf_info.pQueueFamilyIndices   := nil;
     buf_info.sharingMode           := VK_SHARING_MODE_EXCLUSIVE;
     buf_info.flags                 := 0;
     res := vkCreateBuffer( Vulkan_.Instans[0].Devices[0].Handle, @buf_info, nil, @Vulkan_.Info.vertex_buffer.buf );
     Assert( res = VK_SUCCESS );

     vkGetBufferMemoryRequirements( Vulkan_.Instans[0].Devices[0].Handle, Vulkan_.Info.vertex_buffer.buf, @mem_reqs );

     alloc_info                 := Default( VkMemoryAllocateInfo );
     alloc_info.sType           := VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO;
     alloc_info.pNext           := nil;
     alloc_info.memoryTypeIndex := 0;

     alloc_info.allocationSize := mem_reqs.size;
     pass := Vulkan_.Instans[0].Devices[0].memory_type_from_properties( mem_reqs.memoryTypeBits,
                                          Ord( VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT ) or Ord( VK_MEMORY_PROPERTY_HOST_COHERENT_BIT ),
                                          alloc_info.memoryTypeIndex );
     Assert( pass, 'No mappable, coherent memory' );

     res := vkAllocateMemory( Vulkan_.Instans[0].Devices[0].Handle, @alloc_info, nil, @Vulkan_.Info.vertex_buffer.mem );
     Assert( res = VK_SUCCESS );
     Vulkan_.Info.vertex_buffer.buffer_info.range  := mem_reqs.size;
     Vulkan_.Info.vertex_buffer.buffer_info.offset := 0;

     res := vkMapMemory( Vulkan_.Instans[0].Devices[0].Handle, Vulkan_.Info.vertex_buffer.mem, 0, mem_reqs.size, 0, @pData );
     Assert( res = VK_SUCCESS );

     Move( vertexData_^, pData^, dataSize_ );

     vkUnmapMemory( Vulkan_.Instans[0].Devices[0].Handle, Vulkan_.Info.vertex_buffer.mem );

     res := vkBindBufferMemory( Vulkan_.Instans[0].Devices[0].Handle, Vulkan_.Info.vertex_buffer.buf, Vulkan_.Info.vertex_buffer.mem, 0 );
     Assert( res = VK_SUCCESS );

     Vulkan_.Info.vi_binding.binding   := 0;
     Vulkan_.Info.vi_binding.inputRate := VK_VERTEX_INPUT_RATE_VERTEX;
     Vulkan_.Info.vi_binding.stride    := dataStride_;

     Vulkan_.Info.vi_attribs[0].binding     := 0;
     Vulkan_.Info.vi_attribs[0].location    := 0;
     Vulkan_.Info.vi_attribs[0].format      := VK_FORMAT_R32G32B32A32_SFLOAT;
     Vulkan_.Info.vi_attribs[0].offset      := 0;
     Vulkan_.Info.vi_attribs[1].binding     := 0;
     Vulkan_.Info.vi_attribs[1].location    := 1;
     if use_texture_
     then Vulkan_.Info.vi_attribs[1].format := VK_FORMAT_R32G32_SFLOAT
     else Vulkan_.Info.vi_attribs[1].format := VK_FORMAT_R32G32B32A32_SFLOAT;
     Vulkan_.Info.vi_attribs[1].offset      := 16;
end;

procedure init_descriptor_pool( const Vulkan_:TVulkan; use_texture_:T_bool );
var
   res             :VkResult;
   type_count      :array [ 0..2-1 ] of VkDescriptorPoolSize;
   descriptor_pool :VkDescriptorPoolCreateInfo;
begin
     (* DEPENDS on init_uniform_buffer() and
      * init_descriptor_and_pipeline_layouts() *)

     type_count[0].type_           := VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER;
     type_count[0].descriptorCount := 1;
     if use_texture_ then
     begin
          type_count[1].type_           := VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER;
          type_count[1].descriptorCount := 1;
     end;

     descriptor_pool                    := Default( VkDescriptorPoolCreateInfo );
     descriptor_pool.sType              := VK_STRUCTURE_TYPE_DESCRIPTOR_POOL_CREATE_INFO;
     descriptor_pool.pNext              := nil;
     descriptor_pool.maxSets            := 1;
     if use_texture_
     then descriptor_pool.poolSizeCount := 2
     else descriptor_pool.poolSizeCount := 1;
     descriptor_pool.pPoolSizes         := @type_count[0];

     res := vkCreateDescriptorPool( Vulkan_.Instans[0].Devices[0].Handle, @descriptor_pool, nil, @Vulkan_.Info.desc_pool );
     Assert( res = VK_SUCCESS );
end;

procedure init_descriptor_set( const Vulkan_:TVulkan; use_texture_:T_bool );
var
   res        :VkResult;
   alloc_info :array [ 0..1-1 ] of VkDescriptorSetAllocateInfo;
   writes     :array [ 0..2-1 ] of VkWriteDescriptorSet;
begin
     (* DEPENDS on init_descriptor_pool() *)

     alloc_info[0].sType              := VK_STRUCTURE_TYPE_DESCRIPTOR_SET_ALLOCATE_INFO;
     alloc_info[0].pNext              := nil;
     alloc_info[0].descriptorPool     := Vulkan_.Info.desc_pool;
     alloc_info[0].descriptorSetCount := NUM_DESCRIPTOR_SETS;
     alloc_info[0].pSetLayouts        := @Vulkan_.Info.desc_layout[0];

     SetLength( Vulkan_.Info.desc_set, NUM_DESCRIPTOR_SETS );
     res := vkAllocateDescriptorSets( Vulkan_.Instans[0].Devices[0].Handle, @alloc_info[0], @Vulkan_.Info.desc_set[0] );
     Assert( res = VK_SUCCESS );

     writes[0]                 := Default( VkWriteDescriptorSet );
     writes[0].sType           := VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET;
     writes[0].pNext           := nil;
     writes[0].dstSet          := Vulkan_.Info.desc_set[0];
     writes[0].descriptorCount := 1;
     writes[0].descriptorType  := VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER;
     writes[0].pBufferInfo     := @Vulkan_.Instans[0].Devices[0].Buffers.Info;
     writes[0].dstArrayElement := 0;
     writes[0].dstBinding      := 0;

     if use_texture_ then
     begin
          writes[1]                 := Default( VkWriteDescriptorSet );
          writes[1].sType           := VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET;
          writes[1].dstSet          := Vulkan_.Info.desc_set[0];
          writes[1].dstBinding      := 1;
          writes[1].descriptorCount := 1;
          writes[1].descriptorType  := VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER;
          writes[1].pImageInfo      := @Vulkan_.Info.texture_data.image_info;
          writes[1].dstArrayElement := 0;
     end;

     if use_texture_
     then vkUpdateDescriptorSets( Vulkan_.Instans[0].Devices[0].Handle, 2, @writes[0], 0, nil )
     else vkUpdateDescriptorSets( Vulkan_.Instans[0].Devices[0].Handle, 1, @writes[0], 0, nil );
end;

procedure destroy_descriptor_pool( const Vulkan_:TVulkan );
begin
     vkDestroyDescriptorPool( Vulkan_.Instans[0].Devices[0].Handle, Vulkan_.Info.desc_pool, nil );
end;

procedure destroy_vertex_buffer( const Vulkan_:TVulkan );
begin
     vkDestroyBuffer( Vulkan_.Instans[0].Devices[0].Handle, Vulkan_.Info.vertex_buffer.buf, nil );
     vkFreeMemory( Vulkan_.Instans[0].Devices[0].Handle, Vulkan_.Info.vertex_buffer.mem, nil );
end;

//////////////////////////////////////////////////////////////////////////////// 15-draw_cube

procedure init_viewports( const Vulkan_:TVulkan );
begin
     Vulkan_.Info.viewport.height   := Vulkan_.Instans[0].Surfacs[0].PxSizeY;
     Vulkan_.Info.viewport.width    := Vulkan_.Instans[0].Surfacs[0].PxSizeX;
     Vulkan_.Info.viewport.minDepth := 0.0;
     Vulkan_.Info.viewport.maxDepth := 1.0;
     Vulkan_.Info.viewport.x        := 0;
     Vulkan_.Info.viewport.y        := 0;
     vkCmdSetViewport( Vulkan_.Instans[0].Devices[0].Poolers[0].Commans[0].Handle, 0, NUM_VIEWPORTS, @Vulkan_.Info.viewport );
end;

procedure init_scissors( const Vulkan_:TVulkan );
begin
     Vulkan_.Info.scissor.extent.width  := Vulkan_.Instans[0].Surfacs[0].PxSizeX;
     Vulkan_.Info.scissor.extent.height := Vulkan_.Instans[0].Surfacs[0].PxSizeY;
     Vulkan_.Info.scissor.offset.x      := 0;
     Vulkan_.Info.scissor.offset.y      := 0;
     vkCmdSetScissor( Vulkan_.Instans[0].Devices[0].Poolers[0].Commans[0].Handle, 0, NUM_SCISSORS, @Vulkan_.Info.scissor );
end;

procedure init_pipeline_cache( const Vulkan_:TVulkan );
var
   res :VkResult;
   pipelineCache :VkPipelineCacheCreateInfo;
begin
     pipelineCache.sType           := VK_STRUCTURE_TYPE_PIPELINE_CACHE_CREATE_INFO;
     pipelineCache.pNext           := nil;
     pipelineCache.initialDataSize := 0;
     pipelineCache.pInitialData    := nil;
     pipelineCache.flags           := 0;
     res := vkCreatePipelineCache( Vulkan_.Instans[0].Devices[0].Handle, @pipelineCache, nil, @Vulkan_.Info.pipelineCache );
     Assert( res = VK_SUCCESS );
end;

procedure destroy_pipeline_cache( const Vulkan_:TVulkan );
begin
     vkDestroyPipelineCache( Vulkan_.Instans[0].Devices[0].Handle, Vulkan_.Info.pipelineCache, nil );
end;

//////////////////////////////////////////////////////////////////////////////// draw_textured_cube

procedure init_buffer( const Vulkan_:TVulkan; var texObj_:T_texture_object );
var
   res                :VkResult;
   pass               :T_bool;
   buffer_create_info :VkBufferCreateInfo;
   mem_alloc          :VkMemoryAllocateInfo;
   mem_reqs           :VkMemoryRequirements;
   requirements       :VkFlags;
begin
     buffer_create_info                       := Default( VkBufferCreateInfo );
     buffer_create_info.sType                 := VK_STRUCTURE_TYPE_BUFFER_CREATE_INFO;
     buffer_create_info.pNext                 := nil;
     buffer_create_info.flags                 := 0;
     buffer_create_info.size                  := texObj_.tex_width * texObj_.tex_height * 4;
     buffer_create_info.usage                 := Ord( VK_BUFFER_USAGE_TRANSFER_SRC_BIT );
     buffer_create_info.sharingMode           := VK_SHARING_MODE_EXCLUSIVE;
     buffer_create_info.queueFamilyIndexCount := 0;
     buffer_create_info.pQueueFamilyIndices   := nil;
     res := vkCreateBuffer( Vulkan_.Instans[0].Devices[0].Handle, @buffer_create_info, nil, @texObj_.buffer );
     Assert( res = VK_SUCCESS );

     mem_alloc                 := Default( VkMemoryAllocateInfo );
     mem_alloc.sType           := VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO;
     mem_alloc.pNext           := nil;
     mem_alloc.allocationSize  := 0;
     mem_alloc.memoryTypeIndex := 0;

     vkGetBufferMemoryRequirements( Vulkan_.Instans[0].Devices[0].Handle, texObj_.buffer, @mem_reqs );
     mem_alloc.allocationSize := mem_reqs.size;
     texObj_.buffer_size := mem_reqs.size;

     requirements := Ord( VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT ) or Ord( VK_MEMORY_PROPERTY_HOST_COHERENT_BIT );
     pass := Vulkan_.Instans[0].Devices[0].memory_type_from_properties( mem_reqs.memoryTypeBits, requirements, mem_alloc.memoryTypeIndex );
     Assert( pass, '"No mappable, coherent memory' );

     (* allocate memory *)
     res := vkAllocateMemory(Vulkan_.Instans[0].Devices[0].Handle, @mem_alloc, nil, @( texObj_.buffer_memory) );
     Assert( res = VK_SUCCESS );

     (* bind memory *)
     res := vkBindBufferMemory( Vulkan_.Instans[0].Devices[0].Handle, texObj_.buffer, texObj_.buffer_memory, 0 );
     Assert( res = VK_SUCCESS );
end;

procedure init_image( const Vulkan_:TVulkan; var texObj_:T_texture_object; const textureName_:String; extraUsages_:VkImageUsageFlags; extraFeatures_:VkFormatFeatureFlags );
var
   res               :VkResult;
   pass              :T_bool;
   filename          :String;
   formatProps       :VkFormatProperties;
   allFeatures       :VkFormatFeatureFlags;
   image_create_info :VkImageCreateInfo;
   mem_alloc         :VkMemoryAllocateInfo;
   mem_reqs          :VkMemoryRequirements;
   requirements      :VkFlags;
   cmd_bufs          :array [ 0..1-1 ] of VkCommandBuffer;
   fenceInfo         :VkFenceCreateInfo;
   cmdFence          :VkFence;
   submit_info       :array [ 0..1-1 ] of VkSubmitInfo;
   subres            :VkImageSubresource;
   layout            :VkSubresourceLayout;
   data              :P_void;
   rowPitch          :T_uint64_t;
   copy_region       :VkBufferImageCopy;
   view_info         :VkImageViewCreateInfo;
begin
     filename := '../../_DATA/';

     if textureName_ = '' then filename := filename + 'lunarg.ppm'
                         else filename := filename + textureName_;

     if not read_ppm( filename, texObj_.tex_width, texObj_.tex_height, 0, nil ) then
     begin
          Log.d( 'Try relative path' );
          filename := '../../_DATA/';
          if textureName_ ='' then filename := filename + 'lunarg.ppm'
                             else filename := filename + textureName_;
          if not read_ppm( filename, texObj_.tex_width, texObj_.tex_height, 0, nil ) then
          begin
               Log.d( 'Could not read texture file ' + filename );
               RunError( 256-1 );
          end;
     end;

     vkGetPhysicalDeviceFormatProperties( Vulkan_.Instans[0].Devices[0].Physic, VK_FORMAT_R8G8B8A8_UNORM, @formatProps );

     (* See if we can use a linear tiled image for a texture, if not, we will
      * need a staging buffer for the texture data *)
     allFeatures := Ord( VK_FORMAT_FEATURE_SAMPLED_IMAGE_BIT ) or extraFeatures_;
     texObj_.needs_staging := ( ( formatProps.linearTilingFeatures and allFeatures ) <> allFeatures );

     if texObj_.needs_staging then
     begin
          Assert( ( formatProps.optimalTilingFeatures and allFeatures ) = allFeatures );
          init_buffer( Vulkan_, texObj_ );
          extraUsages_ := extraUsages_ or Ord( VK_IMAGE_USAGE_TRANSFER_DST_BIT );
     end
     else
     begin
          texObj_.buffer        := VK_NULL_HANDLE;
          texObj_.buffer_memory := VK_NULL_HANDLE;
     end;

     image_create_info                       := Default( VkImageCreateInfo );
     image_create_info.sType                 := VK_STRUCTURE_TYPE_IMAGE_CREATE_INFO;
     image_create_info.pNext                 := nil;
     image_create_info.imageType             := VK_IMAGE_TYPE_2D;
     image_create_info.format                := VK_FORMAT_R8G8B8A8_UNORM;
     image_create_info.extent.width          := texObj_.tex_width;
     image_create_info.extent.height         := texObj_.tex_height;
     image_create_info.extent.depth          := 1;
     image_create_info.mipLevels             := 1;
     image_create_info.arrayLayers           := 1;
     image_create_info.samples               := NUM_SAMPLES;
     if texObj_.needs_staging
     then image_create_info.tiling           := VK_IMAGE_TILING_OPTIMAL
     else image_create_info.tiling           := VK_IMAGE_TILING_LINEAR;
     if texObj_.needs_staging
     then image_create_info.initialLayout    := VK_IMAGE_LAYOUT_UNDEFINED
     else image_create_info.initialLayout    := VK_IMAGE_LAYOUT_PREINITIALIZED;
     image_create_info.usage                 := Ord( VK_IMAGE_USAGE_SAMPLED_BIT ) or extraUsages_;
     image_create_info.queueFamilyIndexCount := 0;
     image_create_info.pQueueFamilyIndices   := nil;
     image_create_info.sharingMode           := VK_SHARING_MODE_EXCLUSIVE;
     image_create_info.flags                 := 0;

     mem_alloc                 := Default( VkMemoryAllocateInfo );
     mem_alloc.sType           := VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO;
     mem_alloc.pNext           := nil;
     mem_alloc.allocationSize  := 0;
     mem_alloc.memoryTypeIndex := 0;

     res := vkCreateImage( Vulkan_.Instans[0].Devices[0].Handle, @image_create_info, nil, @texObj_.image );
     Assert( res = VK_SUCCESS );

     vkGetImageMemoryRequirements( Vulkan_.Instans[0].Devices[0].Handle, texObj_.image, @mem_reqs );

     mem_alloc.allocationSize := mem_reqs.size;

     if texObj_.needs_staging
     then requirements := 0
     else requirements := Ord( VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT ) or Ord( VK_MEMORY_PROPERTY_HOST_COHERENT_BIT );
     pass := Vulkan_.Instans[0].Devices[0].memory_type_from_properties( mem_reqs.memoryTypeBits, requirements, mem_alloc.memoryTypeIndex );
     Assert( pass );

     (* allocate memory *)
     res := vkAllocateMemory(Vulkan_.Instans[0].Devices[0].Handle, @mem_alloc, nil, @( texObj_.image_memory) );
     Assert( res = VK_SUCCESS );

     (* bind memory *)
     res := vkBindImageMemory( Vulkan_.Instans[0].Devices[0].Handle, texObj_.image, texObj_.image_memory, 0 );
     Assert( res = VK_SUCCESS );

     Vulkan_.Instans[0].Devices[0].Poolers[0].Commans[0].EndRecord;
     cmd_bufs[0] := Vulkan_.Instans[0].Devices[0].Poolers[0].Commans[0].Handle;
     fenceInfo.sType := VK_STRUCTURE_TYPE_FENCE_CREATE_INFO;
     fenceInfo.pNext := nil;
     fenceInfo.flags := 0;
     vkCreateFence( Vulkan_.Instans[0].Devices[0].Handle, @fenceInfo, nil, @cmdFence );

     submit_info[0]                      := Default( VkSubmitInfo );
     submit_info[0].pNext                := nil;
     submit_info[0].sType                := VK_STRUCTURE_TYPE_SUBMIT_INFO;
     submit_info[0].waitSemaphoreCount   := 0;
     submit_info[0].pWaitSemaphores      := nil;
     submit_info[0].pWaitDstStageMask    := nil;
     submit_info[0].commandBufferCount   := 1;
     submit_info[0].pCommandBuffers      := @cmd_bufs[0];
     submit_info[0].signalSemaphoreCount := 0;
     submit_info[0].pSignalSemaphores    := nil;

     (* Queue the command buffer for execution *)
     res := vkQueueSubmit( Vulkan_.Instans[0].Devices[0].QueuerG, 1, @submit_info[0], cmdFence );
     Assert( res = VK_SUCCESS );

     subres            := Default( VkImageSubresource );
     subres.aspectMask := Ord( VK_IMAGE_ASPECT_COLOR_BIT );
     subres.mipLevel   := 0;
     subres.arrayLayer := 0;

     layout := Default( VkSubresourceLayout );
     if not texObj_.needs_staging then
     begin
          (* Get the subresource layout so we know what the row pitch is *)
          vkGetImageSubresourceLayout( Vulkan_.Instans[0].Devices[0].Handle, texObj_.image, @subres, @layout );
     end;

     (* Make sure command buffer is finished before mapping *)
     repeat
           res := vkWaitForFences( Vulkan_.Instans[0].Devices[0].Handle, 1, @cmdFence, VK_TRUE, FENCE_TIMEOUT );

     until res <> VK_TIMEOUT;
     Assert( res = VK_SUCCESS );

     vkDestroyFence( Vulkan_.Instans[0].Devices[0].Handle, cmdFence, nil );

     if texObj_.needs_staging
     then res := vkMapMemory( Vulkan_.Instans[0].Devices[0].Handle, texObj_.buffer_memory, 0, texObj_.buffer_size, 0, @data )
     else res := vkMapMemory( Vulkan_.Instans[0].Devices[0].Handle, texObj_.image_memory , 0, mem_reqs.size     , 0, @data );
     Assert( res = VK_SUCCESS );

     (* Read the ppm file into the mappable image's memory *)
     if texObj_.needs_staging then rowPitch := texObj_.tex_width * 4
                             else rowPitch := layout.rowPitch;
     if not read_ppm( filename, texObj_.tex_width, texObj_.tex_height, rowPitch, data ) then
     begin
          Log.d( 'Could not load texture file lunarg.ppm' );
          RunError( 256-1 );
     end;

     if texObj_.needs_staging
     then vkUnmapMemory( Vulkan_.Instans[0].Devices[0].Handle, texObj_.buffer_memory )
     else vkUnmapMemory( Vulkan_.Instans[0].Devices[0].Handle, texObj_.image_memory  );

     res := vkResetCommandBuffer( Vulkan_.Instans[0].Devices[0].Poolers[0].Commans[0].Handle, 0 );
     Assert( res = VK_SUCCESS );
     Vulkan_.Instans[0].Devices[0].Poolers[0].Commans[0].BeginRecord;

     if not texObj_.needs_staging then
     begin
          (* If we can use the linear tiled image as a texture, just do it *)
          texObj_.imageLayout := VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL;
          set_image_layout( Vulkan_, texObj_.image, Ord( VK_IMAGE_ASPECT_COLOR_BIT ), VK_IMAGE_LAYOUT_PREINITIALIZED, texObj_.imageLayout,
                            Ord( VK_PIPELINE_STAGE_HOST_BIT ), Ord( VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT ) );
     end
     else
     begin
          (* Since we're going to blit to the texture image, set its layout to
           * DESTINATION_OPTIMAL *)
          set_image_layout( Vulkan_, texObj_.image, Ord( VK_IMAGE_ASPECT_COLOR_BIT ), VK_IMAGE_LAYOUT_UNDEFINED,
                            VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL, Ord( VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT ), Ord( VK_PIPELINE_STAGE_TRANSFER_BIT ) );

          copy_region.bufferOffset                    := 0;
          copy_region.bufferRowLength                 := texObj_.tex_width;
          copy_region.bufferImageHeight               := texObj_.tex_height;
          copy_region.imageSubresource.aspectMask     := Ord( VK_IMAGE_ASPECT_COLOR_BIT );
          copy_region.imageSubresource.mipLevel       := 0;
          copy_region.imageSubresource.baseArrayLayer := 0;
          copy_region.imageSubresource.layerCount     := 1;
          copy_region.imageOffset.x                   := 0;
          copy_region.imageOffset.y                   := 0;
          copy_region.imageOffset.z                   := 0;
          copy_region.imageExtent.width               := texObj_.tex_width;
          copy_region.imageExtent.height              := texObj_.tex_height;
          copy_region.imageExtent.depth               := 1;

          (* Put the copy command into the command buffer *)
          vkCmdCopyBufferToImage( Vulkan_.Instans[0].Devices[0].Poolers[0].Commans[0].Handle, texObj_.buffer, texObj_.image, VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL, 1, @copy_region );

          (* Set the layout for the texture image from DESTINATION_OPTIMAL to
           * SHADER_READ_ONLY *)
          texObj_.imageLayout := VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL;
          set_image_layout( Vulkan_, texObj_.image, Ord( VK_IMAGE_ASPECT_COLOR_BIT ), VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL, texObj_.imageLayout,
                            Ord( VK_PIPELINE_STAGE_TRANSFER_BIT ), Ord( VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT ) );
     end;

     view_info                                 := Default( VkImageViewCreateInfo );
     view_info.sType                           := VK_STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO;
     view_info.pNext                           := nil;
     view_info.image                           := VK_NULL_HANDLE;
     view_info.viewType                        := VK_IMAGE_VIEW_TYPE_2D;
     view_info.format                          := VK_FORMAT_R8G8B8A8_UNORM;
     view_info.components.r                    := VK_COMPONENT_SWIZZLE_R;
     view_info.components.g                    := VK_COMPONENT_SWIZZLE_G;
     view_info.components.b                    := VK_COMPONENT_SWIZZLE_B;
     view_info.components.a                    := VK_COMPONENT_SWIZZLE_A;
     view_info.subresourceRange.aspectMask     := Ord( VK_IMAGE_ASPECT_COLOR_BIT );
     view_info.subresourceRange.baseMipLevel   := 0;
     view_info.subresourceRange.levelCount     := 1;
     view_info.subresourceRange.baseArrayLayer := 0;
     view_info.subresourceRange.layerCount     := 1;

     (* create image view *)
     view_info.image := texObj_.image;
     res := vkCreateImageView( Vulkan_.Instans[0].Devices[0].Handle, @view_info, nil, @texObj_.view );
     Assert( res = VK_SUCCESS );
end;

procedure init_sampler( const Vulkan_:TVulkan; var sampler_:VkSampler );
var
   res               :VkResult;
   samplerCreateInfo :VkSamplerCreateInfo;
begin
     samplerCreateInfo                  := Default( VkSamplerCreateInfo );
     samplerCreateInfo.sType            := VK_STRUCTURE_TYPE_SAMPLER_CREATE_INFO;
     samplerCreateInfo.magFilter        := VK_FILTER_NEAREST;
     samplerCreateInfo.minFilter        := VK_FILTER_NEAREST;
     samplerCreateInfo.mipmapMode       := VK_SAMPLER_MIPMAP_MODE_NEAREST;
     samplerCreateInfo.addressModeU     := VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE;
     samplerCreateInfo.addressModeV     := VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE;
     samplerCreateInfo.addressModeW     := VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE;
     samplerCreateInfo.mipLodBias       := 0.0;
     samplerCreateInfo.anisotropyEnable := VK_FALSE;
     samplerCreateInfo.maxAnisotropy    := 1;
     samplerCreateInfo.compareOp        := VK_COMPARE_OP_NEVER;
     samplerCreateInfo.minLod           := 0.0;
     samplerCreateInfo.maxLod           := 0.0;
     samplerCreateInfo.compareEnable    := VK_FALSE;
     samplerCreateInfo.borderColor      := VK_BORDER_COLOR_FLOAT_OPAQUE_WHITE;

     (* create sampler *)
     res := vkCreateSampler( Vulkan_.Instans[0].Devices[0].Handle, @samplerCreateInfo, nil, @sampler_ );
     Assert( res = VK_SUCCESS );
end;

procedure init_texture( const Vulkan_:TVulkan; const textureName_:String = ''; extraUsages_:VkImageUsageFlags = 0; extraFeatures_:VkFormatFeatureFlags = 0 );
var
   texObj :T_texture_object;
begin
     (* create image *)
     init_image( Vulkan_, texObj, textureName_, extraUsages_, extraFeatures_ );

     (* create sampler *)
     init_sampler( Vulkan_, texObj.sampler );

     Vulkan_.Info.textures := Vulkan_.Info.textures + [ texObj ];

     (* track a description of the texture *)
     Vulkan_.Info.texture_data.image_info.imageView   := Vulkan_.Info.textures[ High( Vulkan_.Info.textures ) ].view;
     Vulkan_.Info.texture_data.image_info.sampler     := Vulkan_.Info.textures[ High( Vulkan_.Info.textures ) ].sampler;
     Vulkan_.Info.texture_data.image_info.imageLayout := VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL;
end;

procedure destroy_textures( const Vulkan_:TVulkan );
var
   i :T_size_t;
begin
     for i := 0 to Length( Vulkan_.Info.textures )-1 do
     begin
          vkDestroySampler  ( Vulkan_.Instans[0].Devices[0].Handle, Vulkan_.Info.textures[i].sampler      , nil );
          vkDestroyImageView( Vulkan_.Instans[0].Devices[0].Handle, Vulkan_.Info.textures[i].view         , nil );
          vkDestroyImage    ( Vulkan_.Instans[0].Devices[0].Handle, Vulkan_.Info.textures[i].image        , nil );
          vkFreeMemory      ( Vulkan_.Instans[0].Devices[0].Handle, Vulkan_.Info.textures[i].image_memory , nil );
          vkDestroyBuffer   ( Vulkan_.Instans[0].Devices[0].Handle, Vulkan_.Info.textures[i].buffer       , nil );
          vkFreeMemory      ( Vulkan_.Instans[0].Devices[0].Handle, Vulkan_.Info.textures[i].buffer_memory, nil );
     end;
end;

end. //######################################################################### ■