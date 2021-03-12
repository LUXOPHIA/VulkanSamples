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
          attachments[1].format         := Vulkan_.Instans[0].Devices[0].Depthr.Inform.format;
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

     attachments[1] := Vulkan_.Instans[0].Devices[0].Depthr.Viewer.Handle;

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

     SetLength( Vulkan_.Info.framebuffers, Vulkan_.Instans[0].Devices[0].Swapchs[0].Framers.Count );

     for i := 0 to Vulkan_.Instans[0].Devices[0].Swapchs[0].Framers.Count-1 do
     begin
          attachments[0] := Vulkan_.Instans[0].Devices[0].Swapchs[0].Framers[i].Handle;
          res := vkCreateFramebuffer( Vulkan_.Instans[0].Devices[0].Handle, @fb_info, nil, @Vulkan_.Info.framebuffers[i] );
          Assert( res = VK_SUCCESS );
     end;
end;

procedure destroy_framebuffers( const Vulkan_:TVulkan );
var
   i :T_uint32_t;
begin
     for i := 0 to Vulkan_.Instans[0].Devices[0].Swapchs[0].Framers.Count-1 do vkDestroyFramebuffer( Vulkan_.Instans[0].Devices[0].Handle, Vulkan_.Info.framebuffers[i], nil );
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
     writes[0].pBufferInfo     := @Vulkan_.Instans[0].Devices[0].Buffers[0].Descri;
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
          writes[1].pImageInfo      := Vulkan_.Instans[0].Devices[0].Texturs[0].Handle;
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

end. //######################################################################### ■