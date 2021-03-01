unit vulkan.util;

(*
 * Vulkan Samples
 *
 * Copyright (C) 2015-2016 Valve Corporation
 * Copyright (C) 2015-2016 LunarG, Inc.
 * Copyright (C) 2015-2016 Google, Inc.
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

uses vulkan_core,
     LUX.Code.C,
     LUX.D1, LUX.D2, LUX.D2x2, LUX.D3, LUX.D3x3, LUX.D4x4;

type //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【型】

     //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【レコード】

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% T_texture_object
     (*
      * structure to track all objects related to a texture.
      *)
     T_texture_object = record
       sampler       :VkSampler;

       image         :VkImage;
       imageLayout   :VkImageLayout;

       needs_staging :T_bool;
       buffer        :VkBuffer;
       buffer_size   :VkDeviceSize;

       image_memory  :VkDeviceMemory;
       buffer_memory :VkDeviceMemory;
       view          :VkImageView;
       tex_width     :T_int32_t;
       tex_height    :T_int32_t;
     end;

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% T_swap_chain_buffer
     (*
      * Keep each of our swap chain buffers' image, command buffer and view in one
      * spot
      *)
     T_swap_chain_buffer = record
       image :VkImage;
       view  :VkImageView;
     end;

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% T_layer_properties
     (*
      * A layer can expose extensions, keep track of those
      * extensions here.
      *)
     T_layer_properties = record
       properties          :VkLayerProperties;
       instance_extensions :TArray<VkExtensionProperties>;
       device_extensions   :TArray<VkExtensionProperties>;
     end;

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% T_sample_info
     (*
      * Structure for tracking information used / created / modified
      * by utility functions.
      *)
     P_sample_info = ^T_sample_info;
     T_sample_info = record
     private
       const APP_NAME_STR_LEN = 80;
     public
       connection :T_HINSTANCE;                                // hInstance - Windows Instance
       name       :array [ 0..APP_NAME_STR_LEN-1 ] of T_char;  // Name to put on the window/icon
       window     :T_HWND;                                     // hWnd - window handle
       surface            :VkSurfaceKHR;
       prepared           :T_bool;
       use_staging_buffer :T_bool;
       save_images        :T_bool;

       instance_extension_names      :TArray<P_char>;
       instance_layer_properties     :TArray<T_layer_properties>;

       device_extension_names      :TArray<P_char>;
       device_extension_properties :TArray<VkExtensionProperties>;
       device                      :VkDevice;
       graphics_queue              :VkQueue;
       present_queue               :VkQueue;
       graphics_queue_family_index :T_uint32_t;
       present_queue_family_index  :T_uint32_t;
       gpu_props                   :VkPhysicalDeviceProperties;
       queue_props                 :TArray<VkQueueFamilyProperties>;
       memory_properties           :VkPhysicalDeviceMemoryProperties;

       framebuffers  :TArray<VkFramebuffer>;
       width         :T_int;
       height        :T_int;
       format        :VkFormat;

       swapchainImageCount    :T_uint32_t;
       swap_chain             :VkSwapchainKHR;
       buffers                :TArray<T_swap_chain_buffer>;
       imageAcquiredSemaphore :VkSemaphore;

       cmd_pool :VkCommandPool;

       depth :record
                format :VkFormat;
                image  :VkImage;
                mem    :VkDeviceMemory;
                view   :VkImageView;
              end;

       textures :TArray<T_texture_object>;

       uniform_data :record
                      buf         :VkBuffer;
                      mem         :VkDeviceMemory;
                      buffer_info :VkDescriptorBufferInfo;
                    end;

       texture_data :record
                       image_info :VkDescriptorImageInfo;
                     end;

       vertex_buffer :record
                        buf         :VkBuffer;
                        mem         :VkDeviceMemory;
                        buffer_info :VkDescriptorBufferInfo;
                      end;

       vi_binding    :VkVertexInputBindingDescription;
       vi_attribs    :array [ 0..2-1 ] of VkVertexInputAttributeDescription;

       Projection :TSingleM4;
       View       :TSingleM4;
       Model      :TSingleM4;
       Clip       :TSingleM4;
       MVP        :TSingleM4;

       cmd             :VkCommandBuffer; // Buffer for initialization commands
       pipeline_layout :VkPipelineLayout;
       desc_layout     :TArray<VkDescriptorSetLayout>;
       pipelineCache   :VkPipelineCache;
       render_pass     :VkRenderPass;

       desc_pool :VkDescriptorPool;
       desc_set  :TArray<VkDescriptorSet>;

       dbgCreateDebugReportCallback  :PFN_vkCreateDebugReportCallbackEXT;
       dbgDestroyDebugReportCallback :PFN_vkDestroyDebugReportCallbackEXT;
       dbgBreakCallback              :PFN_vkDebugReportMessageEXT;
       debug_report_callbacks        :TArray<VkDebugReportCallbackEXT>;

       current_buffer     :T_uint32_t;
       queue_family_count :T_uint32_t;

       viewport :VkViewport;
       scissor  :VkRect2D;
     end;

     //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【クラス】

const //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【定数】

     (* Number of samples needs to be the same at image creation,      *)
     (* renderpass creation and pipeline creation.                     *)
     NUM_SAMPLES = VK_SAMPLE_COUNT_1_BIT;

     (* Number of descriptor sets needs to be the same at alloc,       *)
     (* pipeline layout creation, and descriptor set layout creation   *)
     NUM_DESCRIPTOR_SETS = 1;

     (* Amount of time, in nanoseconds, to wait for a command buffer to complete *)
     FENCE_TIMEOUT = 100000000;

     (* Number of viewports and number of scissors have to be the same *)
     (* at pipeline creation and in any call to set them dynamically   *)
     (* They also have to be the same as each other                    *)
     NUM_VIEWPORTS = 1;
     NUM_SCISSORS  = NUM_VIEWPORTS;

//var //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【変数】

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【ルーチン】

function memory_type_from_properties( var info:T_sample_info; typeBits:T_uint32_t; requirements_mask:VkFlags; var typeIndex:T_uint32_t ) :T_bool;

//////////////////////////////////////////////////////////////////////////////// 15-draw_cube

procedure wait_seconds( seconds_:T_int );
procedure set_image_layout( var info_:T_sample_info; image_:VkImage; aspectMask_:VkImageAspectFlags; old_image_layout_:VkImageLayout;
                            new_image_layout_:VkImageLayout; src_stages_:VkPipelineStageFlags; dest_stages_:VkPipelineStageFlags );
procedure write_ppm( var info_:T_sample_info; const basename_:String );

//////////////////////////////////////////////////////////////////////////////// draw_textured_cube

function read_ppm( const filename_:String; var width_:T_int; var height_:T_int; rowPitch_:T_uint64_t; dataPtr_:P_unsigned_char ) :T_bool;

implementation //############################################################### ■

uses System.SysUtils, System.Classes, 
     FMX.Types;

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【レコード】

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【クラス】

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【ルーチン】

function memory_type_from_properties( var info:T_sample_info; typeBits:T_uint32_t; requirements_mask:VkFlags; var typeIndex:T_uint32_t ) :T_bool;
var
   i :T_uint32_t;
begin
     // Search memtypes to find first index with those properties
     for i := 0 to info.memory_properties.memoryTypeCount-1 do
     begin
          if ( typeBits and 1 ) = 1 then
          begin
               // Type is available, does it match user properties?
               if info.memory_properties.memoryTypes[i].propertyFlags and requirements_mask = requirements_mask then
               begin
                    typeIndex := i;
                    Exit( True );
               end;
          end;
          typeBits := typeBits shr 1;
     end;
     // No memory types matched, return failure
     Result := False;
end;

//////////////////////////////////////////////////////////////////////////////// 15-draw_cube

procedure wait_seconds( seconds_:T_int );
begin
     Sleep( seconds_ * 1000 );
end;

procedure set_image_layout( var info_:T_sample_info; image_:VkImage; aspectMask_:VkImageAspectFlags; old_image_layout_:VkImageLayout;
                            new_image_layout_:VkImageLayout; src_stages_:VkPipelineStageFlags; dest_stages_:VkPipelineStageFlags );
var
   image_memory_barrier :VkImageMemoryBarrier;
begin
     (* DEPENDS on info.cmd and info.queue initialized *)

     Assert( NativeInt( info_.cmd            ) <> VK_NULL_HANDLE );
     Assert( NativeInt( info_.graphics_queue ) <> VK_NULL_HANDLE );

     image_memory_barrier.sType                           := VK_STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER;
     image_memory_barrier.pNext                           := nil;
     image_memory_barrier.srcAccessMask                   := 0;
     image_memory_barrier.dstAccessMask                   := 0;
     image_memory_barrier.oldLayout                       := old_image_layout_;
     image_memory_barrier.newLayout                       := new_image_layout_;
     image_memory_barrier.srcQueueFamilyIndex             := VK_QUEUE_FAMILY_IGNORED;
     image_memory_barrier.dstQueueFamilyIndex             := VK_QUEUE_FAMILY_IGNORED;
     image_memory_barrier.image                           := image_;
     image_memory_barrier.subresourceRange.aspectMask     := aspectMask_;
     image_memory_barrier.subresourceRange.baseMipLevel   := 0;
     image_memory_barrier.subresourceRange.levelCount     := 1;
     image_memory_barrier.subresourceRange.baseArrayLayer := 0;
     image_memory_barrier.subresourceRange.layerCount     := 1;

     case old_image_layout_ of
       VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL:
          image_memory_barrier.srcAccessMask := Ord( VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT );

       VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL:
          image_memory_barrier.srcAccessMask := Ord( VK_ACCESS_TRANSFER_WRITE_BIT         );

       VK_IMAGE_LAYOUT_PREINITIALIZED:
          image_memory_barrier.srcAccessMask := Ord( VK_ACCESS_HOST_WRITE_BIT             );
     end;

     case new_image_layout_ of
       VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL:
          image_memory_barrier.dstAccessMask := Ord( VK_ACCESS_TRANSFER_WRITE_BIT                 );

       VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL:
          image_memory_barrier.dstAccessMask := Ord( VK_ACCESS_TRANSFER_READ_BIT                  );

       VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL:
          image_memory_barrier.dstAccessMask := Ord( VK_ACCESS_SHADER_READ_BIT                    );

       VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL:
          image_memory_barrier.dstAccessMask := Ord( VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT         );

       VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL:
          image_memory_barrier.dstAccessMask := Ord( VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT );
     end;

     vkCmdPipelineBarrier( info_.cmd, src_stages_, dest_stages_, 0, 0, nil, 0, nil, 1, @image_memory_barrier );
end;

procedure write_ppm( var info_:T_sample_info; const basename_:String );
var
   filename          :String;
   x, y              :T_int;
   res               :VkResult;
   image_create_info :VkImageCreateInfo;
   mem_alloc         :VkMemoryAllocateInfo;
   mappableImage     :VkImage;
   mappableMemory    :VkDeviceMemory;
   mem_reqs          :VkMemoryRequirements;
   pass              :T_bool;
   cmd_buf_info      :VkCommandBufferBeginInfo;
   copy_region       :VkImageCopy;
   cmd_bufs          :array [ 0..1-1 ] of VkCommandBuffer;
   fenceInfo         :VkFenceCreateInfo;
   cmdFence          :VkFence;
   submit_info       :array [ 0..1-1 ] of VkSubmitInfo;
   subres            :VkImageSubresource;
   sr_layout         :VkSubresourceLayout;
   ptr               :P_char;
   F                 :TFileStream;
   S                 :String;
   row               :P_uint32_t;
   swapped           :T_uint32_t;
begin
     image_create_info.sType                 := VK_STRUCTURE_TYPE_IMAGE_CREATE_INFO;
     image_create_info.pNext                 := nil;
     image_create_info.imageType             := VK_IMAGE_TYPE_2D;
     image_create_info.format                := info_.format;
     image_create_info.extent.width          := info_.width;
     image_create_info.extent.height         := info_.height;
     image_create_info.extent.depth          := 1;
     image_create_info.mipLevels             := 1;
     image_create_info.arrayLayers           := 1;
     image_create_info.samples               := VK_SAMPLE_COUNT_1_BIT;
     image_create_info.tiling                := VK_IMAGE_TILING_LINEAR;
     image_create_info.initialLayout         := VK_IMAGE_LAYOUT_UNDEFINED;
     image_create_info.usage                 := Ord( VK_IMAGE_USAGE_TRANSFER_DST_BIT );
     image_create_info.queueFamilyIndexCount := 0;
     image_create_info.pQueueFamilyIndices   := nil;
     image_create_info.sharingMode           := VK_SHARING_MODE_EXCLUSIVE;
     image_create_info.flags                 := 0;

     mem_alloc.sType           := VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO;
     mem_alloc.pNext           := nil;
     mem_alloc.allocationSize  := 0;
     mem_alloc.memoryTypeIndex := 0;

     (* Create a mappable image *)
     res := vkCreateImage( info_.device, @image_create_info, nil, @mappableImage );
     Assert( res = VK_SUCCESS );

     vkGetImageMemoryRequirements( info_.device, mappableImage, @mem_reqs );

     mem_alloc.allocationSize := mem_reqs.size;

     (* Find the memory type that is host mappable *)
     pass := memory_type_from_properties(
                  info_, mem_reqs.memoryTypeBits, Ord( VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT ) or Ord( VK_MEMORY_PROPERTY_HOST_COHERENT_BIT ),
                  mem_alloc.memoryTypeIndex );
     Assert( pass, 'No mappable, coherent memory' );

     (* allocate memory *)
     res := vkAllocateMemory( info_.device, @mem_alloc, nil, @mappableMemory );
     Assert( res = VK_SUCCESS );

     (* bind memory *)
     res := vkBindImageMemory( info_.device, mappableImage, mappableMemory, 0 );
     Assert( res = VK_SUCCESS );

     cmd_buf_info.sType            := VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO;
     cmd_buf_info.pNext            := nil;
     cmd_buf_info.flags            := 0;
     cmd_buf_info.pInheritanceInfo := nil;

     res := vkBeginCommandBuffer( info_.cmd, @cmd_buf_info );
     Assert( res = VK_SUCCESS );
     set_image_layout( info_, mappableImage, Ord( VK_IMAGE_ASPECT_COLOR_BIT ), VK_IMAGE_LAYOUT_UNDEFINED,
                       VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL, Ord( VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT ), Ord( VK_PIPELINE_STAGE_TRANSFER_BIT ) );

     set_image_layout( info_, info_.buffers[info_.current_buffer].image, Ord( VK_IMAGE_ASPECT_COLOR_BIT ), VK_IMAGE_LAYOUT_PRESENT_SRC_KHR,
                       VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL, Ord( VK_PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT ), Ord( VK_PIPELINE_STAGE_TRANSFER_BIT ) );

     copy_region.srcSubresource.aspectMask     := Ord( VK_IMAGE_ASPECT_COLOR_BIT );
     copy_region.srcSubresource.mipLevel       := 0;
     copy_region.srcSubresource.baseArrayLayer := 0;
     copy_region.srcSubresource.layerCount     := 1;
     copy_region.srcOffset.x                   := 0;
     copy_region.srcOffset.y                   := 0;
     copy_region.srcOffset.z                   := 0;
     copy_region.dstSubresource.aspectMask     := Ord( VK_IMAGE_ASPECT_COLOR_BIT );
     copy_region.dstSubresource.mipLevel       := 0;
     copy_region.dstSubresource.baseArrayLayer := 0;
     copy_region.dstSubresource.layerCount     := 1;
     copy_region.dstOffset.x                   := 0;
     copy_region.dstOffset.y                   := 0;
     copy_region.dstOffset.z                   := 0;
     copy_region.extent.width                  := info_.width;
     copy_region.extent.height                 := info_.height;
     copy_region.extent.depth                  := 1;

     (* Put the copy command into the command buffer *)
     vkCmdCopyImage( info_.cmd, info_.buffers[info_.current_buffer].image, VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL, mappableImage,
                     VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL, 1, @copy_region);

     set_image_layout( info_, mappableImage, Ord( VK_IMAGE_ASPECT_COLOR_BIT ), VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL, VK_IMAGE_LAYOUT_GENERAL,
                       Ord( VK_PIPELINE_STAGE_TRANSFER_BIT ), Ord( VK_PIPELINE_STAGE_HOST_BIT ) );

     res := vkEndCommandBuffer( info_.cmd );
     Assert( res = VK_SUCCESS );
     cmd_bufs[0] := info_.cmd;
     fenceInfo.sType := VK_STRUCTURE_TYPE_FENCE_CREATE_INFO;
     fenceInfo.pNext := nil;
     fenceInfo.flags := 0;
     vkCreateFence( info_.device, @fenceInfo, nil, @cmdFence );

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
     res := vkQueueSubmit( info_.graphics_queue, 1, @submit_info[0], cmdFence );
     Assert( res = VK_SUCCESS );

     (* Make sure command buffer is finished before mapping *)
     repeat
           res := vkWaitForFences( info_.device, 1, @cmdFence, VK_TRUE, FENCE_TIMEOUT );

     until res <> VK_TIMEOUT;
     Assert( res = VK_SUCCESS );

     vkDestroyFence( info_.device, cmdFence, nil );

     filename := basename_ + '.ppm';

     subres.aspectMask := Ord( VK_IMAGE_ASPECT_COLOR_BIT );
     subres.mipLevel   := 0;
     subres.arrayLayer := 0;
     vkGetImageSubresourceLayout( info_.device, mappableImage, @subres, @sr_layout );

     res := vkMapMemory( info_.device, mappableMemory, 0, mem_reqs.size, 0, @ptr );
     Assert( res = VK_SUCCESS );

     Inc( ptr, sr_layout.offset );
     F := TFileStream.Create( filename, fmCreate );

     S := 'P6'                                               + #13#10;  F.Write( BytesOf( S ), Length( S ) );
     S := info_.width.ToString + ' ' + info_.height.ToString + #13#10;  F.Write( BytesOf( S ), Length( S ) );
     S := '255'                                              + #13#10;  F.Write( BytesOf( S ), Length( S ) );

     for y := 0 to info_.height-1 do
     begin
          row := P_uint32_t( ptr );

          if ( info_.format = VK_FORMAT_B8G8R8A8_UNORM ) or ( info_.format = VK_FORMAT_B8G8R8A8_SRGB ) then
          begin
               for x := 0 to info_.width-1 do
               begin
                    swapped := ( row^ and $ff00ff00 ) or ( row^ and $000000ff ) shl 16 or ( row^ and $00ff0000 ) shr 16;
                    F.Write( swapped, 3 );
                    Inc( row );
               end;
          end
          else
          if info_.format = VK_FORMAT_R8G8B8A8_UNORM then
          begin
               for x := 0 to info_.width-1 do
               begin
                    F.Write( row^, 3 );
                    Inc( row );
               end;
          end
          else
          begin
               Log.d( 'Unrecognized image format - will not write image files' );
               Break;
          end;

          Inc( ptr, sr_layout.rowPitch );
     end;

     F.Free;
     vkUnmapMemory( info_.device, mappableMemory );
     vkDestroyImage( info_.device, mappableImage, nil );
     vkFreeMemory( info_.device, mappableMemory, nil );
end;

//////////////////////////////////////////////////////////////////////////////// draw_textured_cube

function read_ppm( const filename_:String; var width_:T_int; var height_:T_int; rowPitch_:T_uint64_t; dataPtr_:P_unsigned_char ) :T_bool;
type
    T_ReadHeader = reference to function( var S:String ) :Boolean;
const
     saneDimension :T_int = 32768;  //??
var
    magicStr  :String;
   heightStr  :String;
    widthStr  :String;
   formatStr  :String;
   fPtr       :TFileStream;
   ReadHeader :T_ReadHeader;
   count      :T_int;
   x, y       :T_int;
   rowPtr     :P_unsigned_char;
begin
     // PPM format expected from http://netpbm.sourceforge.net/doc/ppm.html
     //  1. magic number
     //  2. whitespace
     //  3. width
     //  4. whitespace
     //  5. height
     //  6. whitespace
     //  7. max color value
     //  8. whitespace
     //  7. data

     // Comments are not supported, but are detected and we kick out
     // Only 8 bits per channel is supported
     // If dataPtr is nullptr, only width and height are returned

     // Read in values from the PPM file as characters to check for comments
      magicStr := '';
      widthStr := '';
     heightStr := '';
     formatStr := '';

     try
          fPtr := TFileStream.Create( filename_, fmOpenRead or fmShareDenyWrite );

          try
               // Read the four values from file, accounting with any and all whitepace
               ReadHeader := function( var S:String ) :Boolean
               var
                  C :T_char;
               begin
                    while fPtr.Read( C, 1 ) = 1 do
                    begin
                         if C in [ #09, #10, #13, #32 ] then Exit( True );
                         S := S + Char( C );
                    end;
                    Result := False;
               end;
               Assert( ReadHeader(  magicStr ) );
               Assert( ReadHeader(  widthStr ) );
               Assert( ReadHeader( heightStr ) );
               Assert( ReadHeader( formatStr ) );

               // Kick out if comments present
               if ( magicStr.Chars[0] = '#' ) or ( widthStr.Chars[0] = '#' ) or ( heightStr.Chars[0] = '#' ) or ( formatStr.Chars[0] = '#' ) then
               begin
                    Log.d( 'Unhandled comment in PPM file' );
                    Exit( False );
               end;

               // Only one magic value is valid
               if magicStr <> 'P6' then
               begin
                    Log.d( 'Unhandled PPM magic number: ' + magicStr );
                    Exit( False );
               end;

               width_  := StrToInt(  widthStr );
               height_ := StrToInt( heightStr );

               // Ensure we got something sane for width/height
               if (  width_ <= 0 ) or (  width_ > saneDimension ) then
               begin
                    Log.d( 'Width seems wrong.  Update read_ppm if not: ' + width_.ToString );
                    Exit( False );
               end;
               if ( height_ <= 0 ) or ( height_ > saneDimension ) then
               begin
                    Log.d( 'Height seems wrong.  Update read_ppm if not: ' + height_.ToString );
                    Exit( False );
               end;

               if dataPtr_ = nil then
               begin
                    // If no destination pointer, caller only wanted dimensions
                    Exit( True );
               end;

               // Now read the data
               for y := 0 to height_-1 do
               begin
                    rowPtr := dataPtr_;
                    for x := 0 to width_-1 do
                    begin
                         count := fPtr.Read( rowPtr^, 3 );
                         Assert( count = 3 );
                         Inc( rowPtr, 3 ); rowPtr^ := 255; (* Alpha of 1 *)
                         Inc( rowPtr );
                    end;
                    Inc( dataPtr_, rowPitch_ );
               end;

               Result := True;

          finally
                 fPtr.Free;
          end;

     except
           Log.d( 'Bad filename in read_ppm: ' + filename_ );

           Result := False;
     end;
end;

end. //######################################################################### ■