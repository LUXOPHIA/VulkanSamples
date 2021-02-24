unit Main;

{ https://github.com/LunarG/VulkanSamples/tree/master/API-Samples/06-init_depth_buffer }

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
create Vulkan depth buffer
*)

(* This is part of the draw cube progression *)

interface //#################################################################### ■

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Memo.Types, FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo,
  vulkan_core, vulkan_win32,
  vulkan.util, vulkan.util_init,
  LUX.Code.C;

type
  TForm1 = class(TForm)
    Memo1: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { private 宣言 }
    const sample_title = 'Depth Buffer Sample';
  public
    { public 宣言 }
    info :T_sample_info;
  end;

var
  Form1: TForm1;

implementation //############################################################### ■

{$R *.fmx}

procedure TForm1.FormCreate(Sender: TObject);
var
   res          :VkResult;
   pass         :T_bool;
   image_info   :VkImageCreateInfo;
   depth_format :VkFormat;
   props        :VkFormatProperties;
   mem_alloc    :VkMemoryAllocateInfo;
   view_info    :VkImageViewCreateInfo;
   mem_reqs     :VkMemoryRequirements;
begin
     (*
      * Make a depth buffer:
      * - Create an Image to be the depth buffer
      * - Find memory requirements
      * - Allocate and bind memory
      * - Set the image layout
      * - Create an attachment view
      *)

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

     (* VULKAN_KEY_START *)
     depth_format := VK_FORMAT_D16_UNORM;
     vkGetPhysicalDeviceFormatProperties( info.gpus[0], depth_format, @props );
     if ( props.linearTilingFeatures and VkFormatFeatureFlags( VK_FORMAT_FEATURE_DEPTH_STENCIL_ATTACHMENT_BIT ) ) <> 0
     then image_info.tiling := VK_IMAGE_TILING_LINEAR
     else
     if ( props.optimalTilingFeatures and VkFormatFeatureFlags( VK_FORMAT_FEATURE_DEPTH_STENCIL_ATTACHMENT_BIT ) ) <> 0
     then image_info.tiling := VK_IMAGE_TILING_OPTIMAL
     else
     begin
          (* Try other depth formats? *)
          Log.d( 'VK_FORMAT_D16_UNORM Unsupported.' );
          RunError( 256-1 );
     end;

    image_info.sType                 := VK_STRUCTURE_TYPE_IMAGE_CREATE_INFO;
    image_info.pNext                 := nil;
    image_info.imageType             := VK_IMAGE_TYPE_2D;
    image_info.format                := depth_format;
    image_info.extent.width          := info.width;
    image_info.extent.height         := info.height;
    image_info.extent.depth          := 1;
    image_info.mipLevels             := 1;
    image_info.arrayLayers           := 1;
    image_info.samples               := NUM_SAMPLES;
    image_info.initialLayout         := VK_IMAGE_LAYOUT_UNDEFINED;
    image_info.usage                 := VkImageUsageFlags( VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT );
    image_info.queueFamilyIndexCount := 0;
    image_info.pQueueFamilyIndices   := nil;
    image_info.sharingMode           := VK_SHARING_MODE_EXCLUSIVE;
    image_info.flags                 := 0;

    mem_alloc.sType           := VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO;
    mem_alloc.pNext           := nil;
    mem_alloc.allocationSize  := 0;
    mem_alloc.memoryTypeIndex := 0;

    view_info.sType                           := VK_STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO;
    view_info.pNext                           := nil;
    view_info.image                           := VK_NULL_HANDLE;
    view_info.format                          := depth_format;
    view_info.components.r                    := VK_COMPONENT_SWIZZLE_R;
    view_info.components.g                    := VK_COMPONENT_SWIZZLE_G;
    view_info.components.b                    := VK_COMPONENT_SWIZZLE_B;
    view_info.components.a                    := VK_COMPONENT_SWIZZLE_A;
    view_info.subresourceRange.aspectMask     := VkImageAspectFlags( VK_IMAGE_ASPECT_DEPTH_BIT );
    view_info.subresourceRange.baseMipLevel   := 0;
    view_info.subresourceRange.levelCount     := 1;
    view_info.subresourceRange.baseArrayLayer := 0;
    view_info.subresourceRange.layerCount     := 1;
    view_info.viewType                        := VK_IMAGE_VIEW_TYPE_2D;
    view_info.flags                           := 0;

    info.depth.format := depth_format;

    (* Create image *)
    res := vkCreateImage( info.device, @image_info, nil, @info.depth.image );
    Assert( res = VK_SUCCESS );

    vkGetImageMemoryRequirements( info.device, info.depth.image, @mem_reqs );

    mem_alloc.allocationSize := mem_reqs.size;
    (* Use the memory properties to determine the type of memory required *)
    pass := memory_type_from_properties( info, mem_reqs.memoryTypeBits, VkFlags( VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT ), mem_alloc.memoryTypeIndex );
    Assert( pass );

    (* Allocate memory *)
    res := vkAllocateMemory( info.device, @mem_alloc, nil, @info.depth.mem );
    assert( res = VK_SUCCESS );

    (* Bind memory *)
    res := vkBindImageMemory( info.device, info.depth.image, info.depth.mem, 0 );
    assert( res = VK_SUCCESS );

    (* Create image view *)
    view_info.image := info.depth.image;
    res := vkCreateImageView( info.device, @view_info, nil, @info.depth.view );
    assert( res = VK_SUCCESS );

    (* VULKAN_KEY_END *)
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
     (* Clean Up *)
     vkDestroyImageView( info.device, info.depth.view, nil );
     vkDestroyImage( info.device, info.depth.image, nil );
     vkFreeMemory( info.device, info.depth.mem, nil );
     destroy_device( info );
     destroy_window( info );
     destroy_instance( info );
end;

end. //######################################################################### ■
