unit Main;

{ https://github.com/LunarG/VulkanSamples/tree/master/API-Samples/05-init_swapchain }

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
Inititalize Swapchain
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
    const sample_title = 'Swapchain Initialization Sample';
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
   res                            :VkResult;
   {$IFDEF MSWINDOWS }
   createInfo                     :VkWin32SurfaceCreateInfoKHR;
   {$ELSEIF Defined( Android ) }
   createInfo                     :VkAndroidSurfaceCreateInfoKHR;
   {$ELSEIF Defined( VK_USE_PLATFORM_WAYLAND_KHR ) }
   createInfo                     :VkWaylandSurfaceCreateInfoKHR;
   {$ELSE}
   createInfo                     :VkXcbSurfaceCreateInfoKHR;
   {$ENDIF}
   pSupportsPresent               :TArray<VkBool32>;
   i                              :T_uint32_t;
   formatCount                    :T_uint32_t;
   surfFormats                    :TArray<VkSurfaceFormatKHR>;
   surfCapabilities               :VkSurfaceCapabilitiesKHR;
   presentModeCount               :T_uint32_t;
   presentModes                   :TArray<VkPresentModeKHR>;
   swapchainExtent                :VkExtent2D;
   swapchainPresentMode           :VkPresentModeKHR;
   desiredNumberOfSwapChainImages :T_uint32_t;
   preTransform                   :VkSurfaceTransformFlagBitsKHR;
   compositeAlpha                 :VkCompositeAlphaFlagBitsKHR;
   compositeAlphaFlags            :array [ 0..4-1 ] of VkCompositeAlphaFlagBitsKHR;
   swapchain_ci                   :VkSwapchainCreateInfoKHR;
   queueFamilyIndices             :array [ 0..2-1 ] of T_uint32_t;
   swapchainImages                :TArray<VkImage>;
   color_image_view               :VkImageViewCreateInfo;
begin
     Caption := sample_title;

     (*
      * Set up swapchain:
      * - Get supported uses for all queues
      * - Try to find a queue that supports both graphics and present
      * - If no queue supports both, find a present queue and make sure we have a
      *   graphics queue
      * - Get a list of supported formats and use the first one
      * - Get surface properties and present modes and use them to create a swap
      *   chain
      * - Create swap chain buffers
      * - For each buffer, create a color attachment view and set its layout to
      *   color attachment
      *)

     init_global_layer_properties( info );
     init_instance_extension_names( info );
     init_device_extension_names( info );
     init_instance( info, sample_title );
     init_enumerate_device( info );
     init_window_size( info, 400, 300 );
     init_connection( info );
     init_window( info );

     (* VULKAN_KEY_START *)

     // Construct the surface description:
     {$IFDEF MSWINDOWS }
     createInfo.sType      := VK_STRUCTURE_TYPE_WIN32_SURFACE_CREATE_INFO_KHR;
     createInfo.pNext      := nil;
     createInfo.hinstance  := info.connection;
     createInfo.hwnd       := info.window;
     res := vkCreateWin32SurfaceKHR( info.inst, @createInfo, nil, @info.surface );
     {$ELSEIF Defined( Android ) }
     GET_INSTANCE_PROC_ADDR( info.inst, CreateAndroidSurfaceKHR );

     createInfo.sType      := VK_STRUCTURE_TYPE_ANDROID_SURFACE_CREATE_INFO_KHR;
     createInfo.pNext      := nil;
     createInfo.flags      := 0;
     createInfo.window     := AndroidGetApplicationWindow;
     res := info.fpCreateAndroidSurfaceKHR( info.inst, &createInfo, nil, @info.surface );
     {$ELSEIF Defined( VK_USE_PLATFORM_WAYLAND_KHR ) }
     createInfo.sType      := VK_STRUCTURE_TYPE_WAYLAND_SURFACE_CREATE_INFO_KHR;
     createInfo.pNext      := nil;
     createInfo.display    := info.display;
     createInfo.surface    := info.window;
     res := vkCreateWaylandSurfaceKHR( info.inst, @createInfo, nil, @info.surface );
     {$ELSE}
     createInfo.sType      := VK_STRUCTURE_TYPE_XCB_SURFACE_CREATE_INFO_KHR;
     createInfo.pNext      := nil;
     createInfo.connection := info.connection;
     createInfo.window     := info.window;
     res := vkCreateXcbSurfaceKHR( info.inst, @createInfo, nil, @info.surface );
     {$ENDIF}  // MSWINDOWS
     Assert( res = VK_SUCCESS );

     // Iterate over each queue to learn whether it supports presenting:
     SetLength( pSupportsPresent, info.queue_family_count );
     for i := 0 to info.queue_family_count-1 do vkGetPhysicalDeviceSurfaceSupportKHR( info.gpus[0], i, info.surface, @pSupportsPresent[i] );

     // Search for a graphics and a present queue in the array of queue
     // families, try to find one that supports both
     info.graphics_queue_family_index := UINT32_MAX;
     info.present_queue_family_index  := UINT32_MAX;
     for i := 0 to info.queue_family_count-1 do
     begin
          if ( info.queue_props[i].queueFlags and VkQueueFlags( VK_QUEUE_GRAPHICS_BIT ) ) <> 0 then
          begin
               if info.graphics_queue_family_index = UINT32_MAX then info.graphics_queue_family_index := i;

               if pSupportsPresent[i] = VK_TRUE then
               begin
                    info.graphics_queue_family_index := i;
                    info.present_queue_family_index  := i;
                    Break;
               end;
          end;
     end;

     if info.present_queue_family_index = UINT32_MAX then
     begin
          // If didn't find a queue that supports both graphics and present, then
          // find a separate present queue.
          for i := 0 to info.queue_family_count-1 do
          begin
               if pSupportsPresent[i] = VK_TRUE then
               begin
                    info.present_queue_family_index := i;
                    Break;
               end;
          end;
     end;
     pSupportsPresent := nil;

     // Generate error if could not find queues that support graphics
     // and present
     if ( info.graphics_queue_family_index = UINT32_MAX ) or ( info.present_queue_family_index = UINT32_MAX ) then
     begin
          Log.d( 'Could not find a queues for graphics and present' );
          RunError( 256-1 );
     end;

     init_device( info );

     // Get the list of VkFormats that are supported:
     res := vkGetPhysicalDeviceSurfaceFormatsKHR( info.gpus[0], info.surface, @formatCount, nil );
     Assert( res = VK_SUCCESS );
     SetLength( surfFormats, formatCount );
     res := vkGetPhysicalDeviceSurfaceFormatsKHR( info.gpus[0], info.surface, @formatCount, @surfFormats[0] );
     Assert( res = VK_SUCCESS );
     // If the format list includes just one entry of VK_FORMAT_UNDEFINED,
     // the surface has no preferred format.  Otherwise, at least one
     // supported format will be returned.
     if ( formatCount = 1 ) and ( surfFormats[0].format = VK_FORMAT_UNDEFINED ) then info.format := VK_FORMAT_B8G8R8A8_UNORM
     else
     begin
          Assert( formatCount >= 1 );
          info.format := surfFormats[0].format;
     end;
     surfFormats := nil;

     res := vkGetPhysicalDeviceSurfaceCapabilitiesKHR( info.gpus[0], info.surface, @surfCapabilities );
     Assert( res = VK_SUCCESS );

     res := vkGetPhysicalDeviceSurfacePresentModesKHR( info.gpus[0], info.surface, @presentModeCount, nil );
     Assert( res = VK_SUCCESS );
     SetLength( presentModes, presentModeCount );

     res := vkGetPhysicalDeviceSurfacePresentModesKHR( info.gpus[0], info.surface, @presentModeCount, @presentModes[0] );
     Assert( res = VK_SUCCESS );

     // width and height are either both 0xFFFFFFFF, or both not 0xFFFFFFFF.
     if surfCapabilities.currentExtent.width = $FFFFFFFF then
     begin
          // If the surface size is undefined, the size is set to
          // the size of the images requested.
          swapchainExtent.width  := info.width;
          swapchainExtent.height := info.height;

          if swapchainExtent.width  < surfCapabilities.minImageExtent.width  then swapchainExtent.width  := surfCapabilities.minImageExtent.width
                                                                             else
          if swapchainExtent.width  > surfCapabilities.maxImageExtent.width  then swapchainExtent.width  := surfCapabilities.maxImageExtent.width ;

          if swapchainExtent.height < surfCapabilities.minImageExtent.height then swapchainExtent.height := surfCapabilities.minImageExtent.height
                                                                             else
          if swapchainExtent.height > surfCapabilities.maxImageExtent.height then swapchainExtent.height := surfCapabilities.maxImageExtent.height;
     end
     else
     begin
          // If the surface size is defined, the swap chain size must match
          swapchainExtent := surfCapabilities.currentExtent;
     end;

     // The FIFO present mode is guaranteed by the spec to be supported
     swapchainPresentMode := VK_PRESENT_MODE_FIFO_KHR;

     // Determine the number of VkImage's to use in the swap chain.
     // We need to acquire only 1 presentable image at at time.
     // Asking for minImageCount images ensures that we can acquire
     // 1 presentable image as long as we present it before attempting
     // to acquire another.
     desiredNumberOfSwapChainImages := surfCapabilities.minImageCount;

     if ( surfCapabilities.supportedTransforms and VkSurfaceTransformFlagsKHR( VK_SURFACE_TRANSFORM_IDENTITY_BIT_KHR ) ) <> 0 then preTransform := VK_SURFACE_TRANSFORM_IDENTITY_BIT_KHR
                                                                                                                             else preTransform := surfCapabilities.currentTransform;

     // Find a supported composite alpha mode - one of these is guaranteed to be set
     compositeAlpha := VK_COMPOSITE_ALPHA_OPAQUE_BIT_KHR;
     compositeAlphaFlags[0] := VK_COMPOSITE_ALPHA_OPAQUE_BIT_KHR;
     compositeAlphaFlags[1] := VK_COMPOSITE_ALPHA_PRE_MULTIPLIED_BIT_KHR;
     compositeAlphaFlags[2] := VK_COMPOSITE_ALPHA_POST_MULTIPLIED_BIT_KHR;
     compositeAlphaFlags[3] := VK_COMPOSITE_ALPHA_INHERIT_BIT_KHR;
     for i := 0 to Length( compositeAlphaFlags )-1 do
     begin
          if ( surfCapabilities.supportedCompositeAlpha and VkCompositeAlphaFlagsKHR( compositeAlphaFlags[i] ) ) <> 0 then
          begin
               compositeAlpha := compositeAlphaFlags[i];
               Break;
          end;
     end;

     swapchain_ci.sType                 := VK_STRUCTURE_TYPE_SWAPCHAIN_CREATE_INFO_KHR;
     swapchain_ci.pNext                 := nil;
     swapchain_ci.surface               := info.surface;
     swapchain_ci.minImageCount         := desiredNumberOfSwapChainImages;
     swapchain_ci.imageFormat           := info.format;
     swapchain_ci.imageExtent.width     := swapchainExtent.width;
     swapchain_ci.imageExtent.height    := swapchainExtent.height;
     swapchain_ci.preTransform          := preTransform;
     swapchain_ci.compositeAlpha        := compositeAlpha;
     swapchain_ci.imageArrayLayers      := 1;
     swapchain_ci.presentMode           := swapchainPresentMode;
     swapchain_ci.oldSwapchain          := VK_NULL_HANDLE;
     swapchain_ci.clipped               := 1{true};
     swapchain_ci.imageColorSpace       := VK_COLORSPACE_SRGB_NONLINEAR_KHR;
     swapchain_ci.imageUsage            := VkImageUsageFlags( VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT );
     swapchain_ci.imageSharingMode      := VK_SHARING_MODE_EXCLUSIVE;
     swapchain_ci.queueFamilyIndexCount := 0;
     swapchain_ci.pQueueFamilyIndices   := nil;
     queueFamilyIndices[0]              := T_uint32_t( info.graphics_queue_family_index );
     queueFamilyIndices[0]              := T_uint32_t( info.present_queue_family_index );
     if info.graphics_queue_family_index <> info.present_queue_family_index then
     begin
          // If the graphics and present queues are from different queue families,
          // we either have to explicitly transfer ownership of images between
          // the queues, or we have to create the swapchain with imageSharingMode
          // as VK_SHARING_MODE_CONCURRENT
          swapchain_ci.imageSharingMode      := VK_SHARING_MODE_CONCURRENT;
          swapchain_ci.queueFamilyIndexCount := 2;
          swapchain_ci.pQueueFamilyIndices   := @queueFamilyIndices[0];
     end;

     res := vkCreateSwapchainKHR( info.device, @swapchain_ci, nil, @info.swap_chain );
     Assert( res = VK_SUCCESS );

     res := vkGetSwapchainImagesKHR( info.device, info.swap_chain, @info.swapchainImageCount, nil );
     Assert( res = VK_SUCCESS );

     SetLength( swapchainImages, info.swapchainImageCount );
     Assert( Length( swapchainImages ) > 0 );
     res := vkGetSwapchainImagesKHR( info.device, info.swap_chain, @info.swapchainImageCount, @swapchainImages[0] );
     Assert( res = VK_SUCCESS );

     SetLength( info.buffers, info.swapchainImageCount );
     for i := 0 to info.swapchainImageCount-1 do info.buffers[i].image := swapchainImages[i];
     swapchainImages := nil;

     for i := 0 to info.swapchainImageCount-1 do
     begin
          color_image_view.sType                           := VK_STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO;
          color_image_view.pNext                           := nil;
          color_image_view.flags                           := 0;
          color_image_view.image                           := info.buffers[i].image;
          color_image_view.viewType                        := VK_IMAGE_VIEW_TYPE_2D;
          color_image_view.format                          := info.format;
          color_image_view.components.r                    := VK_COMPONENT_SWIZZLE_R;
          color_image_view.components.g                    := VK_COMPONENT_SWIZZLE_G;
          color_image_view.components.b                    := VK_COMPONENT_SWIZZLE_B;
          color_image_view.components.a                    := VK_COMPONENT_SWIZZLE_A;
          color_image_view.subresourceRange.aspectMask     := VkImageAspectFlags( VK_IMAGE_ASPECT_COLOR_BIT );
          color_image_view.subresourceRange.baseMipLevel   := 0;
          color_image_view.subresourceRange.levelCount     := 1;
          color_image_view.subresourceRange.baseArrayLayer := 0;
          color_image_view.subresourceRange.layerCount     := 1;

          res := vkCreateImageView( info.device, @color_image_view, nil, @info.buffers[i].view );
          Assert( res = VK_SUCCESS );
     end;

     (* VULKAN_KEY_END *)
end;

procedure TForm1.FormDestroy(Sender: TObject);
var
   i :T_uint32_t;
begin
     for i := 0 to info.swapchainImageCount-1 do vkDestroyImageView( info.device, info.buffers[i].view, nil );
     vkDestroySwapchainKHR( info.device, info.swap_chain, nil );
     destroy_device( info );
     destroy_window( info );
     destroy_instance( info );
end;

end. //######################################################################### ■
