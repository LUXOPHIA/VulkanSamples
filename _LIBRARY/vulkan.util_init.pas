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

function init_global_extension_properties( var layer_props_:T_layer_properties ) :VkResult;
function init_global_layer_properties( const Vulkan_:TVulkan ) :VkResult;

//////////////////////////////////////////////////////////////////////////////// 02-enumerate_devices

//////////////////////////////////////////////////////////////////////////////// 03-init_device

//////////////////////////////////////////////////////////////////////////////// 04-init_command_buffer

function init_device( const Vulkan_:TVulkan ) :VkResult;
procedure destroy_device( const Vulkan_:TVulkan );

//////////////////////////////////////////////////////////////////////////////// 05-init_swapchain

procedure init_instance_extension_names( const Vulkan_:TVulkan );
procedure init_device_extension_names( const Vulkan_:TVulkan );
procedure init_window_size( const Vulkan_:TVulkan; default_width_,default_height_:UInt32 );
procedure init_connection( const Vulkan_:TVulkan );
procedure init_window( const Vulkan_:TVulkan );
procedure destroy_window( const Vulkan_:TVulkan );

//////////////////////////////////////////////////////////////////////////////// 06-init_depth_buffer

procedure init_swapchain_extension( const Vulkan_:TVulkan );

//////////////////////////////////////////////////////////////////////////////// 07-init_uniform_buffer

//////////////////////////////////////////////////////////////////////////////// 08-init_pipeline_layout

//////////////////////////////////////////////////////////////////////////////// 09-init_descriptor_set

procedure init_uniform_buffer( const Vulkan_:TVulkan );
procedure init_descriptor_and_pipeline_layouts( const Vulkan_:TVulkan; use_texture_:T_bool; descSetLayoutCreateFlags_:VkDescriptorSetLayoutCreateFlags = 0 );
procedure destroy_uniform_buffer( const Vulkan_:TVulkan );
procedure destroy_descriptor_and_pipeline_layouts( const Vulkan_:TVulkan );

//////////////////////////////////////////////////////////////////////////////// 10-init_render_pass

procedure init_device_queue( const Vulkan_:TVulkan );
procedure init_swap_chain( const Vulkan_:TVulkan; usageFlags_:VkImageUsageFlags = Ord( VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT ) or Ord( VK_IMAGE_USAGE_TRANSFER_SRC_BIT ) );
procedure init_depth_buffer( const Vulkan_:TVulkan );
procedure destroy_depth_buffer( const Vulkan_:TVulkan );
procedure destroy_swap_chain( const Vulkan_:TVulkan );

//////////////////////////////////////////////////////////////////////////////// 11-init_shaders

//////////////////////////////////////////////////////////////////////////////// 12-init_frame_buffers

procedure init_command_pool( const Vulkan_:TVulkan );
procedure init_command_buffer( const Vulkan_:TVulkan );
procedure execute_begin_command_buffer( const Vulkan_:TVulkan );
procedure init_renderpass( const Vulkan_:TVulkan;
                           include_depth_:T_bool;
                           clear_:T_bool = True;
                           finalLayout_:VkImageLayout = VK_IMAGE_LAYOUT_PRESENT_SRC_KHR;
                           initialLayout_:VkImageLayout = VK_IMAGE_LAYOUT_UNDEFINED );
procedure execute_end_command_buffer( const Vulkan_:TVulkan );
procedure execute_queue_command_buffer( const Vulkan_:TVulkan );
procedure destroy_renderpass( const Vulkan_:TVulkan );
procedure destroy_command_buffer( const Vulkan_:TVulkan );
procedure destroy_command_pool( const Vulkan_:TVulkan );

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

function init_global_extension_properties( var layer_props_:T_layer_properties ) :VkResult;
var
   instance_extensions      :P_VkExtensionProperties;
   instance_extension_count :T_uint32_t;
   layer_name               :P_char;
begin
     layer_name := layer_props_.properties.layerName;

     repeat
           Result := vkEnumerateInstanceExtensionProperties( layer_name, @instance_extension_count, nil );
           if Result <> VK_SUCCESS then Exit;

           if instance_extension_count = 0 then Exit( VK_SUCCESS );

           SetLength( layer_props_.instance_extensions, instance_extension_count );
           instance_extensions := @layer_props_.instance_extensions[0];
           Result := vkEnumerateInstanceExtensionProperties( layer_name, @instance_extension_count, instance_extensions );

     until Result <> VK_INCOMPLETE;
end;

function init_global_layer_properties( const Vulkan_:TVulkan ) :VkResult;
var
   instance_layer_count :T_uint32_t;
   vk_props             :TArray<VkLayerProperties>;
   i                    :T_uint32_t;
   layer_props          :T_layer_properties;
begin
     (*
      * It's possible, though very rare, that the number of
      * instance layers could change. For example, installing something
      * could include new layers that the loader would pick up
      * between the initial query for the count and the
      * request for VkLayerProperties. The loader indicates that
      * by returning a VK_INCOMPLETE status and will update the
      * the count parameter.
      * The count parameter will be updated with the number of
      * entries loaded into the data pointer - in case the number
      * of layers went down or is smaller than the size given.
      *)
     repeat
           Result := vkEnumerateInstanceLayerProperties( @instance_layer_count, nil );
           if Result <> VK_SUCCESS then Exit;

           if instance_layer_count = 0 then Exit( VK_SUCCESS );

           SetLength( vk_props, instance_layer_count );

           Result := vkEnumerateInstanceLayerProperties( @instance_layer_count, @vk_props[0] );

     until Result <> VK_INCOMPLETE;

     (*
      * Now gather the extension list for each instance layer.
      *)
     for i := 0 to instance_layer_count-1 do
     begin
          layer_props.properties := vk_props[i];
          Result := init_global_extension_properties( layer_props );
          if Result <> VK_SUCCESS then Exit;
          Vulkan_.Info.instance_layer_properties := Vulkan_.Info.instance_layer_properties + [ layer_props ];
     end;
     vk_props := nil;
end;

//////////////////////////////////////////////////////////////////////////////// 02-enumerate_devices

//////////////////////////////////////////////////////////////////////////////// 03-init_device

//////////////////////////////////////////////////////////////////////////////// 04-init_command_buffer

function init_device( const Vulkan_:TVulkan ) :VkResult;
var
   queue_info       :VkDeviceQueueCreateInfo;
   queue_priorities :array [ 0..1-1 ] of T_float;
   device_info      :VkDeviceCreateInfo;
begin
     queue_priorities[0]         := 0;
     queue_info                  := Default( VkDeviceQueueCreateInfo );
     queue_info.sType            := VK_STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO;
     queue_info.pNext            := nil;
     queue_info.queueCount       := 1;
     queue_info.pQueuePriorities := @queue_priorities[0];
     queue_info.queueFamilyIndex := Vulkan_.Info.graphics_queue_family_index;
     device_info                              := Default( VkDeviceCreateInfo );
     device_info.sType                        := VK_STRUCTURE_TYPE_DEVICE_CREATE_INFO;
     device_info.pNext                        := nil;
     device_info.queueCreateInfoCount         := 1;
     device_info.pQueueCreateInfos            := @queue_info;
     device_info.enabledExtensionCount        := Length( Vulkan_.Info.device_extension_names );
     if device_info.enabledExtensionCount > 0
     then device_info.ppEnabledExtensionNames := @Vulkan_.Info.device_extension_names[0]
     else device_info.ppEnabledExtensionNames := nil;
     device_info.pEnabledFeatures             := nil;
     Result := vkCreateDevice( Vulkan_.Devices.Devices[ 0 ].Handle, @device_info, nil, @Vulkan_.Info.device );
     Assert( Result = VK_SUCCESS );
end;

procedure destroy_device( const Vulkan_:TVulkan );
begin
     vkDeviceWaitIdle( Vulkan_.Info.device );
     vkDestroyDevice( Vulkan_.Info.device, nil );
end;

//////////////////////////////////////////////////////////////////////////////// 05-init_swapchain

procedure init_instance_extension_names( const Vulkan_:TVulkan );
begin
     Vulkan_.Info.instance_extension_names := Vulkan_.Info.instance_extension_names + [ VK_KHR_SURFACE_EXTENSION_NAME         ];
     Vulkan_.Info.instance_extension_names := Vulkan_.Info.instance_extension_names + [ VK_KHR_WIN32_SURFACE_EXTENSION_NAME   ];
end;

procedure init_device_extension_names( const Vulkan_:TVulkan );
begin
     Vulkan_.Info.device_extension_names := Vulkan_.Info.device_extension_names + [ VK_KHR_SWAPCHAIN_EXTENSION_NAME ];
end;

procedure init_window_size( const Vulkan_:TVulkan; default_width_,default_height_:UInt32 );
begin
     Vulkan_.Info.width  := default_width_;
     Vulkan_.Info.height := default_height_;
end;

procedure init_connection( const Vulkan_:TVulkan );
begin

end;

procedure run( var info:T_sample_info );
begin
     (* Placeholder for samples that want to show dynamic content *)
end;

function WndProc( hwnd:HWND; uMsg:UINT; wParam:WPARAM; lParam:LPARAM ) :LRESULT; stdcall;
var
   info :P_sample_info;
begin
     info := P_sample_info( GetWindowLongPtr( hWnd, GWLP_USERDATA ) );

     case uMsg of
       WM_CLOSE:
          PostQuitMessage( 0 );
       WM_PAINT:
          begin
               run( info^ );
               Exit( 0 );
          end;
     else
     end;
     Result := DefWindowProc( hWnd, uMsg, wParam, lParam );
end;

procedure init_window( const Vulkan_:TVulkan );
var
   win_class :WNDCLASSEX;
   wr        :TRect;
begin
     Assert( Vulkan_.Info.width  > 0 );
     Assert( Vulkan_.Info.height > 0 );

     Vulkan_.Info.connection := GetModuleHandle( nil );
     Vulkan_.Info.name       := 'Sample';

     // Initialize the window class structure:
     win_class.cbSize        := SizeOf( WNDCLASSEX );
     win_class.style         := CS_HREDRAW or CS_VREDRAW;
     win_class.lpfnWndProc   := @WndProc;
     win_class.cbClsExtra    := 0;
     win_class.cbWndExtra    := 0;
     win_class.hInstance     := Vulkan_.Info.connection;  // hInstance
     win_class.hIcon         := LoadIcon( 0, IDI_APPLICATION );
     win_class.hCursor       := LoadCursor( 0, IDC_ARROW );
     win_class.hbrBackground := HBRUSH( GetStockObject( WHITE_BRUSH ) );
     win_class.lpszMenuName  := nil;
     win_class.lpszClassName := LPCWSTR( WideString( Vulkan_.Info.name ) );
     win_class.hIconSm       := LoadIcon( 0, IDI_WINLOGO );
     // Register window class:
     if RegisterClassEx( win_class ) = 0 then
     begin
          // It didn't work, so try to give a useful error:
          Log.d( 'Unexpected error trying to start the application!' );
          RunError( 1 );
     end;
     // Create window with the registered class:
     wr := TRect.Create( 0, 0, Vulkan_.Info.width, Vulkan_.Info.height );
     AdjustWindowRect( wr, WS_OVERLAPPEDWINDOW, False );
     Vulkan_.Info.window := CreateWindowEx( 0,
                                    LPCWSTR( WideString( Vulkan_.Info.name ) ),              // class name
                                    LPCWSTR( WideString( Vulkan_.Info.name ) ),              // app name
                                    WS_OVERLAPPEDWINDOW or WS_VISIBLE or WS_SYSMENU,  // window style
                                    100, 100,                                         // x/y coords
                                    wr.right - wr.left,                               // width
                                    wr.bottom - wr.top,                               // height
                                    0,                                                // handle to parent
                                    0,                                                // handle to menu
                                    Vulkan_.Info.connection,                                 // hInstance
                                    nil );                                            // no extra parameters
     if Vulkan_.Info.window = 0 then
     begin
          // It didn't work, so try to give a useful error:
          Log.d( 'Cannot create a window in which to draw!' );
          RunError( 1 );
     end;
     SetWindowLongPtr( Vulkan_.Info.window, GWLP_USERDATA, LONG_PTR( @Vulkan_.Info ) );
end;

procedure destroy_window( const Vulkan_:TVulkan );
begin
     vkDestroySurfaceKHR( Vulkan_.Instance.Handle, Vulkan_.Info.surface, nil );
     DestroyWindow( Vulkan_.Info.window );
end;

//////////////////////////////////////////////////////////////////////////////// 06-init_depth_buffer

(* Use this surface format if it's available.  This ensures that generated
* images are similar on different devices and with different drivers.
*)
const PREFERRED_SURFACE_FORMAT = VK_FORMAT_B8G8R8A8_UNORM;

procedure init_swapchain_extension( const Vulkan_:TVulkan );
var
   res              :VkResult;
   createInfo       :VkWin32SurfaceCreateInfoKHR;
   pSupportsPresent :TArray<VkBool32>;
   i                :T_uint32_t;
   formatCount      :T_uint32_t;
   surfFormats      :TArray<VkSurfaceFormatKHR>;
begin
     (* DEPENDS on init_connection() and init_window() *)

     // Construct the surface description:
     createInfo           := Default( VkWin32SurfaceCreateInfoKHR );
     createInfo.sType     := VK_STRUCTURE_TYPE_WIN32_SURFACE_CREATE_INFO_KHR;
     createInfo.pNext     := nil;
     createInfo.hinstance := Vulkan_.Info.connection;
     createInfo.hwnd      := Vulkan_.Info.window;
     res := vkCreateWin32SurfaceKHR( Vulkan_.Instance.Handle, @createInfo, nil, @Vulkan_.Info.surface );
     Assert( res = VK_SUCCESS );

     // Iterate over each queue to learn whether it supports presenting:
     SetLength( pSupportsPresent, Vulkan_.Info.queue_family_count );
     for i := 0 to Vulkan_.Info.queue_family_count-1
     do vkGetPhysicalDeviceSurfaceSupportKHR( Vulkan_.Devices.Devices[ 0 ].Handle, i, Vulkan_.Info.surface, @pSupportsPresent[i] );

     // Search for a graphics and a present queue in the array of queue
     // families, try to find one that supports both
     Vulkan_.Info.graphics_queue_family_index := UINT32_MAX;
     Vulkan_.Info.present_queue_family_index  := UINT32_MAX;
     for i := 0 to Vulkan_.Info.queue_family_count-1 do
     begin
          if ( Vulkan_.Info.queue_props[i].queueFlags and Ord( VK_QUEUE_GRAPHICS_BIT ) ) <> 0 then
          begin
               if Vulkan_.Info.graphics_queue_family_index = UINT32_MAX then Vulkan_.Info.graphics_queue_family_index := i;

               if pSupportsPresent[i] = VK_TRUE then
               begin
                    Vulkan_.Info.graphics_queue_family_index := i;
                    Vulkan_.Info.present_queue_family_index  := i;
                    Break;
               end;
          end;
     end;

     if Vulkan_.Info.present_queue_family_index = UINT32_MAX then
     begin
          // If didn't find a queue that supports both graphics and present, then
          // find a separate present queue.
          for i := 0 to Vulkan_.Info.queue_family_count-1 do
          begin
               if pSupportsPresent[i] = VK_TRUE then
               begin
                    Vulkan_.Info.present_queue_family_index := i;
                    Break;
               end;
          end;
     end;
     pSupportsPresent := nil;

     // Generate error if could not find queues that support graphics
     // and present
     if ( Vulkan_.Info.graphics_queue_family_index = UINT32_MAX ) or ( Vulkan_.Info.present_queue_family_index = UINT32_MAX ) then
     begin
          Log.d( 'Could not find a queues for both graphics and present' );
          RunError( 256-1 );
     end;

     // Get the list of VkFormats that are supported:
     res := vkGetPhysicalDeviceSurfaceFormatsKHR( Vulkan_.Devices.Devices[ 0 ].Handle, Vulkan_.Info.surface, @formatCount, nil );
     Assert( res = VK_SUCCESS );
     SetLength( surfFormats, formatCount );
     res := vkGetPhysicalDeviceSurfaceFormatsKHR( Vulkan_.Devices.Devices[ 0 ].Handle, Vulkan_.Info.surface, @formatCount, @surfFormats[0] );
     Assert( res = VK_SUCCESS );

     // If the device supports our preferred surface format, use it.
     // Otherwise, use whatever the device's first reported surface
     // format is.
     Assert( formatCount >= 1 );
     Vulkan_.Info.format := surfFormats[0].format;
     for i := 0 to formatCount-1 do
     begin
          if surfFormats[i].format = PREFERRED_SURFACE_FORMAT then
          begin
               Vulkan_.Info.format := PREFERRED_SURFACE_FORMAT;
               break;
          end;
     end;

     surfFormats := nil;
end;

//////////////////////////////////////////////////////////////////////////////// 07-init_uniform_buffer

//////////////////////////////////////////////////////////////////////////////// 08-init_pipeline_layout

//////////////////////////////////////////////////////////////////////////////// 09-init_descriptor_set

procedure init_uniform_buffer( const Vulkan_:TVulkan );
var
   res        :VkResult;
   pass       :T_bool;
   fov        :T_float;
   buf_info   :VkBufferCreateInfo;
   mem_reqs   :VkMemoryRequirements;
   alloc_info :VkMemoryAllocateInfo;
   pData      :P_uint8_t;
begin
     fov := DegToRad( 45 );
     if Vulkan_.Info.width > Vulkan_.Info.height then
     begin
          fov := fov * Vulkan_.Info.height / Vulkan_.Info.width;
     end;
     Vulkan_.Info.Projection := TSingleM4.ProjPersH( fov, Vulkan_.Info.width / Vulkan_.Info.height, 0.1, 100 );
     Vulkan_.Info.View := TSingleM4.LookAt( TSingle3D.Create( -5, +3, -10 ),    // Camera is at (-5,3,-10), in World Space
                                     TSingle3D.Create(  0,  0,   0 ),    // and looks at the origin
                                     TSingle3D.Create(  0, -1,   0 ) );  // Head is up (set to 0,-1,0 to look upside-down)

     Vulkan_.Info.Model := TSingleM4.Identity;
     // Vulkan clip space has inverted Y and half Z.
     Vulkan_.Info.Clip := TSingleM4.Create( +1.0,  0.0,  0.0,  0.0,
                                      0.0, -1.0,  0.0,  0.0,
                                      0.0,  0.0, +0.5, +0.5,
                                      0.0,  0.0,  0.0, +1.0 );

     Vulkan_.Info.MVP := Vulkan_.Info.Clip * Vulkan_.Info.Projection * Vulkan_.Info.View * Vulkan_.Info.Model;

     (* VULKAN_KEY_START *)
     buf_info                       := Default( VkBufferCreateInfo );
     buf_info.sType                 := VK_STRUCTURE_TYPE_BUFFER_CREATE_INFO;
     buf_info.pNext                 := nil;
     buf_info.usage                 := Ord( VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT );
     buf_info.size                  := SizeOf( Vulkan_.Info.MVP );
     buf_info.queueFamilyIndexCount := 0;
     buf_info.pQueueFamilyIndices   := nil;
     buf_info.sharingMode           := VK_SHARING_MODE_EXCLUSIVE;
     buf_info.flags                 := 0;
     res := vkCreateBuffer( Vulkan_.Info.device, @buf_info, nil, @Vulkan_.Info.uniform_data.buf );
     Assert( res = VK_SUCCESS );

     vkGetBufferMemoryRequirements( Vulkan_.Info.device, Vulkan_.Info.uniform_data.buf, @mem_reqs );

     alloc_info                 := Default( VkMemoryAllocateInfo );
     alloc_info.sType           := VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO;
     alloc_info.pNext           := nil;
     alloc_info.memoryTypeIndex := 0;

     alloc_info.allocationSize := mem_reqs.size;
     pass := memory_type_from_properties( Vulkan_.Info, mem_reqs.memoryTypeBits,
                                          Ord( VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT ) or Ord( VK_MEMORY_PROPERTY_HOST_COHERENT_BIT ),
                                          alloc_info.memoryTypeIndex );
     Assert( pass, 'No mappable, coherent memory' );

     res := vkAllocateMemory( Vulkan_.Info.device, @alloc_info, nil, @Vulkan_.Info.uniform_data.mem );
     Assert( res = VK_SUCCESS );

     res := vkMapMemory( Vulkan_.Info.device, Vulkan_.Info.uniform_data.mem, 0, mem_reqs.size, 0, @pData );
     Assert( res = VK_SUCCESS );

     Move( Vulkan_.Info.MVP, pData^, SizeOf( Vulkan_.Info.MVP ) );

     vkUnmapMemory( Vulkan_.Info.device, Vulkan_.Info.uniform_data.mem );

     res := vkBindBufferMemory( Vulkan_.Info.device, Vulkan_.Info.uniform_data.buf, Vulkan_.Info.uniform_data.mem, 0 );
     Assert( res = VK_SUCCESS );

     Vulkan_.Info.uniform_data.buffer_info.buffer := Vulkan_.Info.uniform_data.buf;
     Vulkan_.Info.uniform_data.buffer_info.offset := 0;
     Vulkan_.Info.uniform_data.buffer_info.range  := SizeOf( Vulkan_.Info.MVP );
end;

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
     res := vkCreateDescriptorSetLayout( Vulkan_.Info.device, @descriptor_layout, nil, @Vulkan_.Info.desc_layout[0] );
     Assert( res = VK_SUCCESS );

     (* Now use the descriptor layout to create a pipeline layout *)
     pPipelineLayoutCreateInfo                        := Default( VkPipelineLayoutCreateInfo );
     pPipelineLayoutCreateInfo.sType                  := VK_STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO;
     pPipelineLayoutCreateInfo.pNext                  := nil;
     pPipelineLayoutCreateInfo.pushConstantRangeCount := 0;
     pPipelineLayoutCreateInfo.pPushConstantRanges    := nil;
     pPipelineLayoutCreateInfo.setLayoutCount         := NUM_DESCRIPTOR_SETS;
     pPipelineLayoutCreateInfo.pSetLayouts            := @Vulkan_.Info.desc_layout[0];

     res := vkCreatePipelineLayout( Vulkan_.Info.device, @pPipelineLayoutCreateInfo, nil, @Vulkan_.Info.pipeline_layout );
     Assert( res = VK_SUCCESS );
end;

procedure destroy_uniform_buffer( const Vulkan_:TVulkan );
begin
     vkDestroyBuffer( Vulkan_.Info.device, Vulkan_.Info.uniform_data.buf, nil );
     vkFreeMemory( Vulkan_.Info.device, Vulkan_.Info.uniform_data.mem, nil );
end;

procedure destroy_descriptor_and_pipeline_layouts( const Vulkan_:TVulkan );
var
   i :T_int;
begin
     for i := 0 to NUM_DESCRIPTOR_SETS-1 do vkDestroyDescriptorSetLayout( Vulkan_.Info.device, Vulkan_.Info.desc_layout[i], nil );
     vkDestroyPipelineLayout( Vulkan_.Info.device, Vulkan_.Info.pipeline_layout, nil );
end;

//////////////////////////////////////////////////////////////////////////////// 10-init_render_pass

procedure init_device_queue( const Vulkan_:TVulkan );
begin
     (* DEPENDS on init_swapchain_extension() *)

     vkGetDeviceQueue( Vulkan_.Info.device, Vulkan_.Info.graphics_queue_family_index, 0, @Vulkan_.Info.graphics_queue );
     if Vulkan_.Info.graphics_queue_family_index = Vulkan_.Info.present_queue_family_index
     then Vulkan_.Info.present_queue := Vulkan_.Info.graphics_queue
     else vkGetDeviceQueue( Vulkan_.Info.device, Vulkan_.Info.present_queue_family_index, 0, @Vulkan_.Info.present_queue );
end;

procedure init_swap_chain( const Vulkan_:TVulkan; usageFlags_:VkImageUsageFlags = Ord( VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT ) or Ord( VK_IMAGE_USAGE_TRANSFER_SRC_BIT ) );
var
   res                            :VkResult;
   surfCapabilities               :VkSurfaceCapabilitiesKHR;
   presentModeCount               :T_uint32_t;
   presentModes                   :TArray<VkPresentModeKHR>;
   swapchainExtent                :VkExtent2D;
   swapchainPresentMode           :VkPresentModeKHR;
   desiredNumberOfSwapChainImages :T_uint32_t;
   preTransform                   :VkSurfaceTransformFlagBitsKHR;
   compositeAlpha                 :VkCompositeAlphaFlagBitsKHR;
   compositeAlphaFlags            :array [ 0..4-1 ] of VkCompositeAlphaFlagBitsKHR;
   i                              :T_uint32_t;
   swapchain_ci                   :VkSwapchainCreateInfoKHR;
   queueFamilyIndices             :array [ 0..2-1 ] of T_uint32_t;
   swapchainImages                :TArray<VkImage>;
   sc_buffer                      :T_swap_chain_buffer;
   color_image_view               :VkImageViewCreateInfo;
begin
     (* DEPENDS on info.cmd and info.queue initialized *)

     res := vkGetPhysicalDeviceSurfaceCapabilitiesKHR( Vulkan_.Devices.Devices[ 0 ].Handle, Vulkan_.Info.surface, @surfCapabilities );
     Assert( res = VK_SUCCESS );

     res := vkGetPhysicalDeviceSurfacePresentModesKHR( Vulkan_.Devices.Devices[ 0 ].Handle, Vulkan_.Info.surface, @presentModeCount, nil );
     Assert( res = VK_SUCCESS );
     SetLength( presentModes, presentModeCount );
     Assert( Length( presentModes ) > 0 );
     res := vkGetPhysicalDeviceSurfacePresentModesKHR( Vulkan_.Devices.Devices[ 0 ].Handle, Vulkan_.Info.surface, @presentModeCount, @presentModes[0] );
     Assert( res = VK_SUCCESS );

     // width and height are either both 0xFFFFFFFF, or both not 0xFFFFFFFF.
     if surfCapabilities.currentExtent.width = $FFFFFFFF then
     begin
          // If the surface size is undefined, the size is set to
          // the size of the images requested.
          swapchainExtent.width  := Vulkan_.Info.width;
          swapchainExtent.height := Vulkan_.Info.height;
          if swapchainExtent.width < surfCapabilities.minImageExtent.width
          then swapchainExtent.width := surfCapabilities.minImageExtent.width
          else
          if swapchainExtent.width > surfCapabilities.maxImageExtent.width
          then swapchainExtent.width := surfCapabilities.maxImageExtent.width;

          if swapchainExtent.height < surfCapabilities.minImageExtent.height
          then swapchainExtent.height := surfCapabilities.minImageExtent.height
          else
          if swapchainExtent.height > surfCapabilities.maxImageExtent.height
          then swapchainExtent.height := surfCapabilities.maxImageExtent.height;
     end
     else
     begin
          // If the surface size is defined, the swap chain size must match
          swapchainExtent := surfCapabilities.currentExtent;
     end;

     // The FIFO present mode is guaranteed by the spec to be supported
     // Also note that current Android driver only supports FIFO
     swapchainPresentMode := VK_PRESENT_MODE_FIFO_KHR;

     // Determine the number of VkImage's to use in the swap chain.
     // We need to acquire only 1 presentable image at at time.
     // Asking for minImageCount images ensures that we can acquire
     // 1 presentable image as long as we present it before attempting
     // to acquire another.
     desiredNumberOfSwapChainImages := surfCapabilities.minImageCount;

     if ( surfCapabilities.supportedTransforms and Ord( VK_SURFACE_TRANSFORM_IDENTITY_BIT_KHR ) ) <> 0
     then preTransform := VK_SURFACE_TRANSFORM_IDENTITY_BIT_KHR
     else preTransform := surfCapabilities.currentTransform;

     // Find a supported composite alpha mode - one of these is guaranteed to be set
     compositeAlpha         := VK_COMPOSITE_ALPHA_OPAQUE_BIT_KHR;
     compositeAlphaFlags[0] := VK_COMPOSITE_ALPHA_OPAQUE_BIT_KHR;
     compositeAlphaFlags[1] := VK_COMPOSITE_ALPHA_PRE_MULTIPLIED_BIT_KHR;
     compositeAlphaFlags[2] := VK_COMPOSITE_ALPHA_POST_MULTIPLIED_BIT_KHR;
     compositeAlphaFlags[3] := VK_COMPOSITE_ALPHA_INHERIT_BIT_KHR;

     for i := 0 to Length( compositeAlphaFlags )-1 do
     begin
          if ( surfCapabilities.supportedCompositeAlpha and Ord( compositeAlphaFlags[i] ) ) <> 0 then
          begin
               compositeAlpha := compositeAlphaFlags[i];
               Break;
          end;
     end;

     swapchain_ci                       := Default( VkSwapchainCreateInfoKHR );
     swapchain_ci.sType                 := VK_STRUCTURE_TYPE_SWAPCHAIN_CREATE_INFO_KHR;
     swapchain_ci.pNext                 := nil;
     swapchain_ci.surface               := Vulkan_.Info.surface;
     swapchain_ci.minImageCount         := desiredNumberOfSwapChainImages;
     swapchain_ci.imageFormat           := Vulkan_.Info.format;
     swapchain_ci.imageExtent.width     := swapchainExtent.width;
     swapchain_ci.imageExtent.height    := swapchainExtent.height;
     swapchain_ci.preTransform          := preTransform;
     swapchain_ci.compositeAlpha        := compositeAlpha;
     swapchain_ci.imageArrayLayers      := 1;
     swapchain_ci.presentMode           := swapchainPresentMode;
     swapchain_ci.oldSwapchain          := VK_NULL_HANDLE;
     swapchain_ci.clipped               := 1;
     swapchain_ci.imageColorSpace       := VK_COLORSPACE_SRGB_NONLINEAR_KHR;
     swapchain_ci.imageUsage            := usageFlags_;
     swapchain_ci.imageSharingMode      := VK_SHARING_MODE_EXCLUSIVE;
     swapchain_ci.queueFamilyIndexCount := 0;
     swapchain_ci.pQueueFamilyIndices   := nil;
     queueFamilyIndices[0] := Vulkan_.Info.graphics_queue_family_index;
     queueFamilyIndices[1] := Vulkan_.Info.present_queue_family_index;
     if Vulkan_.Info.graphics_queue_family_index <> Vulkan_.Info.present_queue_family_index then
     begin
          // If the graphics and present queues are from different queue families,
          // we either have to explicitly transfer ownership of images between the
          // queues, or we have to create the swapchain with imageSharingMode
          // as VK_SHARING_MODE_CONCURRENT
          swapchain_ci.imageSharingMode      := VK_SHARING_MODE_CONCURRENT;
          swapchain_ci.queueFamilyIndexCount := 2;
          swapchain_ci.pQueueFamilyIndices   := @queueFamilyIndices[0];
     end;

     res := vkCreateSwapchainKHR( Vulkan_.Info.device, @swapchain_ci, nil, @Vulkan_.Info.swap_chain );
     Assert( res = VK_SUCCESS );

     res := vkGetSwapchainImagesKHR( Vulkan_.Info.device, Vulkan_.Info.swap_chain, @Vulkan_.Info.swapchainImageCount, nil );
     Assert( res = VK_SUCCESS );

     SetLength( swapchainImages, Vulkan_.Info.swapchainImageCount );
     Assert( Length( swapchainImages ) > 0 );
     res := vkGetSwapchainImagesKHR( Vulkan_.Info.device, Vulkan_.Info.swap_chain, @Vulkan_.Info.swapchainImageCount, @swapchainImages[0] );
     Assert( res = VK_SUCCESS );

     for i := 0 to Vulkan_.Info.swapchainImageCount-1 do
     begin
          color_image_view                                 := Default( VkImageViewCreateInfo );
          color_image_view.sType                           := VK_STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO;
          color_image_view.pNext                           := nil;
          color_image_view.format                          := Vulkan_.Info.format;
          color_image_view.components.r                    := VK_COMPONENT_SWIZZLE_R;
          color_image_view.components.g                    := VK_COMPONENT_SWIZZLE_G;
          color_image_view.components.b                    := VK_COMPONENT_SWIZZLE_B;
          color_image_view.components.a                    := VK_COMPONENT_SWIZZLE_A;
          color_image_view.subresourceRange.aspectMask     := Ord( VK_IMAGE_ASPECT_COLOR_BIT );
          color_image_view.subresourceRange.baseMipLevel   := 0;
          color_image_view.subresourceRange.levelCount     := 1;
          color_image_view.subresourceRange.baseArrayLayer := 0;
          color_image_view.subresourceRange.layerCount     := 1;
          color_image_view.viewType                        := VK_IMAGE_VIEW_TYPE_2D;
          color_image_view.flags                           := 0;

          sc_buffer.image := swapchainImages[i];

          color_image_view.image := sc_buffer.image;

          res := vkCreateImageView( Vulkan_.Info.device, @color_image_view, nil, @sc_buffer.view );
          Vulkan_.Info.buffers := Vulkan_.Info.buffers + [ sc_buffer ];
          Assert( res = VK_SUCCESS );
     end;
     swapchainImages := nil;
     Vulkan_.Info.current_buffer := 0;

     if nil <> presentModes then presentModes := nil;
end;

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
     vkGetPhysicalDeviceFormatProperties( Vulkan_.Devices.Devices[ 0 ].Handle, depth_format, @props );
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
     image_info.extent.width          := Vulkan_.Info.width;
     image_info.extent.height         := Vulkan_.Info.height;
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
     res := vkCreateImage( Vulkan_.Info.device, @image_info, nil, @Vulkan_.Info.depth.image );
     Assert( res = VK_SUCCESS );

     vkGetImageMemoryRequirements( Vulkan_.Info.device, Vulkan_.Info.depth.image, @mem_reqs );

     mem_alloc.allocationSize := mem_reqs.size;
     (* Use the memory properties to determine the type of memory required *)
     pass := memory_type_from_properties( Vulkan_.Info, mem_reqs.memoryTypeBits, Ord( VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT ), mem_alloc.memoryTypeIndex );
     Assert( pass );

     (* Allocate memory *)
     res := vkAllocateMemory( Vulkan_.Info.device, @mem_alloc, nil, @Vulkan_.Info.depth.mem );
     Assert( res = VK_SUCCESS );

     (* Bind memory *)
     res := vkBindImageMemory( Vulkan_.Info.device, Vulkan_.Info.depth.image, Vulkan_.Info.depth.mem, 0 );
     Assert( res = VK_SUCCESS );

     (* Create image view *)
     view_info.image := Vulkan_.Info.depth.image;
     res := vkCreateImageView( Vulkan_.Info.device, @view_info, nil, @Vulkan_.Info.depth.view );
     Assert( res = VK_SUCCESS );
end;

procedure destroy_depth_buffer( const Vulkan_:TVulkan );
begin
     vkDestroyImageView( Vulkan_.Info.device, Vulkan_.Info.depth.view, nil );
     vkDestroyImage( Vulkan_.Info.device, Vulkan_.Info.depth.image, nil );
     vkFreeMemory( Vulkan_.Info.device, Vulkan_.Info.depth.mem, nil );
end;

procedure destroy_swap_chain( const Vulkan_:TVulkan );
var
   I :T_uint32_t;
begin
     for i := 0 to Vulkan_.Info.swapchainImageCount-1 do vkDestroyImageView( Vulkan_.Info.device, Vulkan_.Info.buffers[i].view, nil );
     vkDestroySwapchainKHR( Vulkan_.Info.device, Vulkan_.Info.swap_chain, nil );
end;

//////////////////////////////////////////////////////////////////////////////// 11-init_shaders

//////////////////////////////////////////////////////////////////////////////// 12-init_frame_buffers

procedure init_command_pool( const Vulkan_:TVulkan );
var
   res           :VkResult;
   cmd_pool_info :VkCommandPoolCreateInfo;
begin
     (* DEPENDS on init_swapchain_extension() *)

     cmd_pool_info                  := Default( VkCommandPoolCreateInfo );
     cmd_pool_info.sType            := VK_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO;
     cmd_pool_info.pNext            := nil;
     cmd_pool_info.queueFamilyIndex := Vulkan_.Info.graphics_queue_family_index;
     cmd_pool_info.flags            := Ord( VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT );

     res := vkCreateCommandPool( Vulkan_.Info.device, @cmd_pool_info, nil, @Vulkan_.Info.cmd_pool );
     Assert( res = VK_SUCCESS );
end;

procedure init_command_buffer( const Vulkan_:TVulkan );
var
   res :VkResult;
   cmd :VkCommandBufferAllocateInfo;
begin
     (* DEPENDS on init_swapchain_extension() and init_command_pool() *)

     cmd                    := Default( VkCommandBufferAllocateInfo );
     cmd.sType              := VK_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO;
     cmd.pNext              := nil;
     cmd.commandPool        := Vulkan_.Info.cmd_pool;
     cmd.level              := VK_COMMAND_BUFFER_LEVEL_PRIMARY;
     cmd.commandBufferCount := 1;

     res := vkAllocateCommandBuffers( Vulkan_.Info.device, @cmd, @Vulkan_.Info.cmd );
     Assert( res = VK_SUCCESS );
end;

procedure execute_begin_command_buffer( const Vulkan_:TVulkan );
var
   res          :VkResult;
   cmd_buf_info :VkCommandBufferBeginInfo;
begin
     (* DEPENDS on init_command_buffer() *)

     cmd_buf_info                  := Default( VkCommandBufferBeginInfo );
     cmd_buf_info.sType            := VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO;
     cmd_buf_info.pNext            := nil;
     cmd_buf_info.flags            := 0;
     cmd_buf_info.pInheritanceInfo := nil;

     res := vkBeginCommandBuffer( Vulkan_.Info.cmd, @cmd_buf_info );
     Assert( res = VK_SUCCESS );
end;

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
     attachments[0].format         := Vulkan_.Info.format;
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

     res := vkCreateRenderPass( Vulkan_.Info.device, @rp_info, nil, @Vulkan_.Info.render_pass );
     Assert( res = VK_SUCCESS );
end;

procedure execute_end_command_buffer( const Vulkan_:TVulkan );
var
   res :VkResult;
begin
     res := vkEndCommandBuffer( Vulkan_.Info.cmd );
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
     cmd_bufs[0] := Vulkan_.Info.cmd;
     fenceInfo.sType := VK_STRUCTURE_TYPE_FENCE_CREATE_INFO;
     fenceInfo.pNext := nil;
     fenceInfo.flags := 0;
     vkCreateFence( Vulkan_.Info.device, @fenceInfo, nil, @drawFence );

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

     res := vkQueueSubmit( Vulkan_.Info.graphics_queue, 1, @submit_info[0], drawFence );
     Assert( res = VK_SUCCESS );

     repeat
           res := vkWaitForFences( Vulkan_.Info.device, 1, @drawFence, VK_TRUE, FENCE_TIMEOUT );

     until res <> VK_TIMEOUT;
     Assert( res = VK_SUCCESS );

     vkDestroyFence( Vulkan_.Info.device, drawFence, nil );
end;

procedure destroy_renderpass( const Vulkan_:TVulkan );
begin
     vkDestroyRenderPass( Vulkan_.Info.device, Vulkan_.Info.render_pass, nil );
end;

procedure destroy_command_buffer( const Vulkan_:TVulkan );
var
   cmd_bufs :array [ 0..1-1 ] of VkCommandBuffer;
begin
     cmd_bufs[0] := Vulkan_.Info.cmd;
     vkFreeCommandBuffers( Vulkan_.Info.device, Vulkan_.Info.cmd_pool, 1, @cmd_bufs[0] );
end;

procedure destroy_command_pool( const Vulkan_:TVulkan );
begin
     vkDestroyCommandPool( Vulkan_.Info.device, Vulkan_.Info.cmd_pool, nil );
end;

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
     fb_info.width                := Vulkan_.Info.width;
     fb_info.height               := Vulkan_.Info.height;
     fb_info.layers               := 1;

     SetLength( Vulkan_.Info.framebuffers, Vulkan_.Info.swapchainImageCount );

     for i := 0 to Vulkan_.Info.swapchainImageCount-1 do
     begin
          attachments[0] := Vulkan_.Info.buffers[i].view;
          res := vkCreateFramebuffer( Vulkan_.Info.device, @fb_info, nil, @Vulkan_.Info.framebuffers[i] );
          Assert( res = VK_SUCCESS );
     end;
end;

procedure destroy_framebuffers( const Vulkan_:TVulkan );
var
   i :T_uint32_t;
begin
     for i := 0 to Vulkan_.Info.swapchainImageCount-1 do vkDestroyFramebuffer( Vulkan_.Info.device, Vulkan_.Info.framebuffers[i], nil );
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
     res := vkCreateBuffer( Vulkan_.Info.device, @buf_info, nil, @Vulkan_.Info.vertex_buffer.buf );
     Assert( res = VK_SUCCESS );

     vkGetBufferMemoryRequirements( Vulkan_.Info.device, Vulkan_.Info.vertex_buffer.buf, @mem_reqs );

     alloc_info                 := Default( VkMemoryAllocateInfo );
     alloc_info.sType           := VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO;
     alloc_info.pNext           := nil;
     alloc_info.memoryTypeIndex := 0;

     alloc_info.allocationSize := mem_reqs.size;
     pass := memory_type_from_properties( Vulkan_.Info, mem_reqs.memoryTypeBits,
                                          Ord( VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT ) or Ord( VK_MEMORY_PROPERTY_HOST_COHERENT_BIT ),
                                          alloc_info.memoryTypeIndex );
     Assert( pass, 'No mappable, coherent memory' );

     res := vkAllocateMemory( Vulkan_.Info.device, @alloc_info, nil, @Vulkan_.Info.vertex_buffer.mem );
     Assert( res = VK_SUCCESS );
     Vulkan_.Info.vertex_buffer.buffer_info.range  := mem_reqs.size;
     Vulkan_.Info.vertex_buffer.buffer_info.offset := 0;

     res := vkMapMemory( Vulkan_.Info.device, Vulkan_.Info.vertex_buffer.mem, 0, mem_reqs.size, 0, @pData );
     Assert( res = VK_SUCCESS );

     Move( vertexData_^, pData^, dataSize_ );

     vkUnmapMemory( Vulkan_.Info.device, Vulkan_.Info.vertex_buffer.mem );

     res := vkBindBufferMemory( Vulkan_.Info.device, Vulkan_.Info.vertex_buffer.buf, Vulkan_.Info.vertex_buffer.mem, 0 );
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

     res := vkCreateDescriptorPool( Vulkan_.Info.device, @descriptor_pool, nil, @Vulkan_.Info.desc_pool );
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
     res := vkAllocateDescriptorSets( Vulkan_.Info.device, @alloc_info[0], @Vulkan_.Info.desc_set[0] );
     Assert( res = VK_SUCCESS );

     writes[0]                 := Default( VkWriteDescriptorSet );
     writes[0].sType           := VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET;
     writes[0].pNext           := nil;
     writes[0].dstSet          := Vulkan_.Info.desc_set[0];
     writes[0].descriptorCount := 1;
     writes[0].descriptorType  := VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER;
     writes[0].pBufferInfo     := @Vulkan_.Info.uniform_data.buffer_info;
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
     then vkUpdateDescriptorSets( Vulkan_.Info.device, 2, @writes[0], 0, nil )
     else vkUpdateDescriptorSets( Vulkan_.Info.device, 1, @writes[0], 0, nil );
end;

procedure destroy_descriptor_pool( const Vulkan_:TVulkan );
begin
     vkDestroyDescriptorPool( Vulkan_.Info.device, Vulkan_.Info.desc_pool, nil );
end;

procedure destroy_vertex_buffer( const Vulkan_:TVulkan );
begin
     vkDestroyBuffer( Vulkan_.Info.device, Vulkan_.Info.vertex_buffer.buf, nil );
     vkFreeMemory( Vulkan_.Info.device, Vulkan_.Info.vertex_buffer.mem, nil );
end;

//////////////////////////////////////////////////////////////////////////////// 15-draw_cube

procedure init_viewports( const Vulkan_:TVulkan );
begin
     Vulkan_.Info.viewport.height   := Vulkan_.Info.height;
     Vulkan_.Info.viewport.width    := Vulkan_.Info.width;
     Vulkan_.Info.viewport.minDepth := 0.0;
     Vulkan_.Info.viewport.maxDepth := 1.0;
     Vulkan_.Info.viewport.x        := 0;
     Vulkan_.Info.viewport.y        := 0;
     vkCmdSetViewport( Vulkan_.Info.cmd, 0, NUM_VIEWPORTS, @Vulkan_.Info.viewport );
end;

procedure init_scissors( const Vulkan_:TVulkan );
begin
     Vulkan_.Info.scissor.extent.width  := Vulkan_.Info.width;
     Vulkan_.Info.scissor.extent.height := Vulkan_.Info.height;
     Vulkan_.Info.scissor.offset.x      := 0;
     Vulkan_.Info.scissor.offset.y      := 0;
     vkCmdSetScissor( Vulkan_.Info.cmd, 0, NUM_SCISSORS, @Vulkan_.Info.scissor );
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
     res := vkCreatePipelineCache( Vulkan_.Info.device, @pipelineCache, nil, @Vulkan_.Info.pipelineCache );
     Assert( res = VK_SUCCESS );
end;

procedure destroy_pipeline_cache( const Vulkan_:TVulkan );
begin
     vkDestroyPipelineCache( Vulkan_.Info.device, Vulkan_.Info.pipelineCache, nil );
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
     res := vkCreateBuffer( Vulkan_.Info.device, @buffer_create_info, nil, @texObj_.buffer );
     Assert( res = VK_SUCCESS );

     mem_alloc                 := Default( VkMemoryAllocateInfo );
     mem_alloc.sType           := VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO;
     mem_alloc.pNext           := nil;
     mem_alloc.allocationSize  := 0;
     mem_alloc.memoryTypeIndex := 0;

     vkGetBufferMemoryRequirements( Vulkan_.Info.device, texObj_.buffer, @mem_reqs );
     mem_alloc.allocationSize := mem_reqs.size;
     texObj_.buffer_size := mem_reqs.size;

     requirements := Ord( VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT ) or Ord( VK_MEMORY_PROPERTY_HOST_COHERENT_BIT );
     pass := memory_type_from_properties( Vulkan_.Info, mem_reqs.memoryTypeBits, requirements, mem_alloc.memoryTypeIndex );
     Assert( pass, '"No mappable, coherent memory' );

     (* allocate memory *)
     res := vkAllocateMemory(Vulkan_.Info.device, @mem_alloc, nil, @( texObj_.buffer_memory) );
     Assert( res = VK_SUCCESS );

     (* bind memory *)
     res := vkBindBufferMemory( Vulkan_.Info.device, texObj_.buffer, texObj_.buffer_memory, 0 );
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
   cmd_buf_info      :VkCommandBufferBeginInfo;
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

     vkGetPhysicalDeviceFormatProperties( Vulkan_.Devices.Devices[ 0 ].Handle, VK_FORMAT_R8G8B8A8_UNORM, @formatProps );

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

     res := vkCreateImage( Vulkan_.Info.device, @image_create_info, nil, @texObj_.image );
     Assert( res = VK_SUCCESS );

     vkGetImageMemoryRequirements( Vulkan_.Info.device, texObj_.image, @mem_reqs );

     mem_alloc.allocationSize := mem_reqs.size;

     if texObj_.needs_staging
     then requirements := 0
     else requirements := Ord( VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT ) or Ord( VK_MEMORY_PROPERTY_HOST_COHERENT_BIT );
     pass := memory_type_from_properties( Vulkan_.Info, mem_reqs.memoryTypeBits, requirements, mem_alloc.memoryTypeIndex );
     Assert( pass );

     (* allocate memory *)
     res := vkAllocateMemory(Vulkan_.Info.device, @mem_alloc, nil, @( texObj_.image_memory) );
     Assert( res = VK_SUCCESS );

     (* bind memory *)
     res := vkBindImageMemory( Vulkan_.Info.device, texObj_.image, texObj_.image_memory, 0 );
     Assert( res = VK_SUCCESS );

     res := vkEndCommandBuffer( Vulkan_.Info.cmd );
     Assert( res = VK_SUCCESS );
     cmd_bufs[0] := Vulkan_.Info.cmd;
     fenceInfo.sType := VK_STRUCTURE_TYPE_FENCE_CREATE_INFO;
     fenceInfo.pNext := nil;
     fenceInfo.flags := 0;
     vkCreateFence( Vulkan_.Info.device, @fenceInfo, nil, @cmdFence );

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
     res := vkQueueSubmit( Vulkan_.Info.graphics_queue, 1, @submit_info[0], cmdFence );
     Assert( res = VK_SUCCESS );

     subres            := Default( VkImageSubresource );
     subres.aspectMask := Ord( VK_IMAGE_ASPECT_COLOR_BIT );
     subres.mipLevel   := 0;
     subres.arrayLayer := 0;

     layout := Default( VkSubresourceLayout );
     if not texObj_.needs_staging then
     begin
          (* Get the subresource layout so we know what the row pitch is *)
          vkGetImageSubresourceLayout( Vulkan_.Info.device, texObj_.image, @subres, @layout );
     end;

     (* Make sure command buffer is finished before mapping *)
     repeat
           res := vkWaitForFences( Vulkan_.Info.device, 1, @cmdFence, VK_TRUE, FENCE_TIMEOUT );

     until res <> VK_TIMEOUT;
     Assert( res = VK_SUCCESS );

     vkDestroyFence( Vulkan_.Info.device, cmdFence, nil );

     if texObj_.needs_staging
     then res := vkMapMemory( Vulkan_.Info.device, texObj_.buffer_memory, 0, texObj_.buffer_size, 0, @data )
     else res := vkMapMemory( Vulkan_.Info.device, texObj_.image_memory , 0, mem_reqs.size     , 0, @data );
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
     then vkUnmapMemory( Vulkan_.Info.device, texObj_.buffer_memory )
     else vkUnmapMemory( Vulkan_.Info.device, texObj_.image_memory  );

     cmd_buf_info                  := Default( VkCommandBufferBeginInfo );
     cmd_buf_info.sType            := VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO;
     cmd_buf_info.pNext            := nil;
     cmd_buf_info.flags            := 0;
     cmd_buf_info.pInheritanceInfo := nil;

     res := vkResetCommandBuffer( Vulkan_.Info.cmd, 0 );
     Assert( res = VK_SUCCESS );
     res := vkBeginCommandBuffer( Vulkan_.Info.cmd, @cmd_buf_info );
     Assert( res = VK_SUCCESS );

     if not texObj_.needs_staging then
     begin
          (* If we can use the linear tiled image as a texture, just do it *)
          texObj_.imageLayout := VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL;
          set_image_layout( Vulkan_.Info, texObj_.image, Ord( VK_IMAGE_ASPECT_COLOR_BIT ), VK_IMAGE_LAYOUT_PREINITIALIZED, texObj_.imageLayout,
                            Ord( VK_PIPELINE_STAGE_HOST_BIT ), Ord( VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT ) );
     end
     else
     begin
          (* Since we're going to blit to the texture image, set its layout to
           * DESTINATION_OPTIMAL *)
          set_image_layout( Vulkan_.Info, texObj_.image, Ord( VK_IMAGE_ASPECT_COLOR_BIT ), VK_IMAGE_LAYOUT_UNDEFINED,
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
          vkCmdCopyBufferToImage( Vulkan_.Info.cmd, texObj_.buffer, texObj_.image, VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL, 1, @copy_region );

          (* Set the layout for the texture image from DESTINATION_OPTIMAL to
           * SHADER_READ_ONLY *)
          texObj_.imageLayout := VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL;
          set_image_layout( Vulkan_.Info, texObj_.image, Ord( VK_IMAGE_ASPECT_COLOR_BIT ), VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL, texObj_.imageLayout,
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
     res := vkCreateImageView( Vulkan_.Info.device, @view_info, nil, @texObj_.view );
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
     res := vkCreateSampler( Vulkan_.Info.device, @samplerCreateInfo, nil, @sampler_ );
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
          vkDestroySampler  ( Vulkan_.Info.device, Vulkan_.Info.textures[i].sampler      , nil );
          vkDestroyImageView( Vulkan_.Info.device, Vulkan_.Info.textures[i].view         , nil );
          vkDestroyImage    ( Vulkan_.Info.device, Vulkan_.Info.textures[i].image        , nil );
          vkFreeMemory      ( Vulkan_.Info.device, Vulkan_.Info.textures[i].image_memory , nil );
          vkDestroyBuffer   ( Vulkan_.Info.device, Vulkan_.Info.textures[i].buffer       , nil );
          vkFreeMemory      ( Vulkan_.Info.device, Vulkan_.Info.textures[i].buffer_memory, nil );
     end;
end;

end. //######################################################################### ■