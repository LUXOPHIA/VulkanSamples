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
     LUX.Code.C;

//type //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【型】

     //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【レコード】

     //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【クラス】

//const //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【定数】

//var //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【変数】

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【ルーチン】

//////////////////////////////////////////////////////////////////////////////// 01-init_instance

function init_global_extension_properties( var layer_props_:T_layer_properties ) :VkResult;
function init_global_layer_properties( var info_:T_sample_info ) :VkResult;

//////////////////////////////////////////////////////////////////////////////// 02-enumerate_devices

function init_instance( var info_:T_sample_info; const app_short_name_:P_char ) :VkResult;

//////////////////////////////////////////////////////////////////////////////// 03-init_device

function init_device_extension_properties( var info_:T_sample_info; var layer_props_:T_layer_properties ) :VkResult;
function init_enumerate_device( var info_:T_sample_info; gpu_count_:T_uint32_t = 1 ) :VkResult;
procedure destroy_instance( var info_:T_sample_info );

//////////////////////////////////////////////////////////////////////////////// 04-init_command_buffer

procedure init_queue_family_index( var info_:T_sample_info );
function init_device( var info_:T_sample_info ) :VkResult;
procedure destroy_device( var info_:T_sample_info );

//////////////////////////////////////////////////////////////////////////////// 05-init_swapchain

procedure init_instance_extension_names( var info_:T_sample_info );
procedure init_device_extension_names( var info_:T_sample_info );
procedure init_window_size( var info_:T_sample_info; default_width_,default_height_:UInt32 );
procedure init_connection( var info_:T_sample_info );
procedure init_window( var info_:T_sample_info );
procedure destroy_window( var info_:T_sample_info );

//////////////////////////////////////////////////////////////////////////////// 06-init_depth_buffer

procedure init_swapchain_extension( var info_:T_sample_info );

//////////////////////////////////////////////////////////////////////////////// 07-init_uniform_buffer

//////////////////////////////////////////////////////////////////////////////// 08-init_pipeline_layout

//////////////////////////////////////////////////////////////////////////////// 09-init_descriptor_set

procedure init_uniform_buffer( var info_:T_sample_info );
procedure init_descriptor_and_pipeline_layouts( var info_:T_sample_info; use_texture_:T_bool; descSetLayoutCreateFlags_:VkDescriptorSetLayoutCreateFlags = 0 );
procedure destroy_uniform_buffer( var info_:T_sample_info );
procedure destroy_descriptor_and_pipeline_layouts( var info_:T_sample_info );

//////////////////////////////////////////////////////////////////////////////// 10-init_render_pass

procedure init_device_queue( var info_:T_sample_info );
procedure init_swap_chain( var info_:T_sample_info; usageFlags_:VkImageUsageFlags = Ord( VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT ) or Ord( VK_IMAGE_USAGE_TRANSFER_SRC_BIT ) );
procedure init_depth_buffer( var info_:T_sample_info );
procedure destroy_depth_buffer( var info_:T_sample_info );
procedure destroy_swap_chain( var info_:T_sample_info );

//////////////////////////////////////////////////////////////////////////////// 11-init_shaders

//////////////////////////////////////////////////////////////////////////////// 12-init_frame_buffers

procedure init_command_pool( var info_:T_sample_info );
procedure init_command_buffer( var info_:T_sample_info );
procedure execute_begin_command_buffer( var info_:T_sample_info );
procedure init_renderpass( var info_:T_sample_info;
                           include_depth_:T_bool;
                           clear_:T_bool = True;
                           finalLayout_:VkImageLayout = VK_IMAGE_LAYOUT_PRESENT_SRC_KHR;
                           initialLayout_:VkImageLayout = VK_IMAGE_LAYOUT_UNDEFINED );
procedure execute_end_command_buffer( var info_:T_sample_info );
procedure execute_queue_command_buffer( var info_:T_sample_info );
procedure destroy_renderpass( var info_:T_sample_info );
procedure destroy_command_buffer( var info_:T_sample_info );
procedure destroy_command_pool( var info_:T_sample_info );

//////////////////////////////////////////////////////////////////////////////// 13-init_vertex_buffer

procedure init_framebuffers( var info_:T_sample_info; include_depth:T_bool );
procedure destroy_framebuffers( var info_:T_sample_info );

//////////////////////////////////////////////////////////////////////////////// 14-init_pipeline

procedure init_vertex_buffer( var info_:T_sample_info; const vertexData_:P_void; dataSize_:T_uint32_t; dataStride_:T_uint32_t; use_texture_:T_bool );
procedure init_descriptor_pool( var info_:T_sample_info; use_texture_:T_bool );
procedure init_descriptor_set( var info_:T_sample_info; use_texture_:T_bool );
procedure init_shaders( var info_:T_sample_info; const vertShaderCI_:P_VkShaderModuleCreateInfo; const fragShaderCI_:P_VkShaderModuleCreateInfo );
procedure destroy_descriptor_pool( var info_:T_sample_info );
procedure destroy_vertex_buffer( var info_:T_sample_info );
procedure destroy_shaders( var info_:T_sample_info );

//////////////////////////////////////////////////////////////////////////////// 15-draw_cube

procedure init_viewports( var info_:T_sample_info );
procedure init_scissors( var info_:T_sample_info );
procedure init_pipeline_cache( var info:T_sample_info );
procedure init_pipeline( var info:T_sample_info; include_depth:VkBool32; include_vi:VkBool32 = 1 );
procedure destroy_pipeline( var info_:T_sample_info );
procedure destroy_pipeline_cache( var info_:T_sample_info );

//////////////////////////////////////////////////////////////////////////////// draw_textured_cube

procedure init_buffer( var info_:T_sample_info; var texObj_:T_texture_object );
procedure init_image( var info_:T_sample_info; var texObj_:T_texture_object; const textureName_:String; extraUsages_:VkImageUsageFlags; extraFeatures_:VkFormatFeatureFlags );
procedure init_sampler( var info_:T_sample_info; var sampler_:VkSampler );
procedure init_texture( var info_:T_sample_info; const textureName_:String = ''; extraUsages_:VkImageUsageFlags = 0; extraFeatures_:VkFormatFeatureFlags = 0 );
procedure destroy_textures( var info_:T_sample_info );

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

function init_global_layer_properties( var info_:T_sample_info ) :VkResult;
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
          info_.instance_layer_properties := info_.instance_layer_properties + [ layer_props ];
     end;
     vk_props := nil;
end;

//////////////////////////////////////////////////////////////////////////////// 02-enumerate_devices

function init_instance( var info_:T_sample_info; const app_short_name_:P_char ) :VkResult;
var
   app_info  :VkApplicationInfo;
   inst_info :VkInstanceCreateInfo;
begin
     app_info                    := Default( VkApplicationInfo );
     app_info.sType              := VK_STRUCTURE_TYPE_APPLICATION_INFO;
     app_info.pNext              := nil;
     app_info.pApplicationName   := app_short_name_;
     app_info.applicationVersion := 1;
     app_info.pEngineName        := app_short_name_;
     app_info.engineVersion      := 1;
     app_info.apiVersion         := VK_API_VERSION_1_0;

     inst_info                          := Default( VkInstanceCreateInfo );
     inst_info.sType                    := VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO;
     inst_info.pNext                    := nil;
     inst_info.flags                    := 0;
     inst_info.pApplicationInfo         := @app_info;
     inst_info.enabledLayerCount        := Length( info_.instance_layer_names );
     if Length( info_.instance_layer_names ) > 0
     then inst_info.ppEnabledLayerNames := @info_.instance_layer_names[0]
     else inst_info.ppEnabledLayerNames := nil;
     inst_info.enabledExtensionCount    := Length( info_.instance_extension_names );
     inst_info.ppEnabledExtensionNames  := @info_.instance_extension_names[0];

     Result := vkCreateInstance( @inst_info, nil, @info_.inst );
     Assert( Result = VK_SUCCESS );
end;

//////////////////////////////////////////////////////////////////////////////// 03-init_device

function init_device_extension_properties( var info_:T_sample_info; var layer_props_:T_layer_properties ) :VkResult;
var
   device_extensions      :P_VkExtensionProperties;
   device_extension_count :T_uint32_t;
   layer_name             :P_char;
begin
     layer_name := layer_props_.properties.layerName;

     repeat
           Result := vkEnumerateDeviceExtensionProperties( info_.gpus[0], layer_name, @device_extension_count, nil );
           if Result <> VK_SUCCESS then Exit;

           if device_extension_count = 0 then Exit( VK_SUCCESS );

           SetLength( layer_props_.device_extensions, device_extension_count );
           device_extensions := @layer_props_.device_extensions[0];
           Result := vkEnumerateDeviceExtensionProperties( info_.gpus[0], layer_name, @device_extension_count, device_extensions );

     until Result <> VK_INCOMPLETE;
end;

function init_enumerate_device( var info_:T_sample_info; gpu_count_:T_uint32_t = 1 ) :VkResult;
var
   req_count :T_uint32_t;
   I         :Integer;
begin
     req_count := gpu_count_;
     Result := vkEnumeratePhysicalDevices( info_.inst, @gpu_count_, nil );
     Assert( ( Result = VK_SUCCESS ) and ( gpu_count_ > 0 ) );
     SetLength( info_.gpus, gpu_count_ );

     Result := vkEnumeratePhysicalDevices( info_.inst, @gpu_count_, @info_.gpus[0] );
     Assert( ( Result = VK_SUCCESS ) and ( gpu_count_ >= req_count ) );

     vkGetPhysicalDeviceQueueFamilyProperties( info_.gpus[0], @info_.queue_family_count, nil );
     Assert( info_.queue_family_count >= 1 );

     SetLength( info_.queue_props, info_.queue_family_count );
     vkGetPhysicalDeviceQueueFamilyProperties( info_.gpus[0], @info_.queue_family_count, @info_.queue_props[0] );
     Assert( info_.queue_family_count >= 1 );

     (* This is as good a place as any to do this *)
     vkGetPhysicalDeviceMemoryProperties( info_.gpus[0], @info_.memory_properties );
     vkGetPhysicalDeviceProperties( info_.gpus[0], @info_.gpu_props );
     (* query device extensions for enabled layers *)
     for I := 0 to Length( info_.instance_layer_properties )-1
     do init_device_extension_properties( info_, info_.instance_layer_properties[I] );
end;

procedure destroy_instance( var info_:T_sample_info );
begin
     vkDestroyInstance( info_.inst, nil );
end;

//////////////////////////////////////////////////////////////////////////////// 04-init_command_buffer

procedure init_queue_family_index( var info_:T_sample_info );
var
   found :T_bool;
   i     :T_unsigned_int;
begin
     (* This routine simply finds a graphics queue for a later vkCreateDevice,
      * without consideration for which queue family can present an image.
      * Do not use this if your intent is to present later in your sample,
      * instead use the init_connection, init_window, init_swapchain_extension,
      * init_device call sequence to get a graphics and present compatible queue
      * family
      *)

     vkGetPhysicalDeviceQueueFamilyProperties( info_.gpus[0], @info_.queue_family_count, nil );
     Assert( info_.queue_family_count >= 1 );

     SetLength( info_.queue_props, info_.queue_family_count );
     vkGetPhysicalDeviceQueueFamilyProperties( info_.gpus[0], @info_.queue_family_count, @info_.queue_props[0] );
     Assert( info_.queue_family_count >= 1 );

     found := False;
     for i := 0 to info_.queue_family_count-1 do
     begin
          if ( info_.queue_props[i].queueFlags and Ord( VK_QUEUE_GRAPHICS_BIT ) ) <> 0 then
          begin
               info_.graphics_queue_family_index := i;
               found := True;
               Break;
          end;
     end;
     Assert( found );
end;

function init_device( var info_:T_sample_info ) :VkResult;
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
     queue_info.queueFamilyIndex := info_.graphics_queue_family_index;

     device_info                              := Default( VkDeviceCreateInfo );
     device_info.sType                        := VK_STRUCTURE_TYPE_DEVICE_CREATE_INFO;
     device_info.pNext                        := nil;
     device_info.queueCreateInfoCount         := 1;
     device_info.pQueueCreateInfos            := @queue_info;
     device_info.enabledExtensionCount        := Length( info_.device_extension_names );
     if device_info.enabledExtensionCount > 0
     then device_info.ppEnabledExtensionNames := @info_.device_extension_names[0]
     else device_info.ppEnabledExtensionNames := nil;
     device_info.pEnabledFeatures             := nil;

     Result := vkCreateDevice( info_.gpus[0], @device_info, nil, @info_.device );
     Assert( Result = VK_SUCCESS );
end;

procedure destroy_device( var info_:T_sample_info );
begin
     vkDeviceWaitIdle( info_.device );
     vkDestroyDevice( info_.device, nil );
end;

//////////////////////////////////////////////////////////////////////////////// 05-init_swapchain

procedure init_instance_extension_names( var info_:T_sample_info );
begin
     info_.instance_extension_names := info_.instance_extension_names + [ VK_KHR_SURFACE_EXTENSION_NAME         ];
     info_.instance_extension_names := info_.instance_extension_names + [ VK_KHR_WIN32_SURFACE_EXTENSION_NAME   ];
end;

procedure init_device_extension_names( var info_:T_sample_info );
begin
     info_.device_extension_names := info_.device_extension_names + [ VK_KHR_SWAPCHAIN_EXTENSION_NAME ];
end;

procedure init_window_size( var info_:T_sample_info; default_width_,default_height_:UInt32 );
begin
     info_.width  := default_width_;
     info_.height := default_height_;
end;

procedure init_connection( var info_:T_sample_info );
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

procedure init_window( var info_:T_sample_info );
var
   win_class :WNDCLASSEX;
   wr        :TRect;
begin
     Assert( info_.width  > 0 );
     Assert( info_.height > 0 );

     info_.connection := GetModuleHandle( nil );
     info_.name       := 'Sample';

     // Initialize the window class structure:
     win_class.cbSize        := SizeOf( WNDCLASSEX );
     win_class.style         := CS_HREDRAW or CS_VREDRAW;
     win_class.lpfnWndProc   := @WndProc;
     win_class.cbClsExtra    := 0;
     win_class.cbWndExtra    := 0;
     win_class.hInstance     := info_.connection;  // hInstance
     win_class.hIcon         := LoadIcon( 0, IDI_APPLICATION );
     win_class.hCursor       := LoadCursor( 0, IDC_ARROW );
     win_class.hbrBackground := HBRUSH( GetStockObject( WHITE_BRUSH ) );
     win_class.lpszMenuName  := nil;
     win_class.lpszClassName := LPCWSTR( WideString( info_.name ) );
     win_class.hIconSm       := LoadIcon( 0, IDI_WINLOGO );
     // Register window class:
     if RegisterClassEx( win_class ) = 0 then
     begin
          // It didn't work, so try to give a useful error:
          Log.d( 'Unexpected error trying to start the application!' );
          RunError( 1 );
     end;
     // Create window with the registered class:
     wr := TRect.Create( 0, 0, info_.width, info_.height );
     AdjustWindowRect( wr, WS_OVERLAPPEDWINDOW, False );
     info_.window := CreateWindowEx( 0,
                                    LPCWSTR( WideString( info_.name ) ),              // class name
                                    LPCWSTR( WideString( info_.name ) ),              // app name
                                    WS_OVERLAPPEDWINDOW or WS_VISIBLE or WS_SYSMENU,  // window style
                                    100, 100,                                         // x/y coords
                                    wr.right - wr.left,                               // width
                                    wr.bottom - wr.top,                               // height
                                    0,                                                // handle to parent
                                    0,                                                // handle to menu
                                    info_.connection,                                 // hInstance
                                    nil );                                            // no extra parameters
     if info_.window = 0 then
     begin
          // It didn't work, so try to give a useful error:
          Log.d( 'Cannot create a window in which to draw!' );
          RunError( 1 );
     end;
     SetWindowLongPtr( info_.window, GWLP_USERDATA, LONG_PTR( @info_ ) );
end;

procedure destroy_window( var info_:T_sample_info );
begin
     vkDestroySurfaceKHR( info_.inst, info_.surface, nil );
     DestroyWindow( info_.window );
end;

//////////////////////////////////////////////////////////////////////////////// 06-init_depth_buffer

(* Use this surface format if it's available.  This ensures that generated
* images are similar on different devices and with different drivers.
*)
const PREFERRED_SURFACE_FORMAT = VK_FORMAT_B8G8R8A8_UNORM;

procedure init_swapchain_extension( var info_:T_sample_info );
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
     createInfo.hinstance := info_.connection;
     createInfo.hwnd      := info_.window;
     res := vkCreateWin32SurfaceKHR( info_.inst, @createInfo, nil, @info_.surface );
     Assert( res = VK_SUCCESS );

     // Iterate over each queue to learn whether it supports presenting:
     SetLength( pSupportsPresent, info_.queue_family_count );
     for i := 0 to info_.queue_family_count-1
     do vkGetPhysicalDeviceSurfaceSupportKHR( info_.gpus[0], i, info_.surface, @pSupportsPresent[i] );

     // Search for a graphics and a present queue in the array of queue
     // families, try to find one that supports both
     info_.graphics_queue_family_index := UINT32_MAX;
     info_.present_queue_family_index  := UINT32_MAX;
     for i := 0 to info_.queue_family_count-1 do
     begin
          if ( info_.queue_props[i].queueFlags and Ord( VK_QUEUE_GRAPHICS_BIT ) ) <> 0 then
          begin
               if info_.graphics_queue_family_index = UINT32_MAX then info_.graphics_queue_family_index := i;

               if pSupportsPresent[i] = VK_TRUE then
               begin
                    info_.graphics_queue_family_index := i;
                    info_.present_queue_family_index  := i;
                    Break;
               end;
          end;
     end;

     if info_.present_queue_family_index = UINT32_MAX then
     begin
          // If didn't find a queue that supports both graphics and present, then
          // find a separate present queue.
          for i := 0 to info_.queue_family_count-1 do
          begin
               if pSupportsPresent[i] = VK_TRUE then
               begin
                    info_.present_queue_family_index := i;
                    Break;
               end;
          end;
     end;
     pSupportsPresent := nil;

     // Generate error if could not find queues that support graphics
     // and present
     if ( info_.graphics_queue_family_index = UINT32_MAX ) or ( info_.present_queue_family_index = UINT32_MAX ) then
     begin
          Log.d( 'Could not find a queues for both graphics and present' );
          RunError( 256-1 );
     end;

     // Get the list of VkFormats that are supported:
     res := vkGetPhysicalDeviceSurfaceFormatsKHR( info_.gpus[0], info_.surface, @formatCount, nil );
     Assert( res = VK_SUCCESS );
     SetLength( surfFormats, formatCount );
     res := vkGetPhysicalDeviceSurfaceFormatsKHR( info_.gpus[0], info_.surface, @formatCount, @surfFormats[0] );
     Assert( res = VK_SUCCESS );

     // If the device supports our preferred surface format, use it.
     // Otherwise, use whatever the device's first reported surface
     // format is.
     Assert( formatCount >= 1 );
     info_.format := surfFormats[0].format;
     for i := 0 to formatCount-1 do
     begin
          if surfFormats[i].format = PREFERRED_SURFACE_FORMAT then
          begin
               info_.format := PREFERRED_SURFACE_FORMAT;
               break;
          end;
     end;

     surfFormats := nil;
end;

//////////////////////////////////////////////////////////////////////////////// 07-init_uniform_buffer

//////////////////////////////////////////////////////////////////////////////// 08-init_pipeline_layout

//////////////////////////////////////////////////////////////////////////////// 09-init_descriptor_set

procedure init_uniform_buffer( var info_:T_sample_info );
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
     if info_.width > info_.height then
     begin
          fov := fov * info_.height / info_.width;
     end;
     info_.Projection := TSingleM4.ProjPersH( fov, info_.width / info_.height, 0.1, 100 );
     info_.View := TSingleM4.LookAt( TSingle3D.Create( -5, +3, -10 ),    // Camera is at (-5,3,-10), in World Space
                                     TSingle3D.Create(  0,  0,   0 ),    // and looks at the origin
                                     TSingle3D.Create(  0, -1,   0 ) );  // Head is up (set to 0,-1,0 to look upside-down)

     info_.Model := TSingleM4.Identity;
     // Vulkan clip space has inverted Y and half Z.
     info_.Clip := TSingleM4.Create( +1.0,  0.0,  0.0,  0.0,
                                      0.0, -1.0,  0.0,  0.0,
                                      0.0,  0.0, +0.5, +0.5,
                                      0.0,  0.0,  0.0, +1.0 );

     info_.MVP := info_.Clip * info_.Projection * info_.View * info_.Model;

     (* VULKAN_KEY_START *)
     buf_info                       := Default( VkBufferCreateInfo );
     buf_info.sType                 := VK_STRUCTURE_TYPE_BUFFER_CREATE_INFO;
     buf_info.pNext                 := nil;
     buf_info.usage                 := Ord( VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT );
     buf_info.size                  := SizeOf( info_.MVP );
     buf_info.queueFamilyIndexCount := 0;
     buf_info.pQueueFamilyIndices   := nil;
     buf_info.sharingMode           := VK_SHARING_MODE_EXCLUSIVE;
     buf_info.flags                 := 0;
     res := vkCreateBuffer( info_.device, @buf_info, nil, @info_.uniform_data.buf );
     Assert( res = VK_SUCCESS );

     vkGetBufferMemoryRequirements( info_.device, info_.uniform_data.buf, @mem_reqs );

     alloc_info                 := Default( VkMemoryAllocateInfo );
     alloc_info.sType           := VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO;
     alloc_info.pNext           := nil;
     alloc_info.memoryTypeIndex := 0;

     alloc_info.allocationSize := mem_reqs.size;
     pass := memory_type_from_properties( info_, mem_reqs.memoryTypeBits,
                                          Ord( VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT ) or Ord( VK_MEMORY_PROPERTY_HOST_COHERENT_BIT ),
                                          alloc_info.memoryTypeIndex );
     Assert( pass, 'No mappable, coherent memory' );

     res := vkAllocateMemory( info_.device, @alloc_info, nil, @info_.uniform_data.mem );
     Assert( res = VK_SUCCESS );

     res := vkMapMemory( info_.device, info_.uniform_data.mem, 0, mem_reqs.size, 0, @pData );
     Assert( res = VK_SUCCESS );

     Move( info_.MVP, pData^, SizeOf( info_.MVP ) );

     vkUnmapMemory( info_.device, info_.uniform_data.mem );

     res := vkBindBufferMemory( info_.device, info_.uniform_data.buf, info_.uniform_data.mem, 0 );
     Assert( res = VK_SUCCESS );

     info_.uniform_data.buffer_info.buffer := info_.uniform_data.buf;
     info_.uniform_data.buffer_info.offset := 0;
     info_.uniform_data.buffer_info.range  := SizeOf( info_.MVP );
end;

procedure init_descriptor_and_pipeline_layouts( var info_:T_sample_info; use_texture_:T_bool; descSetLayoutCreateFlags_:VkDescriptorSetLayoutCreateFlags = 0 );
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

     SetLength( info_.desc_layout, NUM_DESCRIPTOR_SETS );
     res := vkCreateDescriptorSetLayout( info_.device, @descriptor_layout, nil, @info_.desc_layout[0] );
     Assert( res = VK_SUCCESS );

     (* Now use the descriptor layout to create a pipeline layout *)
     pPipelineLayoutCreateInfo                        := Default( VkPipelineLayoutCreateInfo );
     pPipelineLayoutCreateInfo.sType                  := VK_STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO;
     pPipelineLayoutCreateInfo.pNext                  := nil;
     pPipelineLayoutCreateInfo.pushConstantRangeCount := 0;
     pPipelineLayoutCreateInfo.pPushConstantRanges    := nil;
     pPipelineLayoutCreateInfo.setLayoutCount         := NUM_DESCRIPTOR_SETS;
     pPipelineLayoutCreateInfo.pSetLayouts            := @info_.desc_layout[0];

     res := vkCreatePipelineLayout( info_.device, @pPipelineLayoutCreateInfo, nil, @info_.pipeline_layout );
     Assert( res = VK_SUCCESS );
end;

procedure destroy_uniform_buffer( var info_:T_sample_info );
begin
     vkDestroyBuffer( info_.device, info_.uniform_data.buf, nil );
     vkFreeMemory( info_.device, info_.uniform_data.mem, nil );
end;

procedure destroy_descriptor_and_pipeline_layouts( var info_:T_sample_info );
var
   i :T_int;
begin
     for i := 0 to NUM_DESCRIPTOR_SETS-1 do vkDestroyDescriptorSetLayout( info_.device, info_.desc_layout[i], nil );
     vkDestroyPipelineLayout( info_.device, info_.pipeline_layout, nil );
end;

//////////////////////////////////////////////////////////////////////////////// 10-init_render_pass

procedure init_device_queue( var info_:T_sample_info );
begin
     (* DEPENDS on init_swapchain_extension() *)

     vkGetDeviceQueue( info_.device, info_.graphics_queue_family_index, 0, @info_.graphics_queue );
     if info_.graphics_queue_family_index = info_.present_queue_family_index
     then info_.present_queue := info_.graphics_queue
     else vkGetDeviceQueue( info_.device, info_.present_queue_family_index, 0, @info_.present_queue );
end;

procedure init_swap_chain( var info_:T_sample_info; usageFlags_:VkImageUsageFlags = Ord( VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT ) or Ord( VK_IMAGE_USAGE_TRANSFER_SRC_BIT ) );
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

     res := vkGetPhysicalDeviceSurfaceCapabilitiesKHR( info_.gpus[0], info_.surface, @surfCapabilities );
     Assert( res = VK_SUCCESS );

     res := vkGetPhysicalDeviceSurfacePresentModesKHR( info_.gpus[0], info_.surface, @presentModeCount, nil );
     Assert( res = VK_SUCCESS );
     SetLength( presentModes, presentModeCount );
     Assert( Length( presentModes ) > 0 );
     res := vkGetPhysicalDeviceSurfacePresentModesKHR( info_.gpus[0], info_.surface, @presentModeCount, @presentModes[0] );
     Assert( res = VK_SUCCESS );

     // width and height are either both 0xFFFFFFFF, or both not 0xFFFFFFFF.
     if surfCapabilities.currentExtent.width = $FFFFFFFF then
     begin
          // If the surface size is undefined, the size is set to
          // the size of the images requested.
          swapchainExtent.width  := info_.width;
          swapchainExtent.height := info_.height;
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
     swapchain_ci.surface               := info_.surface;
     swapchain_ci.minImageCount         := desiredNumberOfSwapChainImages;
     swapchain_ci.imageFormat           := info_.format;
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
     queueFamilyIndices[0] := info_.graphics_queue_family_index;
     queueFamilyIndices[1] := info_.present_queue_family_index;
     if info_.graphics_queue_family_index <> info_.present_queue_family_index then
     begin
          // If the graphics and present queues are from different queue families,
          // we either have to explicitly transfer ownership of images between the
          // queues, or we have to create the swapchain with imageSharingMode
          // as VK_SHARING_MODE_CONCURRENT
          swapchain_ci.imageSharingMode      := VK_SHARING_MODE_CONCURRENT;
          swapchain_ci.queueFamilyIndexCount := 2;
          swapchain_ci.pQueueFamilyIndices   := @queueFamilyIndices[0];
     end;

     res := vkCreateSwapchainKHR( info_.device, @swapchain_ci, nil, @info_.swap_chain );
     Assert( res = VK_SUCCESS );

     res := vkGetSwapchainImagesKHR( info_.device, info_.swap_chain, @info_.swapchainImageCount, nil );
     Assert( res = VK_SUCCESS );

     SetLength( swapchainImages, info_.swapchainImageCount );
     Assert( Length( swapchainImages ) > 0 );
     res := vkGetSwapchainImagesKHR( info_.device, info_.swap_chain, @info_.swapchainImageCount, @swapchainImages[0] );
     Assert( res = VK_SUCCESS );

     for i := 0 to info_.swapchainImageCount-1 do
     begin
          color_image_view                                 := Default( VkImageViewCreateInfo );
          color_image_view.sType                           := VK_STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO;
          color_image_view.pNext                           := nil;
          color_image_view.format                          := info_.format;
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

          res := vkCreateImageView( info_.device, @color_image_view, nil, @sc_buffer.view );
          info_.buffers := info_.buffers + [ sc_buffer ];
          Assert( res = VK_SUCCESS );
     end;
     swapchainImages := nil;
     info_.current_buffer := 0;

     if nil <> presentModes then presentModes := nil;
end;

procedure init_depth_buffer( var info_:T_sample_info );
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
     if info_.depth.format = VK_FORMAT_UNDEFINED then info_.depth.format := VK_FORMAT_D16_UNORM;

     depth_format := info_.depth.format;
     vkGetPhysicalDeviceFormatProperties( info_.gpus[0], depth_format, @props );
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
     image_info.extent.width          := info_.width;
     image_info.extent.height         := info_.height;
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
     res := vkCreateImage( info_.device, @image_info, nil, @info_.depth.image );
     Assert( res = VK_SUCCESS );

     vkGetImageMemoryRequirements( info_.device, info_.depth.image, @mem_reqs );

     mem_alloc.allocationSize := mem_reqs.size;
     (* Use the memory properties to determine the type of memory required *)
     pass := memory_type_from_properties( info_, mem_reqs.memoryTypeBits, Ord( VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT ), mem_alloc.memoryTypeIndex );
     Assert( pass );

     (* Allocate memory *)
     res := vkAllocateMemory( info_.device, @mem_alloc, nil, @info_.depth.mem );
     Assert( res = VK_SUCCESS );

     (* Bind memory *)
     res := vkBindImageMemory( info_.device, info_.depth.image, info_.depth.mem, 0 );
     Assert( res = VK_SUCCESS );

     (* Create image view *)
     view_info.image := info_.depth.image;
     res := vkCreateImageView( info_.device, @view_info, nil, @info_.depth.view );
     Assert( res = VK_SUCCESS );
end;

procedure destroy_depth_buffer( var info_:T_sample_info );
begin
     vkDestroyImageView( info_.device, info_.depth.view, nil );
     vkDestroyImage( info_.device, info_.depth.image, nil );
     vkFreeMemory( info_.device, info_.depth.mem, nil );
end;

procedure destroy_swap_chain( var info_:T_sample_info );
var
   I :T_uint32_t;
begin
     for i := 0 to info_.swapchainImageCount-1 do vkDestroyImageView( info_.device, info_.buffers[i].view, nil );
     vkDestroySwapchainKHR( info_.device, info_.swap_chain, nil );
end;

//////////////////////////////////////////////////////////////////////////////// 11-init_shaders

//////////////////////////////////////////////////////////////////////////////// 12-init_frame_buffers

procedure init_command_pool( var info_:T_sample_info );
var
   res           :VkResult;
   cmd_pool_info :VkCommandPoolCreateInfo;
begin
     (* DEPENDS on init_swapchain_extension() *)

     cmd_pool_info                  := Default( VkCommandPoolCreateInfo );
     cmd_pool_info.sType            := VK_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO;
     cmd_pool_info.pNext            := nil;
     cmd_pool_info.queueFamilyIndex := info_.graphics_queue_family_index;
     cmd_pool_info.flags            := Ord( VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT );

     res := vkCreateCommandPool( info_.device, @cmd_pool_info, nil, @info_.cmd_pool );
     Assert( res = VK_SUCCESS );
end;

procedure init_command_buffer( var info_:T_sample_info );
var
   res :VkResult;
   cmd :VkCommandBufferAllocateInfo;
begin
     (* DEPENDS on init_swapchain_extension() and init_command_pool() *)

     cmd                    := Default( VkCommandBufferAllocateInfo );
     cmd.sType              := VK_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO;
     cmd.pNext              := nil;
     cmd.commandPool        := info_.cmd_pool;
     cmd.level              := VK_COMMAND_BUFFER_LEVEL_PRIMARY;
     cmd.commandBufferCount := 1;

     res := vkAllocateCommandBuffers( info_.device, @cmd, @info_.cmd );
     Assert( res = VK_SUCCESS );
end;

procedure execute_begin_command_buffer( var info_:T_sample_info );
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

     res := vkBeginCommandBuffer( info_.cmd, @cmd_buf_info );
     Assert( res = VK_SUCCESS );
end;

procedure init_renderpass( var info_:T_sample_info;
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
     attachments[0].format         := info_.format;
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
          attachments[1].format         := info_.depth.format;
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

     res := vkCreateRenderPass( info_.device, @rp_info, nil, @info_.render_pass );
     Assert( res = VK_SUCCESS );
end;

procedure execute_end_command_buffer( var info_:T_sample_info );
var
   res :VkResult;
begin
     res := vkEndCommandBuffer( info_.cmd );
     Assert( res = VK_SUCCESS );
end;

procedure execute_queue_command_buffer( var info_:T_sample_info );
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
     cmd_bufs[0] := info_.cmd;
     fenceInfo.sType := VK_STRUCTURE_TYPE_FENCE_CREATE_INFO;
     fenceInfo.pNext := nil;
     fenceInfo.flags := 0;
     vkCreateFence( info_.device, @fenceInfo, nil, @drawFence );

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

     res := vkQueueSubmit( info_.graphics_queue, 1, @submit_info[0], drawFence );
     Assert( res = VK_SUCCESS );

     repeat
           res := vkWaitForFences( info_.device, 1, @drawFence, VK_TRUE, FENCE_TIMEOUT );

     until res <> VK_TIMEOUT;
     Assert( res = VK_SUCCESS );

     vkDestroyFence( info_.device, drawFence, nil );
end;

procedure destroy_renderpass( var info_:T_sample_info );
begin
     vkDestroyRenderPass( info_.device, info_.render_pass, nil );
end;

procedure destroy_command_buffer( var info_:T_sample_info );
var
   cmd_bufs :array [ 0..1-1 ] of VkCommandBuffer;
begin
     cmd_bufs[0] := info_.cmd;
     vkFreeCommandBuffers( info_.device, info_.cmd_pool, 1, @cmd_bufs[0] );
end;

procedure destroy_command_pool( var info_:T_sample_info );
begin
     vkDestroyCommandPool( info_.device, info_.cmd_pool, nil );
end;

//////////////////////////////////////////////////////////////////////////////// 13-init_vertex_buffer

procedure init_framebuffers( var info_:T_sample_info; include_depth:T_bool );
var
   res         :VkResult;
   attachments :array [ 0..2-1 ] of VkImageView;
   fb_info     :VkFramebufferCreateInfo;
   i           :T_uint32_t;
begin
     (* DEPENDS on init_depth_buffer(), init_renderpass() and
      * init_swapchain_extension() *)

     attachments[1] := info_.depth.view;

     fb_info                      := Default( VkFramebufferCreateInfo );
     fb_info.sType                := VK_STRUCTURE_TYPE_FRAMEBUFFER_CREATE_INFO;
     fb_info.pNext                := nil;
     fb_info.renderPass           := info_.render_pass;
     if include_depth
     then fb_info.attachmentCount := 2
     else fb_info.attachmentCount := 1;
     fb_info.pAttachments         := @attachments[0];
     fb_info.width                := info_.width;
     fb_info.height               := info_.height;
     fb_info.layers               := 1;

     SetLength( info_.framebuffers, info_.swapchainImageCount );

     for i := 0 to info_.swapchainImageCount-1 do
     begin
          attachments[0] := info_.buffers[i].view;
          res := vkCreateFramebuffer( info_.device, @fb_info, nil, @info_.framebuffers[i] );
          Assert( res = VK_SUCCESS );
     end;
end;

procedure destroy_framebuffers( var info_:T_sample_info );
var
   i :T_uint32_t;
begin
     for i := 0 to info_.swapchainImageCount-1 do vkDestroyFramebuffer( info_.device, info_.framebuffers[i], nil );
     info_.framebuffers := nil;
end;

//////////////////////////////////////////////////////////////////////////////// 14-init_pipeline

procedure init_vertex_buffer( var info_:T_sample_info; const vertexData_:P_void; dataSize_:T_uint32_t; dataStride_:T_uint32_t; use_texture_:T_bool );
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
     res := vkCreateBuffer( info_.device, @buf_info, nil, @info_.vertex_buffer.buf );
     Assert( res = VK_SUCCESS );

     vkGetBufferMemoryRequirements( info_.device, info_.vertex_buffer.buf, @mem_reqs );

     alloc_info                 := Default( VkMemoryAllocateInfo );
     alloc_info.sType           := VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO;
     alloc_info.pNext           := nil;
     alloc_info.memoryTypeIndex := 0;

     alloc_info.allocationSize := mem_reqs.size;
     pass := memory_type_from_properties( info_, mem_reqs.memoryTypeBits,
                                          Ord( VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT ) or Ord( VK_MEMORY_PROPERTY_HOST_COHERENT_BIT ),
                                          alloc_info.memoryTypeIndex );
     Assert( pass, 'No mappable, coherent memory' );

     res := vkAllocateMemory( info_.device, @alloc_info, nil, @info_.vertex_buffer.mem );
     Assert( res = VK_SUCCESS );
     info_.vertex_buffer.buffer_info.range  := mem_reqs.size;
     info_.vertex_buffer.buffer_info.offset := 0;

     res := vkMapMemory( info_.device, info_.vertex_buffer.mem, 0, mem_reqs.size, 0, @pData );
     Assert( res = VK_SUCCESS );

     Move( vertexData_^, pData^, dataSize_ );

     vkUnmapMemory( info_.device, info_.vertex_buffer.mem );

     res := vkBindBufferMemory( info_.device, info_.vertex_buffer.buf, info_.vertex_buffer.mem, 0 );
     Assert( res = VK_SUCCESS );

     info_.vi_binding.binding   := 0;
     info_.vi_binding.inputRate := VK_VERTEX_INPUT_RATE_VERTEX;
     info_.vi_binding.stride    := dataStride_;

     info_.vi_attribs[0].binding     := 0;
     info_.vi_attribs[0].location    := 0;
     info_.vi_attribs[0].format      := VK_FORMAT_R32G32B32A32_SFLOAT;
     info_.vi_attribs[0].offset      := 0;
     info_.vi_attribs[1].binding     := 0;
     info_.vi_attribs[1].location    := 1;
     if use_texture_
     then info_.vi_attribs[1].format := VK_FORMAT_R32G32_SFLOAT
     else info_.vi_attribs[1].format := VK_FORMAT_R32G32B32A32_SFLOAT;
     info_.vi_attribs[1].offset      := 16;
end;

procedure init_descriptor_pool( var info_:T_sample_info; use_texture_:T_bool );
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

     res := vkCreateDescriptorPool( info_.device, @descriptor_pool, nil, @info_.desc_pool );
     Assert( res = VK_SUCCESS );
end;

procedure init_descriptor_set( var info_:T_sample_info; use_texture_:T_bool );
var
   res        :VkResult;
   alloc_info :array [ 0..1-1 ] of VkDescriptorSetAllocateInfo;
   writes     :array [ 0..2-1 ] of VkWriteDescriptorSet;
begin
     (* DEPENDS on init_descriptor_pool() *)

     alloc_info[0].sType              := VK_STRUCTURE_TYPE_DESCRIPTOR_SET_ALLOCATE_INFO;
     alloc_info[0].pNext              := nil;
     alloc_info[0].descriptorPool     := info_.desc_pool;
     alloc_info[0].descriptorSetCount := NUM_DESCRIPTOR_SETS;
     alloc_info[0].pSetLayouts        := @info_.desc_layout[0];

     SetLength( info_.desc_set, NUM_DESCRIPTOR_SETS );
     res := vkAllocateDescriptorSets( info_.device, @alloc_info[0], @info_.desc_set[0] );
     Assert( res = VK_SUCCESS );

     writes[0]                 := Default( VkWriteDescriptorSet );
     writes[0].sType           := VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET;
     writes[0].pNext           := nil;
     writes[0].dstSet          := info_.desc_set[0];
     writes[0].descriptorCount := 1;
     writes[0].descriptorType  := VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER;
     writes[0].pBufferInfo     := @info_.uniform_data.buffer_info;
     writes[0].dstArrayElement := 0;
     writes[0].dstBinding      := 0;

     if use_texture_ then
     begin
          writes[1]                 := Default( VkWriteDescriptorSet );
          writes[1].sType           := VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET;
          writes[1].dstSet          := info_.desc_set[0];
          writes[1].dstBinding      := 1;
          writes[1].descriptorCount := 1;
          writes[1].descriptorType  := VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER;
          writes[1].pImageInfo      := @info_.texture_data.image_info;
          writes[1].dstArrayElement := 0;
     end;

     if use_texture_
     then vkUpdateDescriptorSets( info_.device, 2, @writes[0], 0, nil )
     else vkUpdateDescriptorSets( info_.device, 1, @writes[0], 0, nil );
end;

procedure init_shaders( var info_:T_sample_info; const vertShaderCI_:P_VkShaderModuleCreateInfo; const fragShaderCI_:P_VkShaderModuleCreateInfo );
var
   res :VkResult;
begin
     if vertShaderCI_ <> nil then
     begin
          info_.shaderStages[0].sType               := VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO;
          info_.shaderStages[0].pNext               := nil;
          info_.shaderStages[0].pSpecializationInfo := nil;
          info_.shaderStages[0].flags               := 0;
          info_.shaderStages[0].stage               := VK_SHADER_STAGE_VERTEX_BIT;
          info_.shaderStages[0].pName               := 'main';
          res := vkCreateShaderModule( info_.device, vertShaderCI_, nil, @info_.shaderStages[0].module );
          Assert( res = VK_SUCCESS );
     end;

     if fragShaderCI_ <> nil then
     begin
          info_.shaderStages[1].sType               := VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO;
          info_.shaderStages[1].pNext               := nil;
          info_.shaderStages[1].pSpecializationInfo := nil;
          info_.shaderStages[1].flags               := 0;
          info_.shaderStages[1].stage               := VK_SHADER_STAGE_FRAGMENT_BIT;
          info_.shaderStages[1].pName               := 'main';
          res := vkCreateShaderModule( info_.device, fragShaderCI_, nil, @info_.shaderStages[1].module );
          Assert( res = VK_SUCCESS );
     end;
end;

procedure destroy_descriptor_pool( var info_:T_sample_info );
begin
     vkDestroyDescriptorPool( info_.device, info_.desc_pool, nil );
end;

procedure destroy_vertex_buffer( var info_:T_sample_info );
begin
     vkDestroyBuffer( info_.device, info_.vertex_buffer.buf, nil );
     vkFreeMemory( info_.device, info_.vertex_buffer.mem, nil );
end;

procedure destroy_shaders( var info_:T_sample_info );
begin
     vkDestroyShaderModule( info_.device, info_.shaderStages[0].module, nil );
     vkDestroyShaderModule( info_.device, info_.shaderStages[1].module, nil );
end;

//////////////////////////////////////////////////////////////////////////////// 15-draw_cube

procedure init_viewports( var info_:T_sample_info );
begin
     info_.viewport.height   := info_.height;
     info_.viewport.width    := info_.width;
     info_.viewport.minDepth := 0.0;
     info_.viewport.maxDepth := 1.0;
     info_.viewport.x        := 0;
     info_.viewport.y        := 0;
     vkCmdSetViewport( info_.cmd, 0, NUM_VIEWPORTS, @info_.viewport );
end;

procedure init_scissors( var info_:T_sample_info );
begin
     info_.scissor.extent.width  := info_.width;
     info_.scissor.extent.height := info_.height;
     info_.scissor.offset.x      := 0;
     info_.scissor.offset.y      := 0;
     vkCmdSetScissor( info_.cmd, 0, NUM_SCISSORS, @info_.scissor );
end;

procedure init_pipeline_cache( var info:T_sample_info );
var
   res :VkResult;
   pipelineCache :VkPipelineCacheCreateInfo;
begin
     pipelineCache.sType           := VK_STRUCTURE_TYPE_PIPELINE_CACHE_CREATE_INFO;
     pipelineCache.pNext           := nil;
     pipelineCache.initialDataSize := 0;
     pipelineCache.pInitialData    := nil;
     pipelineCache.flags           := 0;
     res := vkCreatePipelineCache( info.device, @pipelineCache, nil, @info.pipelineCache );
     Assert( res = VK_SUCCESS );
end;

procedure init_pipeline( var info:T_sample_info; include_depth:VkBool32; include_vi:VkBool32 = 1 );
type
    T_dynamicStateEnables = array [ 0..2-1 ] of VkDynamicState;
var
   res :VkResult;
   dynamicStateEnables :T_dynamicStateEnables;  // Viewport + Scissor
   dynamicState        :VkPipelineDynamicStateCreateInfo;
   vi                  :VkPipelineVertexInputStateCreateInfo;
   ia                  :VkPipelineInputAssemblyStateCreateInfo;
   rs                  :VkPipelineRasterizationStateCreateInfo;
   cb                  :VkPipelineColorBlendStateCreateInfo;
   att_state           :array [ 0..1-1 ] of VkPipelineColorBlendAttachmentState;
   vp                  :VkPipelineViewportStateCreateInfo;
   ds                  :VkPipelineDepthStencilStateCreateInfo;
   ms                  :VkPipelineMultisampleStateCreateInfo;
   pipeline            :VkGraphicsPipelineCreateInfo;
begin
     dynamicStateEnables            := Default( T_dynamicStateEnables );
     dynamicState                   := Default( VkPipelineDynamicStateCreateInfo );
     dynamicState.sType             := VK_STRUCTURE_TYPE_PIPELINE_DYNAMIC_STATE_CREATE_INFO;
     dynamicState.pNext             := nil;
     dynamicState.pDynamicStates    := @dynamicStateEnables[0];
     dynamicState.dynamicStateCount := 0;

     vi       := Default( VkPipelineVertexInputStateCreateInfo );
     vi.sType := VK_STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_STATE_CREATE_INFO;
     if include_vi <> 0 then
     begin
          vi.pNext                           := nil;
          vi.flags                           := 0;
          vi.vertexBindingDescriptionCount   := 1;
          vi.pVertexBindingDescriptions      := @info.vi_binding;
          vi.vertexAttributeDescriptionCount := 2;
          vi.pVertexAttributeDescriptions    := @info.vi_attribs[0];
     end;
     ia.sType                  := VK_STRUCTURE_TYPE_PIPELINE_INPUT_ASSEMBLY_STATE_CREATE_INFO;
     ia.pNext                  := nil;
     ia.flags                  := 0;
     ia.primitiveRestartEnable := VK_FALSE;
     ia.topology               := VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST;

     rs.sType                   := VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_CREATE_INFO;
     rs.pNext                   := nil;
     rs.flags                   := 0;
     rs.polygonMode             := VK_POLYGON_MODE_FILL;
     rs.cullMode                := Ord( VK_CULL_MODE_BACK_BIT );
     rs.frontFace               := VK_FRONT_FACE_CLOCKWISE;
     rs.depthClampEnable        := VK_FALSE;
     rs.rasterizerDiscardEnable := VK_FALSE;
     rs.depthBiasEnable         := VK_FALSE;
     rs.depthBiasConstantFactor := 0;
     rs.depthBiasClamp          := 0;
     rs.depthBiasSlopeFactor    := 0;
     rs.lineWidth               := 1.0;

     cb.sType := VK_STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_STATE_CREATE_INFO;
     cb.flags := 0;
     cb.pNext := nil;

     att_state[0].colorWriteMask      := $f;
     att_state[0].blendEnable         := VK_FALSE;
     att_state[0].alphaBlendOp        := VK_BLEND_OP_ADD;
     att_state[0].colorBlendOp        := VK_BLEND_OP_ADD;
     att_state[0].srcColorBlendFactor := VK_BLEND_FACTOR_ZERO;
     att_state[0].dstColorBlendFactor := VK_BLEND_FACTOR_ZERO;
     att_state[0].srcAlphaBlendFactor := VK_BLEND_FACTOR_ZERO;
     att_state[0].dstAlphaBlendFactor := VK_BLEND_FACTOR_ZERO;

     cb.attachmentCount   := 1;
     cb.pAttachments      := @att_state[0];
     cb.logicOpEnable     := VK_FALSE;
     cb.logicOp           := VK_LOGIC_OP_NO_OP;
     cb.blendConstants[0] := 1.0;
     cb.blendConstants[1] := 1.0;
     cb.blendConstants[2] := 1.0;
     cb.blendConstants[3] := 1.0;

     vp               := Default( VkPipelineViewportStateCreateInfo );
     vp.sType         := VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_STATE_CREATE_INFO;
     vp.pNext         := nil;
     vp.flags         := 0;
     vp.viewportCount := NUM_VIEWPORTS;
     dynamicStateEnables[dynamicState.dynamicStateCount] := VK_DYNAMIC_STATE_VIEWPORT;  Inc( dynamicState.dynamicStateCount );
     vp.scissorCount  := NUM_SCISSORS;
     dynamicStateEnables[dynamicState.dynamicStateCount] := VK_DYNAMIC_STATE_SCISSOR ;  Inc( dynamicState.dynamicStateCount );
     vp.pScissors     := nil;
     vp.pViewports    := nil;

     ds.sType                 := VK_STRUCTURE_TYPE_PIPELINE_DEPTH_STENCIL_STATE_CREATE_INFO;
     ds.pNext                 := nil;
     ds.flags                 := 0;
     ds.depthTestEnable       := include_depth;
     ds.depthWriteEnable      := include_depth;
     ds.depthCompareOp        := VK_COMPARE_OP_LESS_OR_EQUAL;
     ds.depthBoundsTestEnable := VK_FALSE;
     ds.stencilTestEnable     := VK_FALSE;
     ds.back.failOp           := VK_STENCIL_OP_KEEP;
     ds.back.passOp           := VK_STENCIL_OP_KEEP;
     ds.back.compareOp        := VK_COMPARE_OP_ALWAYS;
     ds.back.compareMask      := 0;
     ds.back.reference        := 0;
     ds.back.depthFailOp      := VK_STENCIL_OP_KEEP;
     ds.back.writeMask        := 0;
     ds.minDepthBounds        := 0;
     ds.maxDepthBounds        := 0;
     ds.stencilTestEnable     := VK_FALSE;
     ds.front                 := ds.back;

     ms.sType                 := VK_STRUCTURE_TYPE_PIPELINE_MULTISAMPLE_STATE_CREATE_INFO;
     ms.pNext                 := nil;
     ms.flags                 := 0;
     ms.pSampleMask           := nil;
     ms.rasterizationSamples  := NUM_SAMPLES;
     ms.sampleShadingEnable   := VK_FALSE;
     ms.alphaToCoverageEnable := VK_FALSE;
     ms.alphaToOneEnable      := VK_FALSE;
     ms.minSampleShading      := 0.0;

     pipeline.sType               := VK_STRUCTURE_TYPE_GRAPHICS_PIPELINE_CREATE_INFO;
     pipeline.pNext               := nil;
     pipeline.layout              := info.pipeline_layout;
     pipeline.basePipelineHandle  := VK_NULL_HANDLE;
     pipeline.basePipelineIndex   := 0;
     pipeline.flags               := 0;
     pipeline.pVertexInputState   := @vi;
     pipeline.pInputAssemblyState := @ia;
     pipeline.pRasterizationState := @rs;
     pipeline.pColorBlendState    := @cb;
     pipeline.pTessellationState  := nil;
     pipeline.pMultisampleState   := @ms;
     pipeline.pDynamicState       := @dynamicState;
     pipeline.pViewportState      := @vp;
     pipeline.pDepthStencilState  := @ds;
     pipeline.pStages             := @info.shaderStages[0];
     pipeline.stageCount          := 2;
     pipeline.renderPass          := info.render_pass;
     pipeline.subpass             := 0;

     res := vkCreateGraphicsPipelines(info.device, info.pipelineCache, 1, @pipeline, nil, @info.pipeline);
     Assert( res = VK_SUCCESS );
end;

procedure destroy_pipeline( var info_:T_sample_info );
begin
     vkDestroyPipeline( info_.device, info_.pipeline, nil );
end;

procedure destroy_pipeline_cache( var info_:T_sample_info );
begin
     vkDestroyPipelineCache( info_.device, info_.pipelineCache, nil );
end;

//////////////////////////////////////////////////////////////////////////////// draw_textured_cube

procedure init_buffer( var info_:T_sample_info; var texObj_:T_texture_object );
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
     res := vkCreateBuffer( info_.device, @buffer_create_info, nil, @texObj_.buffer );
     Assert( res = VK_SUCCESS );

     mem_alloc                 := Default( VkMemoryAllocateInfo );
     mem_alloc.sType           := VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO;
     mem_alloc.pNext           := nil;
     mem_alloc.allocationSize  := 0;
     mem_alloc.memoryTypeIndex := 0;

     vkGetBufferMemoryRequirements( info_.device, texObj_.buffer, @mem_reqs );
     mem_alloc.allocationSize := mem_reqs.size;
     texObj_.buffer_size := mem_reqs.size;

     requirements := Ord( VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT ) or Ord( VK_MEMORY_PROPERTY_HOST_COHERENT_BIT );
     pass := memory_type_from_properties( info_, mem_reqs.memoryTypeBits, requirements, mem_alloc.memoryTypeIndex );
     Assert( pass, '"No mappable, coherent memory' );

     (* allocate memory *)
     res := vkAllocateMemory(info_.device, @mem_alloc, nil, @( texObj_.buffer_memory) );
     Assert( res = VK_SUCCESS );

     (* bind memory *)
     res := vkBindBufferMemory( info_.device, texObj_.buffer, texObj_.buffer_memory, 0 );
     Assert( res = VK_SUCCESS );
end;

procedure init_image( var info_:T_sample_info; var texObj_:T_texture_object; const textureName_:String; extraUsages_:VkImageUsageFlags; extraFeatures_:VkFormatFeatureFlags );
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

     vkGetPhysicalDeviceFormatProperties( info_.gpus[0], VK_FORMAT_R8G8B8A8_UNORM, @formatProps );

     (* See if we can use a linear tiled image for a texture, if not, we will
      * need a staging buffer for the texture data *)
     allFeatures := Ord( VK_FORMAT_FEATURE_SAMPLED_IMAGE_BIT ) or extraFeatures_;
     texObj_.needs_staging := ( ( formatProps.linearTilingFeatures and allFeatures ) <> allFeatures );

     if texObj_.needs_staging then
     begin
          Assert( ( formatProps.optimalTilingFeatures and allFeatures ) = allFeatures );
          init_buffer( info_, texObj_ );
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

     res := vkCreateImage( info_.device, @image_create_info, nil, @texObj_.image );
     Assert( res = VK_SUCCESS );

     vkGetImageMemoryRequirements( info_.device, texObj_.image, @mem_reqs );

     mem_alloc.allocationSize := mem_reqs.size;

     if texObj_.needs_staging
     then requirements := 0
     else requirements := Ord( VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT ) or Ord( VK_MEMORY_PROPERTY_HOST_COHERENT_BIT );
     pass := memory_type_from_properties( info_, mem_reqs.memoryTypeBits, requirements, mem_alloc.memoryTypeIndex );
     Assert( pass );

     (* allocate memory *)
     res := vkAllocateMemory(info_.device, @mem_alloc, nil, @( texObj_.image_memory) );
     Assert( res = VK_SUCCESS );

     (* bind memory *)
     res := vkBindImageMemory( info_.device, texObj_.image, texObj_.image_memory, 0 );
     Assert( res = VK_SUCCESS );

     res := vkEndCommandBuffer( info_.cmd );
     Assert( res = VK_SUCCESS );
     cmd_bufs[0] := info_.cmd;
     fenceInfo.sType := VK_STRUCTURE_TYPE_FENCE_CREATE_INFO;
     fenceInfo.pNext := nil;
     fenceInfo.flags := 0;
     vkCreateFence( info_.device, @fenceInfo, nil, @cmdFence );

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
     res := vkQueueSubmit( info_.graphics_queue, 1, @submit_info[0], cmdFence );
     Assert( res = VK_SUCCESS );

     subres            := Default( VkImageSubresource );
     subres.aspectMask := Ord( VK_IMAGE_ASPECT_COLOR_BIT );
     subres.mipLevel   := 0;
     subres.arrayLayer := 0;

     layout := Default( VkSubresourceLayout );
     if not texObj_.needs_staging then
     begin
          (* Get the subresource layout so we know what the row pitch is *)
          vkGetImageSubresourceLayout( info_.device, texObj_.image, @subres, @layout );
     end;

     (* Make sure command buffer is finished before mapping *)
     repeat
           res := vkWaitForFences( info_.device, 1, @cmdFence, VK_TRUE, FENCE_TIMEOUT );

     until res <> VK_TIMEOUT;
     Assert( res = VK_SUCCESS );

     vkDestroyFence( info_.device, cmdFence, nil );

     if texObj_.needs_staging
     then res := vkMapMemory( info_.device, texObj_.buffer_memory, 0, texObj_.buffer_size, 0, @data )
     else res := vkMapMemory( info_.device, texObj_.image_memory , 0, mem_reqs.size     , 0, @data );
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
     then vkUnmapMemory( info_.device, texObj_.buffer_memory )
     else vkUnmapMemory( info_.device, texObj_.image_memory  );

     cmd_buf_info                  := Default( VkCommandBufferBeginInfo );
     cmd_buf_info.sType            := VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO;
     cmd_buf_info.pNext            := nil;
     cmd_buf_info.flags            := 0;
     cmd_buf_info.pInheritanceInfo := nil;

     res := vkResetCommandBuffer( info_.cmd, 0 );
     Assert( res = VK_SUCCESS );
     res := vkBeginCommandBuffer( info_.cmd, @cmd_buf_info );
     Assert( res = VK_SUCCESS );

     if not texObj_.needs_staging then
     begin
          (* If we can use the linear tiled image as a texture, just do it *)
          texObj_.imageLayout := VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL;
          set_image_layout( info_, texObj_.image, Ord( VK_IMAGE_ASPECT_COLOR_BIT ), VK_IMAGE_LAYOUT_PREINITIALIZED, texObj_.imageLayout,
                            Ord( VK_PIPELINE_STAGE_HOST_BIT ), Ord( VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT ) );
     end
     else
     begin
          (* Since we're going to blit to the texture image, set its layout to
           * DESTINATION_OPTIMAL *)
          set_image_layout( info_, texObj_.image, Ord( VK_IMAGE_ASPECT_COLOR_BIT ), VK_IMAGE_LAYOUT_UNDEFINED,
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
          vkCmdCopyBufferToImage( info_.cmd, texObj_.buffer, texObj_.image, VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL, 1, @copy_region );

          (* Set the layout for the texture image from DESTINATION_OPTIMAL to
           * SHADER_READ_ONLY *)
          texObj_.imageLayout := VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL;
          set_image_layout( info_, texObj_.image, Ord( VK_IMAGE_ASPECT_COLOR_BIT ), VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL, texObj_.imageLayout,
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
     res := vkCreateImageView( info_.device, @view_info, nil, @texObj_.view );
     Assert( res = VK_SUCCESS );
end;

procedure init_sampler( var info_:T_sample_info; var sampler_:VkSampler );
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
     res := vkCreateSampler( info_.device, @samplerCreateInfo, nil, @sampler_ );
     Assert( res = VK_SUCCESS );
end;

procedure init_texture( var info_:T_sample_info; const textureName_:String = ''; extraUsages_:VkImageUsageFlags = 0; extraFeatures_:VkFormatFeatureFlags = 0 );
var
   texObj :T_texture_object;
begin
     (* create image *)
     init_image( info_, texObj, textureName_, extraUsages_, extraFeatures_ );

     (* create sampler *)
     init_sampler( info_, texObj.sampler );

     info_.textures := info_.textures + [ texObj ];

     (* track a description of the texture *)
     info_.texture_data.image_info.imageView   := info_.textures[ High( info_.textures ) ].view;
     info_.texture_data.image_info.sampler     := info_.textures[ High( info_.textures ) ].sampler;
     info_.texture_data.image_info.imageLayout := VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL;
end;

procedure destroy_textures( var info_:T_sample_info );
var
   i :T_size_t;
begin
     for i := 0 to Length( info_.textures )-1 do
     begin
          vkDestroySampler  ( info_.device, info_.textures[i].sampler      , nil );
          vkDestroyImageView( info_.device, info_.textures[i].view         , nil );
          vkDestroyImage    ( info_.device, info_.textures[i].image        , nil );
          vkFreeMemory      ( info_.device, info_.textures[i].image_memory , nil );
          vkDestroyBuffer   ( info_.device, info_.textures[i].buffer       , nil );
          vkFreeMemory      ( info_.device, info_.textures[i].buffer_memory, nil );
     end;
end;

end. //######################################################################### ■