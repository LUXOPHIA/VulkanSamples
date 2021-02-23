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

procedure init_instance_extension_names( var info:T_sample_info );
procedure init_device_extension_names( var info:T_sample_info );
procedure init_window_size( var info:T_sample_info; default_width,default_height:UInt32 );
procedure init_connection( var info:T_sample_info );
procedure init_window( var info:T_sample_info );
procedure destroy_window( var info:T_sample_info );

implementation //############################################################### ■

uses System.Types,
     FMX.Types,
     Winapi.Windows, Winapi.Messages;

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
     {$IFDEF Android }
     // This place is the first place for samples to use Vulkan APIs.
     // Here, we are going to open Vulkan.so on the device and retrieve function pointers using
     // vulkan_wrapper helper.
     if not InitVulkan then
     begin
          LOGE(' Failied initializing Vulkan APIs!' );

          Exit( VK_ERROR_INITIALIZATION_FAILED );
     end;
     LOGI( 'Loaded Vulkan APIs.' );
     {$ENDIF}

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
    app_info.sType              := VK_STRUCTURE_TYPE_APPLICATION_INFO;
    app_info.pNext              := nil;
    app_info.pApplicationName   := app_short_name_;
    app_info.applicationVersion := 1;
    app_info.pEngineName        := app_short_name_;
    app_info.engineVersion      := 1;
    app_info.apiVersion         := VK_API_VERSION_1_0;

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
     {Result := }vkEnumeratePhysicalDevices( info_.inst, @gpu_count_, nil );
     Assert( gpu_count_ > 0 );
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
          if ( info_.queue_props[i].queueFlags and VkQueueFlags( VK_QUEUE_GRAPHICS_BIT ) ) > 0 then
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
   queue_priorities :array [ 0..0 ] of T_float;
   device_info      :VkDeviceCreateInfo;
begin
     queue_priorities[0]         := 0;
     queue_info.sType            := VK_STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO;
     queue_info.pNext            := nil;
     queue_info.queueCount       := 1;
     queue_info.pQueuePriorities := @queue_priorities[0];
     queue_info.queueFamilyIndex := info_.graphics_queue_family_index;

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

procedure init_instance_extension_names( var info:T_sample_info );
begin
     info.instance_extension_names := info.instance_extension_names + [ VK_KHR_SURFACE_EXTENSION_NAME         ];
     {$IFDEF Android }
     info.instance_extension_names := info.instance_extension_names + [ VK_KHR_ANDROID_SURFACE_EXTENSION_NAME ];
     {$ELSEIF Defined( MSWINDOWS ) }
     info.instance_extension_names := info.instance_extension_names + [ VK_KHR_WIN32_SURFACE_EXTENSION_NAME   ];
     {$ELSEIF Defined( VK_USE_PLATFORM_METAL_EXT ) }
     info.instance_extension_names := info.instance_extension_names + [ VK_EXT_METAL_SURFACE_EXTENSION_NAME   ];
     {$ELSEIF Defined( VK_USE_PLATFORM_WAYLAND_KHR ) }
     info.instance_extension_names := info.instance_extension_names + [ VK_KHR_WAYLAND_SURFACE_EXTENSION_NAME ];
     {$ELSE}
     info.instance_extension_names := info.instance_extension_names + [ VK_KHR_XCB_SURFACE_EXTENSION_NAME     ];
     {$ENDIF}
end;

procedure init_device_extension_names( var info:T_sample_info );
begin
     info.device_extension_names := info.device_extension_names + [ VK_KHR_SWAPCHAIN_EXTENSION_NAME ];
end;

procedure init_window_size( var info:T_sample_info; default_width,default_height:UInt32 );
begin
     {$IFDEF Android }
     AndroidGetWindowSize( @info.width, @info.height );
     {$ELSE}
     info.width  := default_width;
     info.height := default_height;
     {$ENDIF}
end;

procedure init_connection( var info:T_sample_info );
begin
     {$IF Defined( VK_USE_PLATFORM_XCB_KHR ) }
     var setup :P_xcb_setup_t;
     var iter  :T_xcb_screen_iterator_t;
     var scr   :T_int;

     info.connection := xcb_connect( nil, @scr );
     if ( info.connection = nil ) or xcb_connection_has_error( info.connection ) then
     begin
          Log.d( 'Unable to make an XCB connection');
          RunError( 256-1 );
     end;

     setup := xcb_get_setup( info.connection );
     iter  := xcb_setup_roots_iterator( setup );
     while scr > 0 do
     begin
          xcb_screen_next( @iter );  Dec( scr );
     end;

     info.screen := iter.data;
     {$ELSEIF Defined( VK_USE_PLATFORM_WAYLAND_KHR ) }
     info.display := wl_display_connect( nil );

     if info.display = nil then
     begin
          Log.d( 'Cannot find a compatible Vulkan installable client driver ''(ICD).\nExiting ...' );
          RunError( 1 );
     end;

     info.registry := wl_display_get_registry( info.display );
     wl_registry_add_listener( info.registry, @registry_listener, @info );
     wl_display_dispatch( info.display );
     {$ENDIF}
end;

{$IFDEF MSWINDOWS }

procedure run( var info:T_sample_info );
begin
     (* Placeholder for samples that want to show dynamic content *)
end;

function WndProc( hwnd:HWND; uMsg:UINT; wParam:WPARAM; lParam:LPARAM ) :LRESULT; stdcall;
var
   info :P_sample_info;
begin
     info := P_sample_info( GetWindowLongPtr( hWnd, GWLP_USERDATA ) );

     case (uMsg) of
     WM_CLOSE: PostQuitMessage( 0 );
     WM_PAINT: begin
                    run( info^ );
                    Exit( 0 );
               end;
     else
     end;
     Result := DefWindowProc( hWnd, uMsg, wParam, lParam );
end;

procedure init_window( var info:T_sample_info );
var
   win_class :WNDCLASSEX;
   wr        :TRect;
begin
     Assert( info.width  > 0 );
     Assert( info.height > 0 );

     info.connection := GetModuleHandle( nil );
     info.name       := 'Sample';

     // Initialize the window class structure:
     win_class.cbSize        := SizeOf( WNDCLASSEX );
     win_class.style         := CS_HREDRAW or CS_VREDRAW;
     win_class.lpfnWndProc   := @WndProc;
     win_class.cbClsExtra    := 0;
     win_class.cbWndExtra    := 0;
     win_class.hInstance     := info.connection;  // hInstance
     win_class.hIcon         := LoadIcon( 0, IDI_APPLICATION );
     win_class.hCursor       := LoadCursor( 0, IDC_ARROW );
     win_class.hbrBackground := HBRUSH( GetStockObject( WHITE_BRUSH ) );
     win_class.lpszMenuName  := nil;
     win_class.lpszClassName := LPCWSTR( WideString( info.name ) );
     win_class.hIconSm       := LoadIcon( 0, IDI_WINLOGO );
     // Register window class:
     if RegisterClassEx( win_class ) = 0 then
     begin
          // It didn't work, so try to give a useful error:
          Log.d( 'Unexpected error trying to start the application!' );
          RunError( 1 );
     end;
     // Create window with the registered class:
     wr := TRect.Create( 0, 0, info.width, info.height );
     AdjustWindowRect( wr, WS_OVERLAPPEDWINDOW, False );
     info.window := CreateWindowEx( 0,
                                    LPCWSTR( WideString( info.name ) ),               // class name
                                    LPCWSTR( WideString( info.name ) ),               // app name
                                    WS_OVERLAPPEDWINDOW or WS_VISIBLE or WS_SYSMENU,  // window style
                                    100, 100,                                         // x/y coords
                                    wr.right - wr.left,                               // width
                                    wr.bottom - wr.top,                               // height
                                    0,                                                // handle to parent
                                    0,                                                // handle to menu
                                    info.connection,                                  // hInstance
                                    nil );                                            // no extra parameters
     if info.window = 0 then
     begin
          // It didn't work, so try to give a useful error:
          Log.d( 'Cannot create a window in which to draw!');
          RunError( 1 );
     end;
     SetWindowLongPtr( info.window, GWLP_USERDATA, LONG_PTR(@info) );
end;

procedure destroy_window( var info:T_sample_info );
begin
     vkDestroySurfaceKHR(info.inst, info.surface, nil);
     DestroyWindow(info.window);
end;

{$ELSEIF Defined( VK_USE_PLATFORM_METAL_EXT ) }

// iOS & macOS: init_window() implemented externally to allow access to Objective-C components

procedure destroy_window( var info:T_sample_info );
begin
     info.caMetalLayer := nil;
end;

{$ELSEIF Defined( Android ) }

// Android implementation.
procedure init_window( var info:T_sample_info );
begin

end;

procedure destroy_window( var info:T_sample_info );
begin

end;

{$ELSEIF Defined( VK_USE_PLATFORM_WAYLAND_KHR ) }

procedure init_window( var info:T_sample_info );
begin
     Assert( info.width  > 0 );
     Assert( info.height > 0 );

     info.window := wl_compositor_create_surface( info.compositor );
     if info.window = 0 then
     begin
          Log.d( 'Can not create wayland_surface from compositor!' );
          RunError( 1 );
     end;

     info.shell_surface := wl_shell_get_shell_surface( info.shell, info.window );
     if info.shell_surface = 0 then
     begin
          Log.d( 'Can not get shell_surface from wayland_surface!' );
          RunError( 1 );
     end;

     wl_shell_surface_add_listener( info.shell_surface, @shell_surface_listener, @info );
     wl_shell_surface_set_toplevel( info.shell_surface );
end;

procedure destroy_window( var info:T_sample_info );
begin
     wl_shell_surface_destroy( info.shell_surface );
     wl_surface_destroy( info.window );
     wl_shell_destroy( info.shell );
     wl_compositor_destroy( info.compositor );
     wl_registry_destroy( info.registry );
     wl_display_disconnect( info.display );
end;

{$ELSE}

procedure init_window( var info:T_sample_info );
var
   value_mask :T_uint32_t;
   value_list :array [ 0..32-1 ] of T_uint32_t;
   cookie     :T_xcb_intern_atom_cookie_t;
   reply      :P_xcb_intern_atom_reply_t;
   cookie2    :T_xcb_intern_atom_cookie_t;
   e          :P_xcb_generic_event_t;
const
     coords   :array [ 0..2-1 ] of T_uint32_t = ( 100, 100 );
begin
     Assert( info.width  > 0 );
     Assert( info.height > 0 );

     info.window := xcb_generate_id(info.connection);

     value_mask    := XCB_CW_BACK_PIXEL or XCB_CW_EVENT_MASK;
     value_list[0] := info.screen.black_pixel;
     value_list[1] := XCB_EVENT_MASK_KEY_RELEASE or XCB_EVENT_MASK_EXPOSURE;

     xcb_create_window( info.connection,
                        XCB_COPY_FROM_PARENT,
                        info.window,
                        info.screen->root,
                        0, 0,
                        info.width, info.height,
                        0,
                        XCB_WINDOW_CLASS_INPUT_OUTPUT,
                        info.screen.root_visual,
                        value_mask,
                        value_list );

     (* Magic code that will send notification when window is destroyed *)
     cookie := xcb_intern_atom( info.connection, 1, 12, 'WM_PROTOCOLS' );
     reply := xcb_intern_atom_reply( info.connection, cookie, 0 );

     cookie2 := xcb_intern_atom( info.connection, 0, 16, 'WM_DELETE_WINDOW' );
     info.atom_wm_delete_window := xcb_intern_atom_reply( info.connection, cookie2, 0 );

     xcb_change_property( info.connection, XCB_PROP_MODE_REPLACE, info.window, reply^.atom, 4, 32, 1, @info.atom_wm_delete_window^.atom );
     free( reply );

     xcb_map_window( info.connection, info.window );

     // Force the x/y coordinates to 100,100 results are identical in consecutive
     // runs
     xcb_configure_window( info.connection, info.window, XCB_CONFIG_WINDOW_X or XCB_CONFIG_WINDOW_Y, coords );
     xcb_flush( info.connection );

     while e = xcb_wait_for_event( info.connection ) do
     begin
          if ( e->response_type and not $80 ) = XCB_EXPOSE then Break;
     end;
end;

procedure destroy_window( var info:T_sample_info );
begin
     vkDestroySurfaceKHR( info.inst, info.surface, nil );
     xcb_destroy_window( info.connection, info.window );
     xcb_disconnect( info.connection );
end;

{$ENDIF}

end. //######################################################################### ■