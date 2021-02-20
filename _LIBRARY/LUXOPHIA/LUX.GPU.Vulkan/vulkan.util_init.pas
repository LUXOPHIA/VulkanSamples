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

uses vulkan_core,
     vulkan.util,
     LUX.Code.C;

//type //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【型】

     //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【レコード】

     //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【クラス】

//const //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【定数】

//var //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【変数】

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【ルーチン】

function init_global_extension_properties( var layer_props_:T_layer_properties ) :VkResult;
function init_global_layer_properties( var info_:T_sample_info ) :VkResult;

function init_instance( var info_:T_sample_info; const app_short_name_:P_char ) :VkResult;

function init_device_extension_properties( var info_:T_sample_info; var layer_props_:T_layer_properties ) :VkResult;
function init_enumerate_device( var info_:T_sample_info; gpu_count_:T_uint32_t = 1 ) :VkResult;
procedure destroy_instance( var info_:T_sample_info );

implementation //############################################################### ■

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【レコード】

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【クラス】

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【ルーチン】

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
           instance_extensions := @layer_props_.instance_extensions[ 0 ];
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

           Result := vkEnumerateInstanceLayerProperties( @instance_layer_count, @vk_props[ 0 ] );

        until Result <> VK_INCOMPLETE;

     (*
      * Now gather the extension list for each instance layer.
      *)
     for i := 0 to instance_layer_count-1 do
     begin
          layer_props.properties := vk_props[ i ];
          Result := init_global_extension_properties( layer_props );
          if Result <> VK_SUCCESS then Exit;
          info_.instance_layer_properties := info_.instance_layer_properties + [ layer_props ];
     end;
     vk_props := nil;
end;

//------------------------------------------------------------------------------

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
    inst_info.ppEnabledExtensionNames  := @info_.instance_extension_names[ 0 ];

    Result := vkCreateInstance( @inst_info, nil, @info_.inst );
    Assert( Result = VK_SUCCESS );
end;

//------------------------------------------------------------------------------

function init_device_extension_properties( var info_:T_sample_info; var layer_props_:T_layer_properties ) :VkResult;
var
   device_extensions      :P_VkExtensionProperties;
   device_extension_count :T_uint32_t;
   layer_name             :P_char;
begin
     layer_name := layer_props_.properties.layerName;

     repeat
           Result := vkEnumerateDeviceExtensionProperties( info_.gpus[ 0 ], layer_name, @device_extension_count, nil );
           if Result <> VK_SUCCESS then Exit;

           if device_extension_count = 0 then Exit( VK_SUCCESS );

           SetLength( layer_props_.device_extensions, device_extension_count );
           device_extensions := @layer_props_.device_extensions[ 0 ];
           Result := vkEnumerateDeviceExtensionProperties( info_.gpus[ 0 ], layer_name, @device_extension_count, device_extensions );

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

     Result := vkEnumeratePhysicalDevices( info_.inst, @gpu_count_, @info_.gpus[ 0 ] );
     Assert( ( Result = VK_SUCCESS ) and ( gpu_count_ >= req_count ) );

     vkGetPhysicalDeviceQueueFamilyProperties( info_.gpus[ 0 ], @info_.queue_family_count, nil );
     Assert( info_.queue_family_count >= 1 );

     SetLength( info_.queue_props, info_.queue_family_count );
     vkGetPhysicalDeviceQueueFamilyProperties( info_.gpus[ 0 ], @info_.queue_family_count, @info_.queue_props[ 0 ] );
     Assert( info_.queue_family_count >= 1 );

     (* This is as good a place as any to do this *)
     vkGetPhysicalDeviceMemoryProperties( info_.gpus[ 0 ], @info_.memory_properties );
     vkGetPhysicalDeviceProperties( info_.gpus[ 0 ], @info_.gpu_props );
     (* query device extensions for enabled layers *)
     for I := 0 to Length( info_.instance_layer_properties )-1
     do init_device_extension_properties( info_, info_.instance_layer_properties[ I ] );
end;

procedure destroy_instance( var info_:T_sample_info );
begin
     vkDestroyInstance( info_.inst, nil );
end;

end. //######################################################################### ■