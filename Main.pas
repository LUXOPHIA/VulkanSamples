unit Main;

{ https://github.com/LunarG/VulkanSamples/tree/master/API-Samples/16-vulkan_1_1 }

(*
 * Vulkan Samples
 *
 * Copyright (C) 2015-2017 Valve Corporation
 * Copyright (C) 2015-2017 LunarG, Inc.
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
Determine if the current system can use Vulkan 1.1 API features
*)

interface //#################################################################### ■

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Memo.Types, FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo,
  vulkan_core,
  vulkan.util, vulkan.util_init,
  LUX, LUX.Code.C;

type
  TForm1 = class(TForm)
    Memo1: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { private 宣言 }
    const APP_SHORT_NAME = 'vulkan_1_1_sample';
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
   using_major_version      :T_uint16_t;
   using_minor_version      :T_uint16_t;
   using_version_string     :String;
   desired_major_version    :T_uint16_t;
   desired_minor_version    :T_uint16_t;
   desired_version          :T_uint32_t;
   desired_version_string   :String;
   instance                 :VkInstance;
   physical_devices_desired :TArray<VkPhysicalDevice>;
   api_version              :T_uint32_t;
   loader_major_version     :T_uint32_t;
   loader_minor_version     :T_uint32_t;
   app_info                 :VkApplicationInfo;
   inst_info                :VkInstanceCreateInfo;
   phys_dev_count           :T_uint32_t;
   physical_devices         :TArray<VkPhysicalDevice>;
   dev                      :T_uint32_t;
   physical_device_props    :VkPhysicalDeviceProperties;
begin
     init_global_layer_properties( info );

     (* VULKAN_KEY_START *)

     // Keep track of the major/minor version we can actually use
     using_major_version  := 1;
     using_minor_version  := 0;

     // Set the desired version we want
     desired_major_version  := 1;
     desired_minor_version  := 1;
     desired_version        := VK_MAKE_VERSION( desired_major_version, desired_minor_version, 0 );
     desired_version_string := desired_major_version.ToString
                             + '.'
                             + desired_minor_version.ToString;
     instance := VkInstance( VK_NULL_HANDLE );

     // Determine what API version is available
     if VK_SUCCESS = vkEnumerateInstanceVersion( @api_version ) then
     begin
          // Translate the version into major/minor for easier comparison
          loader_major_version := VK_VERSION_MAJOR( api_version );
          loader_minor_version := VK_VERSION_MINOR( api_version );
          Log.d( 'Loader/Runtime support detected for Vulkan ' + loader_major_version.ToString + '.' + loader_minor_version.ToString );

          // Check current version against what we want to run
          if ( loader_major_version > desired_major_version ) or
             ( loader_major_version = desired_major_version ) and ( loader_minor_version >= desired_minor_version ) then
          begin
               // Initialize the VkApplicationInfo structure with the version of the API we're intending to use
               app_info.sType              := VK_STRUCTURE_TYPE_APPLICATION_INFO;
               app_info.pNext              := nil;
               app_info.pApplicationName   := APP_SHORT_NAME;
               app_info.applicationVersion := 1;
               app_info.pEngineName        := APP_SHORT_NAME;
               app_info.engineVersion      := 1;
               app_info.apiVersion         := desired_version;

               // Initialize the VkInstanceCreateInfo structure
               inst_info.sType                   := VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO;
               inst_info.pNext                   := nil;
               inst_info.flags                   := 0;
               inst_info.pApplicationInfo        := @app_info;
               inst_info.enabledExtensionCount   := 0;
               inst_info.ppEnabledExtensionNames := nil;
               inst_info.enabledLayerCount       := 0;
               inst_info.ppEnabledLayerNames     := nil;

               // Attempt to create the instance
               if VK_SUCCESS <> vkCreateInstance( @inst_info, nil, @instance ) then
               begin
                    Log.d( 'Unknown error creating ' + desired_version_string + ' Instance' );
                    RunError( 256-1 );
               end;

               // Get the list of physical devices
               phys_dev_count := 1;
               if ( VK_SUCCESS <> vkEnumeratePhysicalDevices( instance, @phys_dev_count, nil ) ) or ( phys_dev_count = 0 ) then
               begin
                    Log.d( 'Failed searching for Vulkan physical devices' );
                    RunError( 256-1 );
               end;
               SetLength( physical_devices, phys_dev_count );
               if ( VK_SUCCESS <> vkEnumeratePhysicalDevices( instance, @phys_dev_count, @physical_devices[0] ) ) or
                    ( phys_dev_count = 0 ) then
               begin
                    Log.d( 'Failed enumerating Vulkan physical devices' );
                    RunError( 256-1 );
               end;

               // Go through the list of physical devices and select only those that are capable of running the API version we want.
               for dev := 0 to Length( physical_devices )-1 do
               begin
                    vkGetPhysicalDeviceProperties( physical_devices[dev], @physical_device_props );
                    if physical_device_props.apiVersion >= desired_version
                    then physical_devices_desired := physical_devices_desired
                                                   + [ physical_devices[dev] ];
               end;

               // If we have something in the desired version physical device list, we're good
               if Length( physical_devices_desired ) > 0 then
               begin
                    using_major_version := desired_major_version;
                    using_minor_version := desired_minor_version;
               end;
          end;
     end;

     using_version_string := using_major_version.ToString
                           + '.'
                           + using_minor_version.ToString;

     if using_minor_version < desired_minor_version
     then Log.d( 'Determined that this system can only use Vulkan API version ' + using_version_string
               + ' instead of desired version ' + desired_version_string )
     else Log.d( 'Determined that this system can run desired Vulkan API version ' + desired_version_string );

     // Destroy the instance if it was created
     if VkInstance( VK_NULL_HANDLE ) <> instance then vkDestroyInstance( instance, nil );

     (* VULKAN_KEY_END *)
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
     /////
end;

end. //######################################################################### ■
