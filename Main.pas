unit Main;

{ https://github.com/LunarG/VulkanSamples/blob/master/API-Samples/01-init_instance }

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
create and destroy Vulkan instance
*)

(* This is part of the draw cube progression *)

interface //####################################################################

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  vulkan_core,
  vulkan.util, vulkan.util_init;

type
  TForm1 = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { private 宣言 }
    const APP_SHORT_NAME = 'vulkansamples_instance';
  public
    { public 宣言 }
    info :T_sample_info;
  end;

var
  Form1: TForm1;

implementation //###############################################################

{$R *.fmx}

procedure TForm1.FormCreate(Sender: TObject);
var
   app_info  :VkApplicationInfo;
   inst_info :VkInstanceCreateInfo;
   inst      :VkInstance;
   res       :VkResult;
begin
     Caption := APP_SHORT_NAME;

     init_global_layer_properties( info );

     (* VULKAN_KEY_START *)

     // initialize the VkApplicationInfo structure
     app_info.sType              := VK_STRUCTURE_TYPE_APPLICATION_INFO;
     app_info.pNext              := nil;
     app_info.pApplicationName   := APP_SHORT_NAME;
     app_info.applicationVersion := 1;
     app_info.pEngineName        := APP_SHORT_NAME;
     app_info.engineVersion      := 1;
     app_info.apiVersion         := VK_API_VERSION_1_0;

     // initialize the VkInstanceCreateInfo structure
     inst_info.sType                   := VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO;
     inst_info.pNext                   := nil;
     inst_info.flags                   := 0;
     inst_info.pApplicationInfo        := @app_info;
     inst_info.enabledExtensionCount   := 0;
     inst_info.ppEnabledExtensionNames := nil;
     inst_info.enabledLayerCount       := 0;
     inst_info.ppEnabledLayerNames     := nil;

     res := vkCreateInstance( @inst_info, nil, @inst );
     if res = VK_ERROR_INCOMPATIBLE_DRIVER then
     begin
          Log.d( 'cannot find a compatible Vulkan ICD' );
          RunError( 256-1 );
     end
     else
     if res <> VK_SUCCESS then
     begin
          Log.d( 'unknown error' );
          RunError( 256-1 );
     end;

     vkDestroyInstance( inst, nil );

     (* VULKAN_KEY_END *)
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
     /////
end;

end. //#########################################################################
