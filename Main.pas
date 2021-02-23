unit Main;

{ https://github.com/LunarG/VulkanSamples/tree/master/API-Samples/03-init_device }

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
create and destroy a Vulkan physical device
*)

(* This is part of the draw cube progression *)

interface //####################################################################

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  vulkan_core,
  vulkan.util, vulkan.util_init,
  LUX.Code.C, FMX.Memo.Types, FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo;

type
  TForm1 = class(TForm)
    Memo1: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { private 宣言 }
    const APP_SHORT_NAME = 'vulkansamples_device';
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
   queue_info       :VkDeviceQueueCreateInfo;
   found            :T_bool;
   i                :T_unsigned_int;
   queue_priorities :array [ 0..0 ] of T_float;
   device_info      :VkDeviceCreateInfo;
   device           :VkDevice;
   res              :VkResult;
begin
     Caption := APP_SHORT_NAME;

     init_global_layer_properties( info );
     init_instance( info, APP_SHORT_NAME );

     init_enumerate_device( info );

     (* VULKAN_KEY_START *)

     vkGetPhysicalDeviceQueueFamilyProperties( info.gpus[0], @info.queue_family_count, nil );
     Assert( info.queue_family_count >= 1 );

     SetLength( info.queue_props, info.queue_family_count );
     vkGetPhysicalDeviceQueueFamilyProperties( info.gpus[0], @info.queue_family_count, @info.queue_props[0] );
     Assert( info.queue_family_count >= 1 );

     Memo1.Lines.Add( 'info.queue_family_count = ' + info.queue_family_count.ToString );
     for i := 0 to info.queue_family_count-1 do
     begin
          Memo1.Lines.Add( 'info.queue_props[ ' + i.ToString + ' ]' );
          Memo1.Lines.Add( '    .queueFlags = ' + info.queue_props[i].queueFlags.ToHexString );
          Memo1.Lines.Add( '    .queueCount = ' + info.queue_props[i].queueCount.ToString );
     end;

     found := false;
     for i := 0 to info.queue_family_count-1 do
     begin
          if ( info.queue_props[i].queueFlags and VkQueueFlags( VK_QUEUE_GRAPHICS_BIT ) ) > 0 then
          begin
               queue_info.queueFamilyIndex := i;
               found := true;
               Break;
          end;
     end;
     Assert( found );

     if found then Memo1.Lines.Add( 'found = True'  )
              else Memo1.Lines.Add( 'found = False' );

     queue_priorities[0] := 0;
     queue_info.sType            := VK_STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO;
     queue_info.pNext            := nil;
     queue_info.queueCount       := 1;
     queue_info.pQueuePriorities := @queue_priorities[0];

     device_info.sType                   := VK_STRUCTURE_TYPE_DEVICE_CREATE_INFO;
     device_info.pNext                   := nil;
     device_info.queueCreateInfoCount    := 1;
     device_info.pQueueCreateInfos       := @queue_info;
     device_info.enabledExtensionCount   := 0;
     device_info.ppEnabledExtensionNames := nil;
     device_info.enabledLayerCount       := 0;
     device_info.ppEnabledLayerNames     := nil;
     device_info.pEnabledFeatures        := nil;

     res := vkCreateDevice( info.gpus[0], @device_info, nil, @device );
     assert( res = VK_SUCCESS );

     vkDestroyDevice( device, nil );

     (* VULKAN_KEY_END *)
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
     destroy_instance( info );
end;

end. //#########################################################################
