unit Main;

{ https://github.com/LunarG/VulkanSamples/tree/master/API-Samples/04-init_command_buffer }

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
  public
    { public 宣言 }
  end;

var
  Form1: TForm1;

implementation //###############################################################

{$R *.fmx}

(*
VULKAN_SAMPLE_SHORT_DESCRIPTION
Create Vulkan command buffer
*)

(* This is part of the draw cube progression *)

const sample_title = 'Command Buffer Sample';

procedure TForm1.FormCreate(Sender: TObject);
var
   res           :VkResult;
   info          :T_sample_info;
   cmd_pool_info :VkCommandPoolCreateInfo;
   cmd           :VkCommandBufferAllocateInfo;
   cmd_bufs      :array [ 0..0 ] of VkCommandBuffer;
begin
     Caption := sample_title;

     init_global_layer_properties( info );
     init_instance( info, sample_title );
     init_enumerate_device( info );
     init_queue_family_index( info );
     init_device( info );

     (* VULKAN_KEY_START *)

     (* Create a command pool to allocate our command buffer from *)
     cmd_pool_info.sType            := VK_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO;
     cmd_pool_info.pNext            := nil;
     cmd_pool_info.queueFamilyIndex := info.graphics_queue_family_index;
     cmd_pool_info.flags            := 0;

     res := vkCreateCommandPool( info.device, @cmd_pool_info, nil, @info.cmd_pool );
     Assert( res = VK_SUCCESS );

     Memo1.Lines.Add( 'info.cmd_pool = ' + UInt64( info.cmd_pool ).ToHexString );

     (* Create the command buffer from the command pool *)
     cmd.sType              := VK_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO;
     cmd.pNext              := nil;
     cmd.commandPool        := info.cmd_pool;
     cmd.level              := VK_COMMAND_BUFFER_LEVEL_PRIMARY;
     cmd.commandBufferCount := 1;

     res := vkAllocateCommandBuffers( info.device, @cmd, @info.cmd );
     Assert( res = VK_SUCCESS );

     Memo1.Lines.Add( 'info.cmd = ' + UInt64( info.cmd ).ToHexString );

     (* VULKAN_KEY_END *)

     cmd_bufs[ 0 ] := info.cmd;
     vkFreeCommandBuffers( info.device, info.cmd_pool, 1, @cmd_bufs[ 0 ] );
     vkDestroyCommandPool( info.device, info.cmd_pool, nil );
     destroy_device( info );
     destroy_instance( info );
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
     /////
end;

end. //#########################################################################
