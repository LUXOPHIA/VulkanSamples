unit Main;

{ https://github.com/LunarG/VulkanSamples/tree/master/API-Samples/12-init_frame_buffers }

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
Initialize Framebuffer
*)

(* This is part of the draw cube progression *)

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
    const sample_title = 'Init Framebuffer Sample';
  public
    { public 宣言 }
    info :T_sample_info;
  end;

var
  Form1: TForm1;

implementation //############################################################### ■

{$R *.fmx}

procedure TForm1.FormCreate(Sender: TObject);
const
     depthPresent :T_bool = True;
var
   res         :VkResult;
   attachments :array [ 0..2-1 ] of VkImageView;
   fb_info     :VkFramebufferCreateInfo;
   i           :T_uint32_t;
begin
     init_global_layer_properties( info );
     init_instance_extension_names( info );
     init_device_extension_names( info );
     init_instance( info, sample_title );
     init_enumerate_device( info );
     init_connection( info );
     init_window_size( info, 500, 500 );
     init_window( info );
     init_swapchain_extension( info );
     init_device( info );
     init_command_pool( info );
     init_command_buffer( info );
     execute_begin_command_buffer( info );
     init_device_queue( info );
     init_swap_chain( info );
     init_depth_buffer( info );
     init_renderpass( info, depthPresent );

     (* VULKAN_KEY_START *)
     attachments[1] := info.depth.view;

     fb_info.sType           := VK_STRUCTURE_TYPE_FRAMEBUFFER_CREATE_INFO;
     fb_info.pNext           := nil;
     fb_info.renderPass      := info.render_pass;
     fb_info.attachmentCount := 2;
     fb_info.pAttachments    := @attachments[0];
     fb_info.width           := info.width;
     fb_info.height          := info.height;
     fb_info.layers          := 1;

     SetLength( info.framebuffers, info.swapchainImageCount );
     Assert( Length( info.framebuffers ) > 0 );

     for i := 0 to info.swapchainImageCount-1 do
     begin
          attachments[0] := info.buffers[i].view;
          res := vkCreateFramebuffer( info.device, @fb_info, nil, @info.framebuffers[i] );
          Assert( res = VK_SUCCESS );
     end;
     execute_end_command_buffer( info );
     execute_queue_command_buffer( info );
     (* VULKAN_KEY_END *)
end;

procedure TForm1.FormDestroy(Sender: TObject);
var
   i :T_uint32_t;
begin
     for i := 0 to info.swapchainImageCount-1 do vkDestroyFramebuffer( info.device, info.framebuffers[i], nil );
     info.framebuffers := nil;

     destroy_renderpass( info );
     destroy_depth_buffer( info );
     destroy_swap_chain( info );
     destroy_command_buffer( info );
     destroy_command_pool( info );
     destroy_device( info );
     destroy_window( info );
     destroy_instance( info );
end;

end. //######################################################################### ■
