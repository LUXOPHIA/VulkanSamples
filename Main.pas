unit Main;

{ https://github.com/LunarG/VulkanSamples/tree/master/API-Samples/02-enumerate_devices }

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
enumerate physical devices
*)

const APP_SHORT_NAME = 'vulkansamples_enumerate';

procedure TForm1.FormCreate(Sender: TObject);
var
   info      :T_sample_info;
   gpu_count :T_uint32_t;
   res       :VkResult;
begin
     Caption := APP_SHORT_NAME;

     init_global_layer_properties( info );
     init_instance( info, APP_SHORT_NAME );

     (* VULKAN_KEY_START *)

     gpu_count := 1;
     res := vkEnumeratePhysicalDevices( info.inst, @gpu_count, nil );
     Assert( gpu_count > 0 );
     SetLength( info.gpus, gpu_count );
     res := vkEnumeratePhysicalDevices( info.inst, @gpu_count, @info.gpus[ 0 ] );
     Assert( ( res = VK_SUCCESS ) and ( gpu_count >= 1 ) );

     Memo1.Lines.Add( 'gpu_count = ' + gpu_count.ToString );

     (* VULKAN_KEY_END *)

     vkDestroyInstance( info.inst, nil );
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
     /////
end;

end. //#########################################################################
