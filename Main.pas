unit Main;

{ https://github.com/LunarG/VulkanSamples/tree/master/API-Samples/11-init_shaders }

(*
 * Vulkan Samples
 *
 * Copyright (C) 2015-2020 Valve Corporation
 * Copyright (C) 2015-2020 LunarG, Inc.
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
Initialize Vertex and Fragment Shaders
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
    const sample_title = 'Initialize Shaders Sample';
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
   res                 :VkResult;
   vtx_spv             :TArray<T_unsigned_int>;
   moduleCreateInfo    :VkShaderModuleCreateInfo;
   frag_spv            :TArray<T_unsigned_int>;
   __init_shaders_vert :TMemoryStream;
   __init_shaders_frag :TMemoryStream;
begin
     __init_shaders_vert := TMemoryStream.Create;
     __init_shaders_frag := TMemoryStream.Create;

     init_global_layer_properties( info );
     init_instance( info, sample_title );
     init_enumerate_device( info );
     init_queue_family_index( info );
     init_device( info );

     (* We've setup cmake to process 11-init_shaders.vert and 11-init_shaders.frag             *)
     (* files containing the glsl shader code for this sample.  The generate-spirv script uses *)
     (* glslangValidator to compile the glsl into spir-v and places the spir-v into a struct   *)
     (* into a generated header file                                                           *)

     (* VULKAN_KEY_START *)

     info.shaderStages[0].sType               := VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO;
     info.shaderStages[0].pNext               := nil;
     info.shaderStages[0].pSpecializationInfo := nil;
     info.shaderStages[0].flags               := 0;
     info.shaderStages[0].stage               := VK_SHADER_STAGE_VERTEX_BIT;
     info.shaderStages[0].pName               := 'main';
     {#include ""}

     __init_shaders_vert.LoadFromFile( '../../_DATA/11-init_shaders.vert' );

     moduleCreateInfo.sType    := VK_STRUCTURE_TYPE_SHADER_MODULE_CREATE_INFO;
     moduleCreateInfo.pNext    := nil;
     moduleCreateInfo.flags    := 0;
     moduleCreateInfo.codeSize := __init_shaders_vert.Size;
     moduleCreateInfo.pCode    := __init_shaders_vert.Memory;
     res := vkCreateShaderModule( info.device, @moduleCreateInfo, nil, @info.shaderStages[0].module );
     assert( res = VK_SUCCESS );

     info.shaderStages[1].sType := VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO;
     info.shaderStages[1].pNext := nil;
     info.shaderStages[1].pSpecializationInfo := nil;
     info.shaderStages[1].flags := 0;
     info.shaderStages[1].stage := VK_SHADER_STAGE_FRAGMENT_BIT;
     info.shaderStages[1].pName := 'main';

     __init_shaders_frag.LoadFromFile( '../../_DATA/11-init_shaders.frag' );

     moduleCreateInfo.sType    := VK_STRUCTURE_TYPE_SHADER_MODULE_CREATE_INFO;
     moduleCreateInfo.pNext    := nil;
     moduleCreateInfo.flags    := 0;
     moduleCreateInfo.codeSize := __init_shaders_frag.Size;
     moduleCreateInfo.pCode    := __init_shaders_frag.Memory;
     res := vkCreateShaderModule( info.device, @moduleCreateInfo, nil, @info.shaderStages[1].module );
     assert( res = VK_SUCCESS );

     (* VULKAN_KEY_END *)

     __init_shaders_vert.Free;
     __init_shaders_frag.Free;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
     vkDestroyShaderModule( info.device, info.shaderStages[0].module, nil );
     vkDestroyShaderModule( info.device, info.shaderStages[1].module, nil );
     destroy_device( info );
     destroy_instance( info );
end;

end. //######################################################################### ■
