unit Main;

{ https://github.com/LunarG/VulkanSamples/tree/master/API-Samples/08-init_pipeline_layout }

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
Create Descriptor Layout and Pipeline Layout
*)

(* This is part of the draw cube progression *)

interface //#################################################################### ■

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Memo.Types, FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo,
  vulkan_core, vulkan_win32,
  vulkan.util, vulkan.util_init,
  LUX, LUX.Code.C,
  LUX.D1, LUX.D2, LUX.D3, LUX.D4, LUX.D4x4;

type
  TForm1 = class(TForm)
    Memo1: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { private 宣言 }
    const sample_title = 'Descriptor / Pipeline Layout Sample';
  public
    { public 宣言 }
    info :T_sample_info;
  end;

var
  Form1: TForm1;

implementation //############################################################### ■

{$R *.fmx}

uses System.Math;

procedure TForm1.FormCreate(Sender: TObject);
var
   res                      :VkResult;
   layout_binding           :VkDescriptorSetLayoutBinding;
   descriptor_layout        :VkDescriptorSetLayoutCreateInfo;
   pipelineLayoutCreateInfo :VkPipelineLayoutCreateInfo;
begin
     init_global_layer_properties( info );
     init_instance( info, sample_title );
     init_enumerate_device( info );
     init_queue_family_index( info );
     init_device( info );

     (* VULKAN_KEY_START *)
     (* Start with just our uniform buffer that has our transformation matrices
      * (for the vertex shader). The fragment shader we intend to use needs no
      * external resources, so nothing else is necessary
      *)

     (* Note that when we start using textures, this is where our sampler will
      * need to be specified
      *)
     layout_binding.binding            := 0;
     layout_binding.descriptorType     := VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER;
     layout_binding.descriptorCount    := 1;
     layout_binding.stageFlags         := VkShaderStageFlags( VK_SHADER_STAGE_VERTEX_BIT );
     layout_binding.pImmutableSamplers := nil;

     (* Next take layout bindings and use them to create a descriptor set layout
      *)
     descriptor_layout.sType        := VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_CREATE_INFO;
     descriptor_layout.pNext        := nil;
     descriptor_layout.bindingCount := 1;
     descriptor_layout.pBindings    := @layout_binding;

     SetLength( info.desc_layout, NUM_DESCRIPTOR_SETS );
     res := vkCreateDescriptorSetLayout( info.device, @descriptor_layout, nil, @info.desc_layout[0] );
     Assert( res = VK_SUCCESS );

     (* Now use the descriptor layout to create a pipeline layout *)
     pipelineLayoutCreateInfo.sType                  := VK_STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO;
     pipelineLayoutCreateInfo.pNext                  := nil;
     pipelineLayoutCreateInfo.pushConstantRangeCount := 0;
     pipelineLayoutCreateInfo.pPushConstantRanges    := nil;
     pipelineLayoutCreateInfo.setLayoutCount         := NUM_DESCRIPTOR_SETS;
     pipelineLayoutCreateInfo.pSetLayouts            := @info.desc_layout[0];

     res := vkCreatePipelineLayout( info.device, @pipelineLayoutCreateInfo, nil, @info.pipeline_layout );
     Assert( res = VK_SUCCESS );
     (* VULKAN_KEY_END *)
end;

procedure TForm1.FormDestroy(Sender: TObject);
var
   i :T_int;
begin
     for i := 0 to NUM_DESCRIPTOR_SETS-1 do vkDestroyDescriptorSetLayout( info.device, info.desc_layout[i], nil );
     vkDestroyPipelineLayout( info.device, info.pipeline_layout, nil );
     destroy_device( info );
     destroy_instance( info );
end;

end. //######################################################################### ■
