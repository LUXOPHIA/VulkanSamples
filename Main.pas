unit Main;

{ https://github.com/LunarG/VulkanSamples/tree/master/API-Samples/09-init_descriptor_set }

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
Allocate Descriptor Set
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
    const sample_title = 'Allocate Descriptor Set Sample';
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
   res             :VkResult;
   type_count      :array [ 0..1-1 ] of VkDescriptorPoolSize;
   descriptor_pool :VkDescriptorPoolCreateInfo;
   alloc_info      :array [ 0..1-1 ] of VkDescriptorSetAllocateInfo;
   writes          :array [ 0..1-1 ] of VkWriteDescriptorSet;
begin
     init_global_layer_properties( info );
     init_instance( info, sample_title );
     init_enumerate_device( info );
     init_queue_family_index( info );
     init_device( info );
     init_uniform_buffer( info );
     init_descriptor_and_pipeline_layouts( info, false );

     (* VULKAN_KEY_START *)
     type_count[0].type_           := VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER;
     type_count[0].descriptorCount := 1;

     descriptor_pool.sType         := VK_STRUCTURE_TYPE_DESCRIPTOR_POOL_CREATE_INFO;
     descriptor_pool.pNext         := nil;
     descriptor_pool.maxSets       := 1;
     descriptor_pool.poolSizeCount := 1;
     descriptor_pool.pPoolSizes    := @type_count[0];

     res := vkCreateDescriptorPool(info.device, @descriptor_pool, nil, @info.desc_pool );
     Assert( res = VK_SUCCESS );

     alloc_info[0].sType              := VK_STRUCTURE_TYPE_DESCRIPTOR_SET_ALLOCATE_INFO;
     alloc_info[0].pNext              := nil;
     alloc_info[0].descriptorPool     := info.desc_pool;
     alloc_info[0].descriptorSetCount := NUM_DESCRIPTOR_SETS;
     alloc_info[0].pSetLayouts        := @info.desc_layout[0];

     SetLength( info.desc_set, NUM_DESCRIPTOR_SETS );
     res := vkAllocateDescriptorSets( info.device, @alloc_info[0], @info.desc_set[0] );
     Assert( res = VK_SUCCESS );

     writes[0].sType           := VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET;
     writes[0].pNext           := nil;
     writes[0].dstSet          := info.desc_set[0];
     writes[0].descriptorCount := 1;
     writes[0].descriptorType  := VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER;
     writes[0].pBufferInfo     := @info.uniform_data.buffer_info;
     writes[0].dstArrayElement := 0;
     writes[0].dstBinding      := 0;

     vkUpdateDescriptorSets( info.device, 1, @writes[0], 0, nil );
     (* VULKAN_KEY_END *)
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
     vkDestroyDescriptorPool( info.device, info.desc_pool, nil );
     destroy_uniform_buffer( info );
     destroy_descriptor_and_pipeline_layouts( info );
     destroy_device( info );
     destroy_instance( info );
end;

end. //######################################################################### ■
