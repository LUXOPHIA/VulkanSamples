unit Main;

{ https://github.com/LunarG/VulkanSamples/tree/master/API-Samples/14-init_pipeline }

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
Create Graphics Pipeline
*)

(* This is part of the draw cube progression *)

(* We've setup cmake to process 14-init_pipeline.vert and 14-init_pipeline.frag           *)
(* files containing the glsl shader code for this sample.  The generate-spirv script uses *)
(* glslangValidator to compile the glsl into spir-v and places the spir-v into a struct   *)
(* into a generated header file                                                           *)

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
    const sample_title = 'Graphics Pipeline Sample';
  public
    { public 宣言 }
    info :T_sample_info;
  end;

var
  Form1: TForm1;

implementation //############################################################### ■

{$R *.fmx}

uses cube_data;

procedure TForm1.FormCreate(Sender: TObject);
const
     depthPresent :T_bool = True;
var
   res                  :VkResult;
   __init_pipeline_vert :TMemoryStream;
   __init_pipeline_frag :TMemoryStream;
   vert_info            :VkShaderModuleCreateInfo;
   frag_info            :VkShaderModuleCreateInfo;
   dynamicStateEnables  :array [ 0..2-1 ] of VkDynamicState;  // Viewport + Scissor
   dynamicState         :VkPipelineDynamicStateCreateInfo;
   vi                   :VkPipelineVertexInputStateCreateInfo;
   ia                   :VkPipelineInputAssemblyStateCreateInfo;
   rs                   :VkPipelineRasterizationStateCreateInfo;
   cb                   :VkPipelineColorBlendStateCreateInfo;
   att_state            :array [ 0..1-1 ] of VkPipelineColorBlendAttachmentState;
   vp                   :VkPipelineViewportStateCreateInfo;
   ds                   :VkPipelineDepthStencilStateCreateInfo;
   ms                   :VkPipelineMultisampleStateCreateInfo;
   pipeline             :VkGraphicsPipelineCreateInfo;
begin
     __init_pipeline_vert := TMemoryStream.Create;
     __init_pipeline_frag := TMemoryStream.Create;

     init_global_layer_properties( info );
     init_instance_extension_names( info );
     init_device_extension_names( info );
     init_instance( info, sample_title );
     init_enumerate_device( info );
     init_window_size( info, 500, 500 );
     init_connection( info );
     init_window( info );
     init_swapchain_extension( info );
     init_device( info );
     init_command_pool( info );
     init_command_buffer( info );
     execute_begin_command_buffer( info );
     init_device_queue( info );
     init_swap_chain( info );
     init_depth_buffer( info );
     init_uniform_buffer( info );
     init_renderpass( info, depthPresent );
     init_framebuffers( info, depthPresent );
     init_vertex_buffer( info, @g_vb_solid_face_colors_Data[0], sizeof(g_vb_solid_face_colors_Data ),
                         SizeOf( T_Vertex ) * Length( g_vb_solid_face_colors_Data ), False );
     init_descriptor_and_pipeline_layouts( info, false );
     init_descriptor_pool( info, false );
     init_descriptor_set( info, false );
     __init_pipeline_vert.LoadFromFile( '../../_DATA/14-init_pipeline.vert' );
     __init_pipeline_frag.LoadFromFile( '../../_DATA/14-init_pipeline.frag' );
     vert_info.sType    := VK_STRUCTURE_TYPE_SHADER_MODULE_CREATE_INFO;
     frag_info.sType    := VK_STRUCTURE_TYPE_SHADER_MODULE_CREATE_INFO;
     vert_info.codeSize := __init_pipeline_vert.Size;
     vert_info.pCode    := __init_pipeline_vert.Memory;
     frag_info.codeSize := __init_pipeline_frag.Size;
     frag_info.pCode    := __init_pipeline_frag.Memory;
     init_shaders( info, @vert_info, @frag_info );

     (* VULKAN_KEY_START *)
     dynamicState.sType             := VK_STRUCTURE_TYPE_PIPELINE_DYNAMIC_STATE_CREATE_INFO;
     dynamicState.pNext             := nil;
     dynamicState.pDynamicStates    := @dynamicStateEnables[0];
     dynamicState.dynamicStateCount := 0;

     vi.sType                           := VK_STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_STATE_CREATE_INFO;
     vi.pNext                           := nil;
     vi.flags                           := 0;
     vi.vertexBindingDescriptionCount   := 1;
     vi.pVertexBindingDescriptions      := @info.vi_binding;
     vi.vertexAttributeDescriptionCount := 2;
     vi.pVertexAttributeDescriptions    := @info.vi_attribs[0];

     ia.sType                  := VK_STRUCTURE_TYPE_PIPELINE_INPUT_ASSEMBLY_STATE_CREATE_INFO;
     ia.pNext                  := nil;
     ia.flags                  := 0;
     ia.primitiveRestartEnable := VK_FALSE;
     ia.topology               := VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST;

     rs.sType                   := VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_CREATE_INFO;
     rs.pNext                   := nil;
     rs.flags                   := 0;
     rs.polygonMode             := VK_POLYGON_MODE_FILL;
     rs.cullMode                := Ord( VK_CULL_MODE_BACK_BIT );
     rs.frontFace               := VK_FRONT_FACE_CLOCKWISE;
     rs.depthClampEnable        := VK_FALSE;
     rs.rasterizerDiscardEnable := VK_FALSE;
     rs.depthBiasEnable         := VK_FALSE;
     rs.depthBiasConstantFactor := 0;
     rs.depthBiasClamp          := 0;
     rs.depthBiasSlopeFactor    := 0;
     rs.lineWidth               := 1.0;

     cb.sType := VK_STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_STATE_CREATE_INFO;
     cb.pNext := nil;
     cb.flags := 0;
     att_state[0].colorWriteMask      := $f;
     att_state[0].blendEnable         := VK_FALSE;
     att_state[0].alphaBlendOp        := VK_BLEND_OP_ADD;
     att_state[0].colorBlendOp        := VK_BLEND_OP_ADD;
     att_state[0].srcColorBlendFactor := VK_BLEND_FACTOR_ZERO;
     att_state[0].dstColorBlendFactor := VK_BLEND_FACTOR_ZERO;
     att_state[0].srcAlphaBlendFactor := VK_BLEND_FACTOR_ZERO;
     att_state[0].dstAlphaBlendFactor := VK_BLEND_FACTOR_ZERO;
     cb.attachmentCount   := 1;
     cb.pAttachments      := @att_state[0];
     cb.logicOpEnable     := VK_FALSE;
     cb.logicOp           := VK_LOGIC_OP_NO_OP;
     cb.blendConstants[0] := 1.0;
     cb.blendConstants[1] := 1.0;
     cb.blendConstants[2] := 1.0;
     cb.blendConstants[3] := 1.0;

     vp.sType         := VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_STATE_CREATE_INFO;
     vp.pNext         := nil;
     vp.flags         := 0;
     vp.viewportCount := NUM_VIEWPORTS;
     dynamicStateEnables[dynamicState.dynamicStateCount] := VK_DYNAMIC_STATE_VIEWPORT;  Inc( dynamicState.dynamicStateCount );
     vp.scissorCount  := NUM_SCISSORS;
     dynamicStateEnables[dynamicState.dynamicStateCount] := VK_DYNAMIC_STATE_SCISSOR ;  Inc( dynamicState.dynamicStateCount );
     vp.pScissors     := nil;
     vp.pViewports    := nil;

     ds.sType                 := VK_STRUCTURE_TYPE_PIPELINE_DEPTH_STENCIL_STATE_CREATE_INFO;
     ds.pNext                 := nil;
     ds.flags                 := 0;
     ds.depthTestEnable       := VK_TRUE;
     ds.depthWriteEnable      := VK_TRUE;
     ds.depthCompareOp        := VK_COMPARE_OP_LESS_OR_EQUAL;
     ds.depthBoundsTestEnable := VK_FALSE;
     ds.minDepthBounds        := 0;
     ds.maxDepthBounds        := 0;
     ds.stencilTestEnable     := VK_FALSE;
     ds.back.failOp           := VK_STENCIL_OP_KEEP;
     ds.back.passOp           := VK_STENCIL_OP_KEEP;
     ds.back.compareOp        := VK_COMPARE_OP_ALWAYS;
     ds.back.compareMask      := 0;
     ds.back.reference        := 0;
     ds.back.depthFailOp      := VK_STENCIL_OP_KEEP;
     ds.back.writeMask        := 0;
     ds.front                 := ds.back;

     ms.sType                 := VK_STRUCTURE_TYPE_PIPELINE_MULTISAMPLE_STATE_CREATE_INFO;
     ms.pNext                 := nil;
     ms.flags                 := 0;
     ms.pSampleMask           := nil;
     ms.rasterizationSamples  := NUM_SAMPLES;
     ms.sampleShadingEnable   := VK_FALSE;
     ms.alphaToCoverageEnable := VK_FALSE;
     ms.alphaToOneEnable      := VK_FALSE;
     ms.minSampleShading      := 0.0;

     pipeline.sType               := VK_STRUCTURE_TYPE_GRAPHICS_PIPELINE_CREATE_INFO;
     pipeline.pNext               := nil;
     pipeline.layout              := info.pipeline_layout;
     pipeline.basePipelineHandle  := VK_NULL_HANDLE;
     pipeline.basePipelineIndex   := 0;
     pipeline.flags               := 0;
     pipeline.pVertexInputState   := @vi;
     pipeline.pInputAssemblyState := @ia;
     pipeline.pRasterizationState := @rs;
     pipeline.pColorBlendState    := @cb;
     pipeline.pTessellationState  := nil;
     pipeline.pMultisampleState   := @ms;
     pipeline.pDynamicState       := @dynamicState;
     pipeline.pViewportState      := @vp;
     pipeline.pDepthStencilState  := @ds;
     pipeline.pStages             := @info.shaderStages[0];
     pipeline.stageCount          := 2;
     pipeline.renderPass          := info.render_pass;
     pipeline.subpass             := 0;

     res := vkCreateGraphicsPipelines( info.device, VK_NULL_HANDLE, 1, @pipeline, nil, @info.pipeline );
     Assert( res = VK_SUCCESS, 'vkCreateGraphicsPipelines' );
     execute_end_command_buffer( info );
     execute_queue_command_buffer( info );
     (* VULKAN_KEY_END *)

     __init_pipeline_vert.Free;
     __init_pipeline_frag.Free;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
     vkDestroyPipeline( info.device, info.pipeline, nil );
     destroy_descriptor_pool( info );
     destroy_vertex_buffer( info );
     destroy_framebuffers( info );
     destroy_shaders( info );
     destroy_renderpass( info );
     destroy_descriptor_and_pipeline_layouts( info );
     destroy_uniform_buffer( info );
     destroy_depth_buffer( info );
     destroy_swap_chain( info );
     destroy_command_buffer( info );
     destroy_command_pool( info );
     destroy_device( info );
     destroy_window( info );
     destroy_instance( info );
end;

end. //######################################################################### ■
