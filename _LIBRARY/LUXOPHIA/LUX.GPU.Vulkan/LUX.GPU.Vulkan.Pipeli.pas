unit LUX.GPU.Vulkan.Pipeli;

interface //#################################################################### ■

uses System.Generics.Collections,
     vulkan_core, vulkan_win32,
     vulkan.util,
     LUX.GPU.Vulkan.Shader;

type //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【型】

     TVkPipeli<TVkDevice_:class> = class;

     //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【レコード】

     //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【クラス】

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TVkPipeli

     TVkPipeli<TVkDevice_:class> = class
     private
       type TVkPipeli_  = TVkPipeli<TVkDevice_>;
            TVkShader_  = TVkShader<TVkPipeli_>;
            TVkShaders_ = TObjectList<TVkShader_>;
            T_dynamicStateEnables = array [ 0..2-1 ] of VkDynamicState;
     protected
       _Device    :TVkDevice_;
       _Handle    :VkPipeline;
       _DepthTest :Boolean;
       _Shaders   :TVkShaders_;
       ///// アクセス
       function GetHandle :VkPipeline;
       procedure SetHandle( const Handle_:VkPipeline );
       ///// メソッド
       procedure CreateHandle;
       procedure DestroHandle;
     public
       constructor Create; overload;
       constructor Create( const Device_:TVkDevice_; const DepthTest_:Boolean ); overload;
       destructor Destroy; override;
       ///// プロパティ
       property Device  :TVkDevice_  read   _Device                 ;
       property Handle  :VkPipeline  read GetHandle  write SetHandle;
       property Shaders :TVkShaders_ read   _Shaders                ;
     end;

//const //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【定数】

//var //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【変数】

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【ルーチン】

implementation //############################################################### ■

uses LUX.GPU.Vulkan;

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【レコード】

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【クラス】

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TVkPipeli

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

/////////////////////////////////////////////////////////////////////// アクセス

function TVkPipeli<TVkDevice_>.GetHandle :VkPipeline;
begin
     if _Handle = 0 then CreateHandle;

     Result := _Handle;
end;

procedure TVkPipeli<TVkDevice_>.SetHandle( const Handle_:VkPipeline );
begin
     if _Handle <> 0 then DestroHandle;

     _Handle := Handle_;
end;

/////////////////////////////////////////////////////////////////////// メソッド

procedure TVkPipeli<TVkDevice_>.CreateHandle;
var
   res :VkResult;
   dynamicStateEnables :T_dynamicStateEnables;  // Viewport + Scissor
   dynamicState        :VkPipelineDynamicStateCreateInfo;
   vi                  :VkPipelineVertexInputStateCreateInfo;
   ia                  :VkPipelineInputAssemblyStateCreateInfo;
   rs                  :VkPipelineRasterizationStateCreateInfo;
   cb                  :VkPipelineColorBlendStateCreateInfo;
   att_state           :array [ 0..1-1 ] of VkPipelineColorBlendAttachmentState;
   vp                  :VkPipelineViewportStateCreateInfo;
   ds                  :VkPipelineDepthStencilStateCreateInfo;
   ms                  :VkPipelineMultisampleStateCreateInfo;
   pipeline            :VkGraphicsPipelineCreateInfo;

   shaderStages : TArray<VkPipelineShaderStageCreateInfo>;
   S :TVkShader_;
begin
     dynamicStateEnables            := Default( T_dynamicStateEnables );
     dynamicState                   := Default( VkPipelineDynamicStateCreateInfo );
     dynamicState.sType             := VK_STRUCTURE_TYPE_PIPELINE_DYNAMIC_STATE_CREATE_INFO;
     dynamicState.pNext             := nil;
     dynamicState.pDynamicStates    := @dynamicStateEnables[0];
     dynamicState.dynamicStateCount := 0;

     vi       := Default( VkPipelineVertexInputStateCreateInfo );
     vi.sType := VK_STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_STATE_CREATE_INFO;
     vi.pNext                           := nil;
     vi.flags                           := 0;
     vi.vertexBindingDescriptionCount   := 1;
     vi.pVertexBindingDescriptions      := @TVkDevice( _Device ).Instan.Vulkan.Info.vi_binding;
     vi.vertexAttributeDescriptionCount := 2;
     vi.pVertexAttributeDescriptions    := @TVkDevice( _Device ).Instan.Vulkan.Info.vi_attribs[0];

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
     cb.flags := 0;
     cb.pNext := nil;

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

     vp               := Default( VkPipelineViewportStateCreateInfo );
     vp.sType         := VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_STATE_CREATE_INFO;
     vp.pNext         := nil;
     vp.flags         := 0;
     vp.viewportCount := NUM_VIEWPORTS;
     dynamicStateEnables[dynamicState.dynamicStateCount] := VK_DYNAMIC_STATE_VIEWPORT;  Inc( dynamicState.dynamicStateCount );
     vp.scissorCount  := NUM_SCISSORS;
     dynamicStateEnables[dynamicState.dynamicStateCount] := VK_DYNAMIC_STATE_SCISSOR ;  Inc( dynamicState.dynamicStateCount );
     vp.pScissors     := nil;
     vp.pViewports    := nil;

     var include_depth :VkBool32;
     if _DepthTest then include_depth := 1
                   else include_depth := 0;

     ds.sType                 := VK_STRUCTURE_TYPE_PIPELINE_DEPTH_STENCIL_STATE_CREATE_INFO;
     ds.pNext                 := nil;
     ds.flags                 := 0;
     ds.depthTestEnable       := include_depth;
     ds.depthWriteEnable      := include_depth;
     ds.depthCompareOp        := VK_COMPARE_OP_LESS_OR_EQUAL;
     ds.depthBoundsTestEnable := VK_FALSE;
     ds.stencilTestEnable     := VK_FALSE;
     ds.back.failOp           := VK_STENCIL_OP_KEEP;
     ds.back.passOp           := VK_STENCIL_OP_KEEP;
     ds.back.compareOp        := VK_COMPARE_OP_ALWAYS;
     ds.back.compareMask      := 0;
     ds.back.reference        := 0;
     ds.back.depthFailOp      := VK_STENCIL_OP_KEEP;
     ds.back.writeMask        := 0;
     ds.minDepthBounds        := 0;
     ds.maxDepthBounds        := 0;
     ds.stencilTestEnable     := VK_FALSE;
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
     pipeline.layout              := TVkDevice( _Device ).Instan.Vulkan.Info.pipeline_layout;
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

     for S in _Shaders do shaderStages := shaderStages + [ S.Stage ];

     pipeline.pStages             := @shaderStages[0];
     pipeline.stageCount          := Length( shaderStages );

     pipeline.renderPass          := TVkDevice( _Device ).Instan.Vulkan.Info.render_pass;
     pipeline.subpass             := 0;

     res := vkCreateGraphicsPipelines( TVkDevice( _Device ).Handle, TVkDevice( _Device ).Instan.Vulkan.Info.pipelineCache, 1, @pipeline, nil, @_Handle );
     Assert( res = VK_SUCCESS );
end;

procedure TVkPipeli<TVkDevice_>.DestroHandle;
begin
     vkDestroyPipeline( TVkDevice( _Device ).Handle, _Handle, nil );
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TVkPipeli<TVkDevice_>.Create;
begin
     inherited;

     _Handle := 0;

     _Shaders := TVkShaders_.Create;
end;

constructor TVkPipeli<TVkDevice_>.Create( const Device_:TVkDevice_; const DepthTest_:Boolean );
begin
     Create;

     _Device    := Device_;
     _DepthTest := DepthTest_;
end;

destructor TVkPipeli<TVkDevice_>.Destroy;
begin
     _Shaders.Free;

      Handle := 0;

     inherited;
end;

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【ルーチン】

//############################################################################## □

initialization //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ 初期化

finalization //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ 最終化

end. //######################################################################### ■