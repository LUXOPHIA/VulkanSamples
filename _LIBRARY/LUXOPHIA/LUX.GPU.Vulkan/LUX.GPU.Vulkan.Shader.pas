unit LUX.GPU.Vulkan.Shader;

interface //#################################################################### ■

uses vulkan_core, vulkan_win32;

type //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【型】

     //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【レコード】

     //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【クラス】

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TVkShader

     TVkShader<TVkPipeline_:class> = class
     private
     protected
       _Pipeline :TVkPipeline_;
       _Module   :VkShaderModuleCreateInfo;
       _Stage    :VkPipelineShaderStageCreateInfo;
       ///// アクセス
       ///// メソッド
       procedure CreateModule; virtual;
       procedure DestroModule; virtual;
     public
       constructor Create( const Pipeline_:TVkPipeline_ );
       procedure AfterConstruction; override;
       destructor Destroy; override;
       ///// プロパティ
       property Pipeline :TVkPipeline_                    read _Pipeline;
       property Module   :VkShaderModuleCreateInfo        read _Module  ;
       property Stage    :VkPipelineShaderStageCreateInfo read _Stage   ;
       ///// メソッド
       procedure LoadFromFile( const FileName_:String );
     end;

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TVkShaderVert

     TVkShaderVert<TVkPipeline_:class> = class( TVkShader<TVkPipeline_> )
     private
     protected
     public
       constructor Create( const Pipeline_:TVkPipeline_ );
       procedure AfterConstruction; override;
       destructor Destroy; override;
     end;

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TVkShaderFrag

     TVkShaderFrag<TVkPipeline_:class> = class( TVkShader<TVkPipeline_> )
     private
     protected
     public
       constructor Create( const Pipeline_:TVkPipeline_ );
       procedure AfterConstruction; override;
       destructor Destroy; override;
     end;

//const //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【定数】

//var //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【変数】

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【ルーチン】

implementation //############################################################### ■

uses System.Classes,
     LUX.GPU.Vulkan;

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【レコード】

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【クラス】

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TVkShader

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

/////////////////////////////////////////////////////////////////////// アクセス

/////////////////////////////////////////////////////////////////////// メソッド

procedure TVkShader<TVkPipeline_>.CreateModule;
begin
     Assert( vkCreateShaderModule( TVkPipeline( _Pipeline ).Device.Handle, @_Module, nil, @_Stage.module ) = VK_SUCCESS );
end;

procedure TVkShader<TVkPipeline_>.DestroModule;
begin
     vkDestroyShaderModule( TVkPipeline( _Pipeline ).Device.Handle, _Stage.module, nil );
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TVkShader<TVkPipeline_>.Create( const Pipeline_:TVkPipeline_ );
begin
     inherited Create;

     _Pipeline := Pipeline_;

     TVkPipeline( _Pipeline ).Shaders.Add( TVkShader( Self ) );

     _Module          := Default( VkShaderModuleCreateInfo );
     _Module.sType    := VkStructureType.VK_STRUCTURE_TYPE_SHADER_MODULE_CREATE_INFO;
     _Module.pNext    := nil;
     _Module.flags    := 0;
     _Module.codeSize := 0;
     _Module.pCode    := nil;

     _Stage.sType               := VkStructureType.VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO;
     _Stage.pNext               := nil;
     _Stage.flags               := 0;
  // _Stage.stage
     _Stage.pName               := 'main';
     _Stage.pSpecializationInfo := nil;
end;

procedure TVkShader<TVkPipeline_>.AfterConstruction;
begin
     inherited;

end;

destructor TVkShader<TVkPipeline_>.Destroy;
begin
     if _Stage.module > 0 then DestroModule;

     inherited;
end;

/////////////////////////////////////////////////////////////////////// メソッド

procedure TVkShader<TVkPipeline_>.LoadFromFile( const FileName_:String );
var
   F :TMemoryStream;
begin
     F := TMemoryStream.Create;
     try
          F.LoadFromFile( FileName_ );

          _Module.codeSize := F.Size;
          _Module.pCode    := F.Memory;

          CreateModule;

     finally
          F.Free;
     end;
end;

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TVkShaderVert

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TVkShaderVert<TVkPipeline_>.Create( const Pipeline_:TVkPipeline_ );
begin
     inherited;

     _Stage.stage := VK_SHADER_STAGE_VERTEX_BIT;
end;

procedure TVkShaderVert<TVkPipeline_>.AfterConstruction;
begin
     inherited;

end;

destructor TVkShaderVert<TVkPipeline_>.Destroy;
begin

     inherited;
end;

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TVkShaderFrag

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TVkShaderFrag<TVkPipeline_>.Create( const Pipeline_:TVkPipeline_ );
begin
     inherited;

     _Stage.stage := VK_SHADER_STAGE_FRAGMENT_BIT;
end;

procedure TVkShaderFrag<TVkPipeline_>.AfterConstruction;
begin
     inherited;

end;

destructor TVkShaderFrag<TVkPipeline_>.Destroy;
begin

     inherited;
end;

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【ルーチン】

//############################################################################## □

initialization //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ 初期化

finalization //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ 最終化

end. //######################################################################### ■