unit LUX.GPU.Vulkan.Shader;

interface //#################################################################### ■

uses vulkan_core, vulkan_win32,
     LUX.GPU.Vulkan.root;

type //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【型】

     //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【レコード】

     //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【クラス】

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TVkShader

     TVkShader<TVulkan_:class> = class( TVkObject<TVulkan_> )
     private
     protected
       _Module :VkShaderModuleCreateInfo;
       _Stage  :VkPipelineShaderStageCreateInfo;
       ///// アクセス
       ///// メソッド
       procedure CreateModule; virtual;
       procedure DestroModule; virtual;
     public
       constructor Create( const Vulkan_:TVulkan_ );
       procedure AfterConstruction; override;
       destructor Destroy; override;
       ///// プロパティ
       property Module :VkShaderModuleCreateInfo        read _Module;
       property Stage  :VkPipelineShaderStageCreateInfo read _Stage ;
       ///// メソッド
       procedure LoadFromFile( const FileName_:String );
     end;

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TVkShaderVert

     TVkShaderVert<TVulkan_:class> = class( TVkShader<TVulkan_> )
     private
     protected
     public
       constructor Create( const Vulkan_:TVulkan_ );
       procedure AfterConstruction; override;
       destructor Destroy; override;
     end;

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TVkShaderFrag

     TVkShaderFrag<TVulkan_:class> = class( TVkShader<TVulkan_> )
     private
     protected
     public
       constructor Create( const Vulkan_:TVulkan_ );
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

procedure TVkShader<TVulkan_>.CreateModule;
begin
     Assert( vkCreateShaderModule( TVulkan( Vulkan ).Devices.Devices[0].Handle, @_Module, nil, @_Stage.module ) = VK_SUCCESS );
end;

procedure TVkShader<TVulkan_>.DestroModule;
begin
     vkDestroyShaderModule( TVulkan( Vulkan ).Devices.Devices[0].Handle, _Stage.module, nil );
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TVkShader<TVulkan_>.Create( const Vulkan_:TVulkan_ );
begin
     inherited;

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

procedure TVkShader<TVulkan_>.AfterConstruction;
begin
     inherited;

end;

destructor TVkShader<TVulkan_>.Destroy;
begin
     if _Stage.module > 0 then DestroModule;

     inherited;
end;

/////////////////////////////////////////////////////////////////////// メソッド

procedure TVkShader<TVulkan_>.LoadFromFile( const FileName_:String );
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

constructor TVkShaderVert<TVulkan_>.Create( const Vulkan_:TVulkan_ );
begin
     inherited;

     _Stage.stage := VK_SHADER_STAGE_VERTEX_BIT;
end;

procedure TVkShaderVert<TVulkan_>.AfterConstruction;
begin
     inherited;

end;

destructor TVkShaderVert<TVulkan_>.Destroy;
begin

     inherited;
end;

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TVkShaderFrag

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TVkShaderFrag<TVulkan_>.Create( const Vulkan_:TVulkan_ );
begin
     inherited;

     _Stage.stage := VK_SHADER_STAGE_FRAGMENT_BIT;
end;

procedure TVkShaderFrag<TVulkan_>.AfterConstruction;
begin
     inherited;

end;

destructor TVkShaderFrag<TVulkan_>.Destroy;
begin

     inherited;
end;

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【ルーチン】

//############################################################################## □

initialization //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ 初期化

finalization //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ 最終化

end. //######################################################################### ■