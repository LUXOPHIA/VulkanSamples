unit LUX.GPU.Vulkan.Shader;

interface //#################################################################### ■

uses vulkan_core, vulkan_win32;

type //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【型】

     TVkShader<TVkPipeli_:class>     = class;
     TVkShaderVert<TVkPipeli_:class> = class;
     TVkShaderFrag<TVkPipeli_:class> = class;

     //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【レコード】

     //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【クラス】

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TVkShader

     TVkShader<TVkPipeli_:class> = class
     private
     protected
       _Pipeli :TVkPipeli_;
       _Module :VkShaderModuleCreateInfo;
       _Stage  :VkPipelineShaderStageCreateInfo;
       ///// アクセス
       ///// メソッド
       procedure CreateModule; virtual;
       procedure DestroModule; virtual;
     public
       constructor Create( const Pipeli_:TVkPipeli_ );
       destructor Destroy; override;
       ///// プロパティ
       property Pipeli :TVkPipeli_                      read _Pipeli;
       property Module :VkShaderModuleCreateInfo        read _Module;
       property Stage  :VkPipelineShaderStageCreateInfo read _Stage ;
       ///// メソッド
       procedure LoadFromFile( const FileName_:String );
     end;

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TVkShaderVert

     TVkShaderVert<TVkPipeli_:class> = class( TVkShader<TVkPipeli_> )
     private
     protected
     public
       constructor Create( const Pipeli_:TVkPipeli_ );
       destructor Destroy; override;
     end;

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TVkShaderFrag

     TVkShaderFrag<TVkPipeli_:class> = class( TVkShader<TVkPipeli_> )
     private
     protected
     public
       constructor Create( const Pipeli_:TVkPipeli_ );
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

procedure TVkShader<TVkPipeli_>.CreateModule;
begin
     Assert( vkCreateShaderModule( TVkPipeli( _Pipeli ).Device.Handle, @_Module, nil, @_Stage.module ) = VK_SUCCESS );
end;

procedure TVkShader<TVkPipeli_>.DestroModule;
begin
     vkDestroyShaderModule( TVkPipeli( _Pipeli ).Device.Handle, _Stage.module, nil );
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TVkShader<TVkPipeli_>.Create( const Pipeli_:TVkPipeli_ );
begin
     inherited Create;

     _Pipeli := Pipeli_;

     TVkPipeli( _Pipeli ).Shaders.Add( TVkShader( Self ) );

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

destructor TVkShader<TVkPipeli_>.Destroy;
begin
     if _Stage.module > 0 then DestroModule;

     inherited;
end;

/////////////////////////////////////////////////////////////////////// メソッド

procedure TVkShader<TVkPipeli_>.LoadFromFile( const FileName_:String );
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

constructor TVkShaderVert<TVkPipeli_>.Create( const Pipeli_:TVkPipeli_ );
begin
     inherited;

     _Stage.stage := VK_SHADER_STAGE_VERTEX_BIT;
end;

destructor TVkShaderVert<TVkPipeli_>.Destroy;
begin

     inherited;
end;

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TVkShaderFrag

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TVkShaderFrag<TVkPipeli_>.Create( const Pipeli_:TVkPipeli_ );
begin
     inherited;

     _Stage.stage := VK_SHADER_STAGE_FRAGMENT_BIT;
end;

destructor TVkShaderFrag<TVkPipeli_>.Destroy;
begin

     inherited;
end;

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【ルーチン】

//############################################################################## □

initialization //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ 初期化

finalization //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ 最終化

end. //######################################################################### ■