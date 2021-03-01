unit LUX.GPU.Vulkan;

interface //#################################################################### ■

uses System.Classes,
     vulkan_core, vulkan_win32,
     vulkan.util,
     LUX.GPU.Vulkan.root,
     LUX.GPU.Vulkan.Device,
     LUX.GPU.Vulkan.Buffer,
     LUX.GPU.Vulkan.Shader,
     LUX.GPU.Vulkan.Pipeline;

type //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【型】

     TVulkan       = class;
     TVkInstance   = TVkInstance<TVulkan>;
     TVkDevices    = TVkDevices<TVulkan>;
     TVkShaderVert = TVkShaderVert<TVulkan>;
     TVkShaderFrag = TVkShaderFrag<TVulkan>;
     TVkPipeline   = TVkPipeline<TVulkan>;

     TVkLayers = TArray<T_layer_properties>;

     //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【レコード】

     //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【クラス】

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TVulkan

     TVulkan = class
     private
     protected
       _Instance :TVkInstance;
       _Devices  :TVkDevices;
       _Layers   :TVkLayers;
       ///// メソッド
       function init_global_extension_properties( var L_:T_layer_properties ) :VkResult;
       function init_global_layer_properties :VkResult;
     public
       Info :T_sample_info;
       constructor Create;
       procedure AfterConstruction; override;
       destructor Destroy; override;
       ///// プロパティ
       property Instance :TVkInstance read _Instance write _Instance;
       property Devices  :TVkDevices  read _Devices  write _Devices ;
       property Layers   :TVkLayers   read _Layers   write _Layers  ;
     end;

//const //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【定数】

//var //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【変数】

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【ルーチン】

implementation //############################################################### ■

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【レコード】

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【クラス】

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TVulkan

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

function TVulkan.init_global_extension_properties( var L_:T_layer_properties ) :VkResult;
var
   EsN :UInt32;
begin
     repeat
           Result := vkEnumerateInstanceExtensionProperties( L_.properties.layerName, @EsN, nil );
           if Result <> VK_SUCCESS then Exit;

           if EsN = 0 then Exit( VK_SUCCESS );

           SetLength( L_.instance_extensions, EsN );
           Result := vkEnumerateInstanceExtensionProperties( L_.properties.layerName, @EsN, @L_.instance_extensions[0] );

     until Result <> VK_INCOMPLETE;
end;

function TVulkan.init_global_layer_properties :VkResult;
var
   LsN, I :UInt32;
   Ls :TArray<VkLayerProperties>;
   L :T_layer_properties;
begin
     (*
      * It's possible, though very rare, that the number of
      * instance layers could change. For example, installing something
      * could include new layers that the loader would pick up
      * between the initial query for the count and the
      * request for VkLayerProperties. The loader indicates that
      * by returning a VK_INCOMPLETE status and will update the
      * the count parameter.
      * The count parameter will be updated with the number of
      * entries loaded into the data pointer - in case the number
      * of layers went down or is smaller than the size given.
      *)
     repeat
           Result := vkEnumerateInstanceLayerProperties( @LsN, nil );
           if Result <> VK_SUCCESS then Exit;

           if LsN = 0 then Exit( VK_SUCCESS );

           SetLength( Ls, LsN );

           Result := vkEnumerateInstanceLayerProperties( @LsN, @Ls[0] );

     until Result <> VK_INCOMPLETE;

     (*
      * Now gather the extension list for each instance layer.
      *)
     for I := 0 to LsN-1 do
     begin
          L.properties := Ls[I];
          Result := init_global_extension_properties( L );
          if Result <> VK_SUCCESS then Exit;
          _Layers := _Layers + [ L ];
     end;
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TVulkan.Create;
begin
     inherited;

     init_global_layer_properties;
end;

procedure TVulkan.AfterConstruction;
begin
     inherited;

end;

destructor TVulkan.Destroy;
begin

     inherited;
end;

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【ルーチン】

//############################################################################## □

initialization //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ 初期化

finalization //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ 最終化

end. //######################################################################### ■