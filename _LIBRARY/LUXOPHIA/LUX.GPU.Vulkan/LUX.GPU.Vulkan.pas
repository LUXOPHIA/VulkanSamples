unit LUX.GPU.Vulkan;

interface //#################################################################### ■

uses System.Classes,
     vulkan_core, vulkan_win32,
     vulkan.util,
     LUX, LUX.D1, LUX.D2, LUX.D3, LUX.D4, LUX.D4x4;

type //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【型】

     //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【レコード】

     //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【クラス】

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TVulkan

     TVulkan = class
     private
     protected
     public
       Info :T_sample_info;
       constructor Create;
       procedure AfterConstruction; override;
       destructor Destroy; override;
     end;

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TVkObject

     TVkObject = class
     private
     protected
       _Vulkan :TVulkan;
     public
       constructor Create( const Vulkan_:TVulkan );
       procedure AfterConstruction; override;
       destructor Destroy; override;
     end;

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TVkInstance

     TVkInstance = class( TVkObject )
     private
     protected
       //_Handle     :VkInstance;
       _Name       :String;
       _Layers     :TStringList;
       _Extensions :TStringList;
       ///// メソッド
       procedure CreateHandle;
       procedure DestroHandle;
     public
       constructor Create( const Vulkan_:TVulkan );
       procedure AfterConstruction; override;
       destructor Destroy; override;
       ///// プロパティ
       //property Handle :VkInstance read _Handle            ;
       property Name   :String     read _Name   write _Name;
     end;

//const //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【定数】

//var //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【変数】

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【ルーチン】

implementation //############################################################### ■

uses System.SysUtils, System.AnsiStrings;

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【レコード】

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【クラス】

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TVulkan

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TVulkan.Create;
begin
     inherited;

end;

procedure TVulkan.AfterConstruction;
begin
     inherited;

end;

destructor TVulkan.Destroy;
begin

     inherited;
end;

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TVkObject

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TVkObject.Create( const Vulkan_:TVulkan );
begin
     inherited Create;

     _Vulkan := Vulkan_;
end;

procedure TVkObject.AfterConstruction;
begin
     inherited;

end;

destructor TVkObject.Destroy;
begin

     inherited;
end;

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TVkInstance

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

procedure TVkInstance.CreateHandle;
var
   app_info :VkApplicationInfo;
   L, E :String;
   Ls, Es :TArray<PAnsiChar>;
   inst_info :VkInstanceCreateInfo;
begin
     with app_info do
     begin
          sType              := VK_STRUCTURE_TYPE_APPLICATION_INFO;
          pNext              := nil;
          pApplicationName   := PAnsiChar( AnsiString( _Name ) );
          applicationVersion := 1;
          pEngineName        := PAnsiChar( AnsiString( _Name ) );
          engineVersion      := 1;
          apiVersion         := VK_API_VERSION_1_0;
     end;

     for L in _Layers     do Ls := Ls + [ System.AnsiStrings.StrNew( PAnsiChar( AnsiString( L ) ) ) ];
     for E in _Extensions do Es := Es + [ System.AnsiStrings.StrNew( PAnsiChar( AnsiString( E ) ) ) ];

     with inst_info do
     begin
          sType                   := VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO;
          pNext                   := nil;
          flags                   := 0;
          pApplicationInfo        := @app_info;
          enabledLayerCount       := Length( Ls );
          ppEnabledLayerNames     := @Ls[0];
          enabledExtensionCount   := Length( Es );
          ppEnabledExtensionNames := @Es[0];
     end;

     Assert( vkCreateInstance( @inst_info, nil, @_Vulkan.Info.inst ) = VK_SUCCESS );
end;

procedure TVkInstance.DestroHandle;
begin
     vkDestroyInstance( _Vulkan.Info.inst, nil );
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TVkInstance.Create( const Vulkan_:TVulkan );
begin
     inherited;

     _Layers     := TStringList.Create;
     _Extensions := TStringList.Create;
end;

procedure TVkInstance.AfterConstruction;
begin
     inherited;

     _Extensions.Add( VK_KHR_SURFACE_EXTENSION_NAME       );
     _Extensions.Add( VK_KHR_WIN32_SURFACE_EXTENSION_NAME );

     CreateHandle;
end;

destructor TVkInstance.Destroy;
begin
     DestroHandle;

     _Layers    .Free;
     _Extensions.Free;

     inherited;
end;

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【ルーチン】

//############################################################################## □

initialization //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ 初期化

finalization //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ 最終化

end. //######################################################################### ■