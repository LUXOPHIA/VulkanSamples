unit LUX.GPU.Vulkan.Instance;

interface //#################################################################### ■

uses System.Classes,
     vulkan_core, vulkan_win32,
     LUX.GPU.Vulkan.root,
     LUX.GPU.Vulkan.Device;

type //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【型】

     TVkInstance<TVulkan_:class> = class;

     //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【レコード】

     //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【クラス】

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TVkInstance

     TVkInstance<TVulkan_:class> = class( TVkObject<TVulkan_> )
     private
       type TVkInstance_ = TVkInstance<TVulkan_>;
            TVkDevices_  = TVkDevices<TVkInstance_>;
     protected
       _Handle     :VkInstance;
       _Name       :String;
       _Layers     :TStringList;
       _Extensions :TStringList;
       _Devices    :TVkDevices_;
       ///// メソッド
       procedure CreateHandle;
       procedure DestroHandle;
     public
       constructor Create( const Vulkan_:TVulkan_ );
       procedure AfterConstruction; override;
       destructor Destroy; override;
       ///// プロパティ
       property Handle     :VkInstance  read _Handle                ;
       property Name       :String      read _Name       write _Name;
       property Layers     :TStringList read _Layers                ;
       property Extensions :TStringList read _Extensions            ;
       property Devices    :TVkDevices_ read _Devices               ;
     end;

//const //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【定数】

//var //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【変数】

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【ルーチン】

implementation //############################################################### ■

uses System.AnsiStrings;

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【レコード】

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【クラス】

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TVkInstance

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

procedure TVkInstance<TVulkan_>.CreateHandle;
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

     Assert( vkCreateInstance( @inst_info, nil, @_Handle ) = VK_SUCCESS );
end;

procedure TVkInstance<TVulkan_>.DestroHandle;
begin
     vkDestroyInstance( _Handle, nil );
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TVkInstance<TVulkan_>.Create( const Vulkan_:TVulkan_ );
begin
     inherited;

     _Layers     := TStringList.Create;
     _Extensions := TStringList.Create;

     _Extensions.Add( VK_KHR_SURFACE_EXTENSION_NAME       );
     _Extensions.Add( VK_KHR_WIN32_SURFACE_EXTENSION_NAME );

     CreateHandle;

     _Devices := TVkDevices_.Create( Self );
end;

procedure TVkInstance<TVulkan_>.AfterConstruction;
begin
     inherited;

end;

destructor TVkInstance<TVulkan_>.Destroy;
begin
     _Devices.Free;

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