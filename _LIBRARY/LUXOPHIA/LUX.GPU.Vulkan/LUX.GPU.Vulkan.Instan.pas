unit LUX.GPU.Vulkan.Instan;

interface //#################################################################### ■

uses System.Classes,
     vulkan_core,
     LUX.GPU.Vulkan.root,
     LUX.GPU.Vulkan.Window,
     LUX.GPU.Vulkan.Device;

type //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【型】

     TVkInstan<TVulkan_:class> = class;

     //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【レコード】

     //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【クラス】

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TVkInstan

     TVkInstan<TVulkan_:class> = class( TVkObject<TVulkan_> )
     private
       type TVkInstan_  = TVkInstan<TVulkan_>;
            TVkDevices_ = TVkDevices<TVkInstan_>;
            TVkWindow_  = TVkWindow<TVkInstan_>;
     protected
       _Vulkan  :TVulkan_;
       _Name    :String;
       _Applic  :VkApplicationInfo;
       _Layeres :TStringList;
       _Extenss :TStringList;
       _Inform  :VkInstanceCreateInfo;
       _Handle  :VkInstance;
       _Devices :TVkDevices_;
       _Window  :TVkWindow_;
       ///// アクセス
       function GetHandle :VkInstance;
       ///// メソッド
       procedure CreateHandle;
       procedure DestroHandle;
     public
       constructor Create( const Vulkan_:TVulkan_ );
       destructor Destroy; override;
       ///// プロパティ
       property Vulkan  :TVulkan_             read   _Vulkan                ;
       property Name    :String               read   _Name    write _Name   ;
       property Applic  :VkApplicationInfo    read   _Applic                ;
       property Layers  :TStringList          read   _Layeres               ;
       property Extenss :TStringList          read   _Extenss               ;
       property Inform  :VkInstanceCreateInfo read   _Inform                ;
       property Handle  :VkInstance           read GetHandle                ;
       property Devices :TVkDevices_          read   _Devices write _Devices;
       property Window  :TVkWindow_           read   _Window  write _Window ;
     end;

//const //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【定数】

//var //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【変数】

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【ルーチン】

implementation //############################################################### ■

uses System.AnsiStrings,
     vulkan_win32,
     LUX.GPU.Vulkan;

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【レコード】

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【クラス】

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TVkInstan

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

function TVkInstan<TVulkan_>.GetHandle :VkInstance;
begin
     if not Assigned( _Handle ) then CreateHandle;

     Result := _Handle;
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

procedure TVkInstan<TVulkan_>.CreateHandle;
var
   L, E :String;
   Ls, Es :TArray<PAnsiChar>;
begin
     for L in _Layeres do Ls := Ls + [ System.AnsiStrings.StrNew( PAnsiChar( AnsiString( L ) ) ) ];
     for E in _Extenss do Es := Es + [ System.AnsiStrings.StrNew( PAnsiChar( AnsiString( E ) ) ) ];

     with _Inform do
     begin
          sType                   := VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO;
          pNext                   := nil;
          flags                   := 0;
          pApplicationInfo        := @_Applic;
          enabledLayerCount       := Length( Ls );
          ppEnabledLayerNames     := @Ls[0];
          enabledExtensionCount   := Length( Es );
          ppEnabledExtensionNames := @Es[0];
     end;

     Assert( vkCreateInstance( @_Inform, nil, @_Handle ) = VK_SUCCESS );
end;

procedure TVkInstan<TVulkan_>.DestroHandle;
begin
     vkDestroyInstance( _Handle, nil );
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TVkInstan<TVulkan_>.Create( const Vulkan_:TVulkan_ );
begin
     inherited;

     _Layeres := TStringList.Create;
     _Extenss := TStringList.Create;

     _Vulkan := Vulkan_;

     TVulkan( Vulkan_ ).Instans := TVkInstan( Self );

     with _Applic do
     begin
          sType              := VK_STRUCTURE_TYPE_APPLICATION_INFO;
          pNext              := nil;
          pApplicationName   := PAnsiChar( AnsiString( _Name ) );
          applicationVersion := 1;
          pEngineName        := PAnsiChar( AnsiString( _Name ) );
          engineVersion      := 1;
          apiVersion         := VK_API_VERSION_1_0;
     end;

     _Extenss.Add( VK_KHR_SURFACE_EXTENSION_NAME       );
     _Extenss.Add( VK_KHR_WIN32_SURFACE_EXTENSION_NAME );

     _Handle := nil;

     CreateHandle;
end;

destructor TVkInstan<TVulkan_>.Destroy;
begin
     _Devices.Free;

     if Assigned( _Handle ) then DestroHandle;

     _Layeres.Free;
     _Extenss.Free;

     inherited;
end;

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【ルーチン】

//############################################################################## □

initialization //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ 初期化

finalization //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ 最終化

end. //######################################################################### ■