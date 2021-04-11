unit LUX.GPU.Vulkan.Instan;

interface //#################################################################### ■

uses System.Classes, System.Generics.Collections,
     vulkan_core,
     LUX.Data.List,
     LUX.GPU.Vulkan.Surfac,
     LUX.GPU.Vulkan.Device;

type //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【型】

     TVkInstans<TVulkan_:class>      = class;
       TVkInstan<TVulkan_:class>     = class;
         TVkInsInf<TVulkan_:class>   = class;
           TVkAppInf<TVulkan_:class> = class;

     //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【レコード】

     //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【クラス】

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TVkAppInf

     TVkAppInf<TVulkan_:class> = class
     private
       type TVkInsInf_ = TVkInsInf<TVulkan_>;
     protected
       _Parent :TVkInsInf_;
       _Inform :VkApplicationInfo;
       _Handle :P_VkApplicationInfo;
       ///// アクセス
       function GetKind :VkStructureType;
       procedure SetKind( const Kind_:VkStructureType );
       function GetNext :Pointer;
       procedure SetNext( const Next_:Pointer );
       function GetAppName :String;
       procedure SetAppName( const AppName_:String );
       function GetAppVers :UInt32;
       procedure SetAppVers( const AppVers_:UInt32 );
       function GetEngName :String;
       procedure SetEngName( const EngName_:String );
       function GetEngVers :UInt32;
       procedure SetEngVers( const EngVers_:UInt32 );
       function GetApiVers :UInt32;
       procedure SetApiVers( const ApiVersion_:UInt32 );
       function GetHandle :P_VkApplicationInfo;
       procedure SetHandle( const Handle_:P_VkApplicationInfo );
     public
       constructor Create; overload; virtual;
       constructor Create( const Parent_:TVkInsInf_ ); overload; virtual;
       destructor Destroy; override;
       ///// プロパティ
       property Parent  :TVkInsInf_          read   _Parent                  ;
       property Kind    :VkStructureType     read GetKind    write SetKind   ;
       property Next    :Pointer             read GetNext    write SetNext   ;
       property AppName :String              read GetAppName write SetAppName;
       property AppVers :UInt32              read GetAppVers write SetAppVers;
       property EngName :String              read GetEngName write SetEngName;
       property EngVers :UInt32              read GetEngVers write SetEngVers;
       property ApiVers :UInt32              read GetApiVers write SetApiVers;
       property Handle  :P_VkApplicationInfo read GetHandle  write SetHandle ;
     end;

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TVkInsInf

     TVkInsInf<TVulkan_:class> = class
     private
       type TVkInstan_ = TVkInstan<TVulkan_>;
            TVkAppInf_ = TVkAppInf<TVulkan_>;
     protected
       _Parent  :TVkInstan_;
       _Inform  :VkInstanceCreateInfo;
       _Applic  :TVkAppInf_;
       _Layeres :TStringList;
       _Extenss :TStringList;
       _Handle  :P_VkInstanceCreateInfo;
       ///// アクセス
       function GetKind :VkStructureType;
       procedure SetKind( const Kind_:VkStructureType );
       function GetNext :Pointer;
       procedure SetNext( const Next_:Pointer );
       function GetFlags :VkInstanceCreateFlags;
       procedure SetFlags( const Flags_:VkInstanceCreateFlags );
       function GetHandle :P_VkInstanceCreateInfo;
       procedure SetHandle( const Handle_:P_VkInstanceCreateInfo );
     public
       constructor Create; overload; virtual;
       constructor Create( const Parent_:TVkInstan_ ); overload; virtual;
       destructor Destroy; override;
       ///// プロパティ
       property Parent  :TVkInstan_             read    _Parent                ;
       property Kind    :VkStructureType        read GetKind    write SetKind  ;
       property Next    :Pointer                read GetNext    write SetNext  ;
       property Flags   :VkInstanceCreateFlags  read GetFlags   write SetFlags ;
       property Applic  :TVkAppInf_             read   _Applic                 ;
       property Layeres :TStringList            read   _Layeres                ;
       property Extenss :TStringList            read   _Extenss                ;
       property Handle  :P_VkInstanceCreateInfo read GetHandle  write SetHandle;
     end;

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TVkInstan

     TVkInstan<TVulkan_:class> = class( TListChildr<TVulkan_,TVkInstans<TVulkan_>> )
     private
       type TVkInstans_ = TVkInstans<TVulkan_>;
            TVkInstan_  = TVkInstan <TVulkan_>;
            TVkInsInf_  = TVkInsInf <TVulkan_>;
            TVkSurfacs_ = TVkSurfacs<TVkInstan_>;
            TVkDevices_ = TVkDevices<TVkInstan_>;
     protected
       _Inform  :TVkInsInf_;
       _Handle  :VkInstance;
       _Surfacs :TVkSurfacs_;
       _Devices :TVkDevices_;
       ///// アクセス
       function GetHandle :VkInstance;
       procedure SetHandle( const Handle_:VkInstance );
       ///// メソッド
       procedure CreateHandle;
       procedure DestroHandle;
     public
       constructor Create; overload; override;
       constructor Create( const Instans_:TVkInstans_ ); overload; override;
       constructor Create( const Vulkan_:TVulkan_ ); overload; virtual;
       destructor Destroy; override;
       ///// プロパティ
       property Vulkan  :TVulkan_    read GetOwnere                  ;
       property Instans :TVkInstans_ read GetParent                  ;
       property Inform  :TVkInsInf_  read   _Inform                  ;
       property Handle  :VkInstance  read GetHandle  write SetHandle ;
       property Surfacs :TVkSurfacs_ read   _Surfacs                 ;
       property Devices :TVkDevices_ read   _Devices write   _Devices;
     end;

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TVkInstans

     TVkInstans<TVulkan_:class> = class( TListParent<TVulkan_,TVkInstan<TVulkan_>> )
     private
       type TVkInstan_ = TVkInstan<TVulkan_>;
     protected
     public
       ///// プロパティ
       property Vulkan :TVulkan_ read GetOwnere;
       ///// メソッド
       function Add :TVkInstan_; overload;
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

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TVkAppInf

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

/////////////////////////////////////////////////////////////////////// アクセス

function TVkAppInf<TVulkan_>.GetKind :VkStructureType;
begin
     Result := _Inform.sType;
end;

procedure TVkAppInf<TVulkan_>.SetKind( const Kind_:VkStructureType );
begin
     _Inform.sType := Kind_;  Handle := nil;
end;

function TVkAppInf<TVulkan_>.GetNext :Pointer;
begin
     Result := _Inform.pNext;
end;

procedure TVkAppInf<TVulkan_>.SetNext( const Next_:Pointer );
begin
     _Inform.pNext := Next_;  Handle := nil;
end;

function TVkAppInf<TVulkan_>.GetAppName :String;
begin
     Result := String( _Inform.pApplicationName );
end;

procedure TVkAppInf<TVulkan_>.SetAppName( const AppName_:String );
begin
     _Inform.pApplicationName := PAnsiChar( AnsiString( AppName_ ) );  Handle := nil;
end;

function TVkAppInf<TVulkan_>.GetAppVers :UInt32;
begin
     Result := _Inform.applicationVersion;
end;

procedure TVkAppInf<TVulkan_>.SetAppVers( const AppVers_:UInt32 );
begin
     _Inform.applicationVersion := AppVers_;  Handle := nil;
end;

function TVkAppInf<TVulkan_>.GetEngName :String;
begin
     Result := String( _Inform.pEngineName );
end;

procedure TVkAppInf<TVulkan_>.SetEngName( const EngName_:String );
begin
     _Inform.pEngineName := PAnsiChar( AnsiString( EngName_ ) );  Handle := nil;
end;

function TVkAppInf<TVulkan_>.GetEngVers :UInt32;
begin
     Result := _Inform.engineVersion;
end;

procedure TVkAppInf<TVulkan_>.SetEngVers( const EngVers_:UInt32 );
begin
     _Inform.engineVersion := EngVers_;  Handle := nil;
end;

function TVkAppInf<TVulkan_>.GetApiVers :UInt32;
begin
     Result := _Inform.apiVersion;
end;

procedure TVkAppInf<TVulkan_>.SetApiVers( const ApiVersion_:UInt32 );
begin
     _Inform.apiVersion := ApiVersion_;  Handle := nil;
end;

//------------------------------------------------------------------------------

function TVkAppInf<TVulkan_>.GetHandle :P_VkApplicationInfo;
begin
     if not Assigned( _Handle ) then _Handle := @_Inform;

     Result := _Handle;
end;

procedure TVkAppInf<TVulkan_>.SetHandle( const Handle_:P_VkApplicationInfo );
begin
     _Handle := Handle_;

     if Assigned( _Parent ) then _Parent.Handle := nil;
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TVkAppInf<TVulkan_>.Create;
begin
     inherited;

     _Handle := nil;

     Kind    := VK_STRUCTURE_TYPE_APPLICATION_INFO;
     Next    := nil;
     AppName := 'Application';
     AppVers := 1;
     EngName := 'Engine';
     EngVers := 1;
     ApiVers := VK_API_VERSION_1_0;
end;

constructor TVkAppInf<TVulkan_>.Create( const Parent_:TVkInsInf_ );
begin
     Create;

     _Parent := Parent_;
end;

destructor TVkAppInf<TVulkan_>.Destroy;
begin
     Handle := nil;

     inherited;
end;

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TVkInsInf

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

/////////////////////////////////////////////////////////////////////// アクセス

function TVkInsInf<TVulkan_>.GetKind :VkStructureType;
begin
     Result := _Inform.sType;
end;

procedure TVkInsInf<TVulkan_>.SetKind( const Kind_:VkStructureType );
begin
     _Inform.sType := Kind_;  Handle := nil;
end;

function TVkInsInf<TVulkan_>.GetNext :Pointer;
begin
     Result := _Inform.pNext;
end;

procedure TVkInsInf<TVulkan_>.SetNext( const Next_:Pointer );
begin
     _Inform.pNext := Next_;  Handle := nil;
end;

function TVkInsInf<TVulkan_>.GetFlags :VkInstanceCreateFlags;
begin
     Result := _Inform.flags;
end;

procedure TVkInsInf<TVulkan_>.SetFlags( const Flags_:VkInstanceCreateFlags );
begin
     _Inform.flags := Flags_;  Handle := nil;
end;

//------------------------------------------------------------------------------

function TVkInsInf<TVulkan_>.GetHandle :P_VkInstanceCreateInfo;
var
   L, E :String;
   Ls, Es :TArray<PAnsiChar>;
begin
     if not Assigned( _Handle ) then
     begin
          for L in _Layeres do Ls := Ls + [ System.AnsiStrings.StrNew( PAnsiChar( AnsiString( L ) ) ) ];
          for E in _Extenss do Es := Es + [ System.AnsiStrings.StrNew( PAnsiChar( AnsiString( E ) ) ) ];

          with _Inform do
          begin
               enabledLayerCount     := Length( Ls );
             ppEnabledLayerNames     := @Ls[0];
               enabledExtensionCount := Length( Es );
             ppEnabledExtensionNames := @Es[0];
          end;

          _Handle := @_Inform;
     end;

     Result := _Handle;
end;

procedure TVkInsInf<TVulkan_>.SetHandle( const Handle_:P_VkInstanceCreateInfo );
begin
     _Handle := Handle_;

     if Assigned( _Parent ) then _Parent.Handle := nil;
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TVkInsInf<TVulkan_>.Create;
begin
     inherited;

     _Handle := nil;

     _Applic  := TVkAppInf_.Create( Self );
     _Layeres := TStringList.Create;
     _Extenss := TStringList.Create;

     Kind  := VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO;
     Next  := nil;
     Flags := 0;

     Extenss.Add( VK_KHR_SURFACE_EXTENSION_NAME       );
     Extenss.Add( VK_KHR_WIN32_SURFACE_EXTENSION_NAME );
end;

constructor TVkInsInf<TVulkan_>.Create( const Parent_:TVkInstan_ );
begin
     Create;

     _Parent := Parent_;
end;

destructor TVkInsInf<TVulkan_>.Destroy;
begin
     _Applic .Free;
     _Layeres.Free;
     _Extenss.Free;

      Handle := nil;

     inherited;
end;

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TVkInstan

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

/////////////////////////////////////////////////////////////////////// アクセス

function TVkInstan<TVulkan_>.GetHandle :VkInstance;
begin
     if not Assigned( _Handle ) then CreateHandle;

     Result := _Handle;
end;

procedure TVkInstan<TVulkan_>.SetHandle( const Handle_:VkInstance );
begin
     if Assigned( _Handle ) then DestroHandle;

     _Handle := Handle_;
end;

/////////////////////////////////////////////////////////////////////// メソッド

procedure TVkInstan<TVulkan_>.CreateHandle;
begin
     Assert( vkCreateInstance( _Inform.Handle, nil, @_Handle ) = VK_SUCCESS );
end;

procedure TVkInstan<TVulkan_>.DestroHandle;
begin
     vkDestroyInstance( _Handle, nil );
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TVkInstan<TVulkan_>.Create;
begin
     inherited;

     _Inform := TVkInsInf_.Create( Self );

     _Handle := nil;

     _Surfacs := TVkSurfacs_.Create( Self );
     _Devices := TVkDevices_.Create( Self );
end;

constructor TVkInstan<TVulkan_>.Create( const Instans_:TVkInstans_ );
begin
     inherited;

     _Devices.FindDevices;
end;

constructor TVkInstan<TVulkan_>.Create( const Vulkan_:TVulkan_ );
begin
     Create( TVkInstans_( TVulkan( Vulkan_ ).Instans ) );
end;

destructor TVkInstan<TVulkan_>.Destroy;
begin
     _Devices.Free;
     _Surfacs.Free;

      Handle := nil;

     _Inform.Free;

     inherited;
end;

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TVkInstans

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

/////////////////////////////////////////////////////////////////////// メソッド

function TVkInstans<TVulkan_>.Add :TVkInstan_;
begin
     Result := TVkInstan_.Create( Self );
end;

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【ルーチン】

//############################################################################## □

initialization //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ 初期化

finalization //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ 最終化

end. //######################################################################### ■