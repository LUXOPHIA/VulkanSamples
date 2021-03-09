unit LUX.GPU.Vulkan.Surfac;

interface //#################################################################### ■

uses System.Generics.Collections,
     WinApi.Windows,
     vulkan_core, vulkan_win32;

type //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【型】

     TVkSurfacs<TVkInstan_:class>  = class;
       TVkSurfac<TVkInstan_:class> = class;

     //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【レコード】

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TVkSurfac

     TVkSurfac<TVkInstan_:class> = class
     private
       type TVkSurfac_ = TVkSurfac<TVkInstan_>;
     protected
       _Instan  :TVkInstan_;
       _Inform  :VkWin32SurfaceCreateInfoKHR;
       _PxSizeX :Integer;
       _PxSizeY :Integer;
       _Handle  :VkSurfaceKHR;
       ///// アクセス
       function GetWindow :HWND;
       procedure SetWindow( const Window_:HWND );
       function GetPxSizeX :Integer;
       function GetPxSizeY :Integer;
       function GetHandle :VkSurfaceKHR;
       procedure SetHandle( const Handle_:VkSurfaceKHR );
       ///// メソッド
       procedure CreateHandle;
       procedure DestroHandle;
     public
       constructor Create; overload;
       constructor Create( const Instan_:TVkInstan_ ); overload;
       constructor Create( const Instan_:TVkInstan_; const Window_:HWND ); overload;
       destructor Destroy; override;
       ///// プロパティ
       property Instan  :TVkInstan_                  read   _Instan ;
       property Window  :HWND                        read GetWindow  write SetWindow;
       property PxSizeX :Integer                     read GetPxSizeX;
       property PxSizeY :Integer                     read GetPxSizeY;
       property Inform  :VkWin32SurfaceCreateInfoKHR read   _Inform ;
       property Handle  :VkSurfaceKHR                read GetHandle  write SetHandle;
     end;

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TVkSurfacs

     TVkSurfacs<TVkInstan_:class> = class( TObjectList<TVkSurfac<TVkInstan_>> )
     private
       type TVkSurfac_ = TVkSurfac<TVkInstan_>;
     protected
       _Instan :TVkInstan_;
     public
       constructor Create( const Instan_:TVkInstan_ );
       destructor Destroy; override;
       ///// プロパティ
       property Instan :TVkInstan_ read _Instan;
       ///// メソッド
       function Add( const HWND_:HWND ) :TVkSurfac_; overload;
     end;

     //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【クラス】

//const //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【定数】

//var //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【変数】

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【ルーチン】

implementation //############################################################### ■

uses System.SysUtils,
     FMX.Types,
     LUX.GPU.Vulkan;

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【レコード】

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【クラス】

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TVkSurfac

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

/////////////////////////////////////////////////////////////////////// アクセス

function TVkSurfac<TVkInstan_>.GetWindow :HWND;
begin
     Result := _Inform.hwnd;
end;

procedure TVkSurfac<TVkInstan_>.SetWindow( const Window_:HWND );
var
   R :TRect;
begin
     _Inform.hwnd := Window_;

     GetClientRect( Window, R );

     _PxSizeX := R.Width ;
     _PxSizeY := R.Height;

     _Inform.hinstance := GetWindowLong( Window, GWL_HINSTANCE );

     Handle := 0;
end;

function TVkSurfac<TVkInstan_>.GetPxSizeX :Integer;
begin
     Result := _PxSizeX;
end;

function TVkSurfac<TVkInstan_>.GetPxSizeY :Integer;
begin
     Result := _PxSizeY;
end;

function TVkSurfac<TVkInstan_>.GetHandle :VkSurfaceKHR;
begin
     if _Handle = 0 then CreateHandle;

     Result := _Handle;
end;

procedure TVkSurfac<TVkInstan_>.SetHandle( const Handle_:VkSurfaceKHR );
begin
     if _Handle <> 0 then DestroHandle;

     _Handle := Handle_;
end;

/////////////////////////////////////////////////////////////////////// メソッド

procedure TVkSurfac<TVkInstan_>.CreateHandle;
begin
     Assert( vkCreateWin32SurfaceKHR( TVkInstan( _Instan ).Handle, @_Inform, nil, @_Handle ) = VK_SUCCESS );
end;

procedure TVkSurfac<TVkInstan_>.DestroHandle;
begin
     vkDestroySurfaceKHR( TVkInstan( _Instan ).Handle, _Handle, nil );
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TVkSurfac<TVkInstan_>.Create;
begin
     inherited;

     _Handle := 0;

     with _Inform do
     begin
          sType     := VK_STRUCTURE_TYPE_WIN32_SURFACE_CREATE_INFO_KHR;
          pNext     := nil;
          flags     := 0;
       // hinstance
       // hwnd
     end;
end;

constructor TVkSurfac<TVkInstan_>.Create( const Instan_:TVkInstan_ );
begin
     Create;

     _Instan := Instan_;

     TVkInstan( _Instan ).Surfacs.Add( TVkSurfac( Self ) );
end;

constructor TVkSurfac<TVkInstan_>.Create( const Instan_:TVkInstan_; const Window_:HWND );
begin
     Create( Instan_ );

     Window := Window_;
end;

destructor TVkSurfac<TVkInstan_>.Destroy;
begin
     Handle := 0;

     inherited;
end;

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TVkSurfacs

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TVkSurfacs<TVkInstan_>.Create( const Instan_:TVkInstan_ );
begin
     inherited Create;

     _Instan := Instan_;
end;

destructor TVkSurfacs<TVkInstan_>.Destroy;
begin

     inherited;
end;

/////////////////////////////////////////////////////////////////////// メソッド

function TVkSurfacs<TVkInstan_>.Add( const HWND_:HWND ) :TVkSurfac_;
begin
     Result := TVkSurfac_.Create( _Instan, HWND_ );
end;

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【ルーチン】

//############################################################################## □

initialization //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ 初期化

finalization //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ 最終化

end. //######################################################################### ■