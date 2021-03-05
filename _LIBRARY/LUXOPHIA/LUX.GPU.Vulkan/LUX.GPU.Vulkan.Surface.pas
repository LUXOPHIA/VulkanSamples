unit LUX.GPU.Vulkan.Surface;

interface //#################################################################### ■

uses WinApi.Windows,
     vulkan_core, vulkan_win32;

type //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【型】

     //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【レコード】

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TVkSurface

     TVkSurface<TVkWindow_:class> = class
     private
       type TVkSurface_ = TVkSurface<TVkWindow_>;
     protected
       _Window :TVkWindow_;
       _Inform :VkWin32SurfaceCreateInfoKHR;
       _Handle :VkSurfaceKHR;
       ///// メソッド
       procedure CreateHandle;
       procedure DestroHandle;
     public
       constructor Create( const Window_:TVkWindow_ );
       procedure AfterConstruction; override;
       destructor Destroy; override;
       ///// プロパティ
       property Window :TVkWindow_                  read _Window;
       property Inform :VkWin32SurfaceCreateInfoKHR read _Inform;
       property Handle :VkSurfaceKHR                read _Handle;
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

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TVkSurface

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

procedure TVkSurface<TVkWindow_>.CreateHandle;
begin
     Assert( vkCreateWin32SurfaceKHR( TVkWindow( _Window ).Instance.Handle, @_Inform, nil, @_Handle ) = VK_SUCCESS );
end;

procedure TVkSurface<TVkWindow_>.DestroHandle;
begin
     vkDestroySurfaceKHR( TVkWindow( _Window ).Instance.Handle, _Handle, nil );
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TVkSurface<TVkWindow_>.Create( const Window_:TVkWindow_ );
begin
     inherited Create;

     _Window := Window_;

     TVkWindow( _Window ).Surface := TVkSurface( Self );

     with _Inform do
     begin
          sType     := VK_STRUCTURE_TYPE_WIN32_SURFACE_CREATE_INFO_KHR;
          pNext     := nil;
          flags     := 0;
          hinstance := TVkWindow( _Window ).connection;
          hwnd      := TVkWindow( _Window ).window;
     end;

     CreateHandle;
end;

procedure TVkSurface<TVkWindow_>.AfterConstruction;
begin
     inherited;

end;

destructor TVkSurface<TVkWindow_>.Destroy;
begin
     DestroHandle;

     inherited;
end;

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【ルーチン】

//############################################################################## □

initialization //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ 初期化

finalization //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ 最終化

end. //######################################################################### ■