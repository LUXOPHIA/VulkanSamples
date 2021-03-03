unit LUX.GPU.Vulkan.Window;

interface //#################################################################### ■

uses WinApi.Windows,
     vulkan_core, vulkan_win32;

type //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【型】

     //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【レコード】

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TVkWindow

     TVkWindow<TVkDevice_:class> = class
     private
     protected
       _Device :TVkDevice_;
       _Width  :Integer;
       _Height :Integer;
       _Proc   :TFNWndProc;
       ///// メソッド
       procedure init_window;
       procedure destroy_window;
     public
       connection :HINST;   // hInstance - Windows Instance
       name       :String;  // Name to put on the window/icon
       window     :HWND;    // hWnd - window handle

       constructor Create( const Device_:TVkDevice_; const Width_,Height_:Integer; const Proc_:TFNWndProc );
       procedure AfterConstruction; override;
       destructor Destroy; override;
       ///// プロパティ
       property Device :TVkDevice_ read _Device;
       property Width  :Integer    read _Width  write _Width ;
       property Height :Integer    read _Height write _Height;
     end;

     //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【クラス】

//const //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【定数】

//var //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【変数】

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【ルーチン】

implementation //############################################################### ■

uses FMX.Types,
     LUX.GPU.Vulkan;

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【レコード】

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【クラス】

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TVkWindow

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

procedure TVkWindow<TVkDevice_>.init_window;
var
   win_class :WNDCLASSEX;
   wr        :TRect;
begin
     Assert( _Width  > 0 );
     Assert( _Height > 0 );

     connection := GetModuleHandle( nil );
     name       := 'Sample';

     // Initialize the window class structure:
     win_class.cbSize        := SizeOf( WNDCLASSEX );
     win_class.style         := CS_HREDRAW or CS_VREDRAW;
     win_class.lpfnWndProc   := _Proc;
     win_class.cbClsExtra    := 0;
     win_class.cbWndExtra    := 0;
     win_class.hInstance     := connection;  // hInstance
     win_class.hIcon         := LoadIcon( 0, IDI_APPLICATION );
     win_class.hCursor       := LoadCursor( 0, IDC_ARROW );
     win_class.hbrBackground := HBRUSH( GetStockObject( WHITE_BRUSH ) );
     win_class.lpszMenuName  := nil;
     win_class.lpszClassName := LPCWSTR( name );
     win_class.hIconSm       := LoadIcon( 0, IDI_WINLOGO );
     // Register window class:
     if RegisterClassEx( win_class ) = 0 then
     begin
          // It didn't work, so try to give a useful error:
          Log.d( 'Unexpected error trying to start the application!' );
          RunError( 1 );
     end;
     // Create window with the registered class:
     wr := TRect.Create( 0, 0, _Width, _Height );
     AdjustWindowRect( wr, WS_OVERLAPPEDWINDOW, False );
     window := CreateWindowEx( 0,
                               LPCWSTR( name ),              // class name
                               LPCWSTR( name ),              // app name
                               WS_OVERLAPPEDWINDOW or WS_VISIBLE or WS_SYSMENU,  // window style
                               100, 100,                                         // x/y coords
                               wr.right - wr.left,                               // width
                               wr.bottom - wr.top,                               // height
                               0,                                                // handle to parent
                               0,                                                // handle to menu
                               connection,                                 // hInstance
                               nil );                                            // no extra parameters
     if window = 0 then
     begin
          // It didn't work, so try to give a useful error:
          Log.d( 'Cannot create a window in which to draw!' );
          RunError( 1 );
     end;
     SetWindowLongPtr( window, GWLP_USERDATA, LONG_PTR( @TVkDevice( _Device ).Devices.Instance.Vulkan.Info ) );
end;

procedure TVkWindow<TVkDevice_>.destroy_window;
begin
     vkDestroySurfaceKHR( TVkDevice( Device ).Devices.Instance.Handle, TVkDevice( Device ).Devices.Instance.Vulkan.Info.surface, nil );
     DestroyWindow( window );
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TVkWindow<TVkDevice_>.Create( const Device_:TVkDevice_; const Width_,Height_:Integer; const Proc_:TFNWndProc );
begin
     inherited Create;

     _Device := Device_;
     _Width  := Width_ ;
     _Height := Height_;
     _Proc   := Proc_;

     TVkDevice( _Device ).Window := TVkWindow( Self );

     init_window;
end;

procedure TVkWindow<TVkDevice_>.AfterConstruction;
begin
     inherited;

end;

destructor TVkWindow<TVkDevice_>.Destroy;
begin
     destroy_window;

     inherited;
end;

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【ルーチン】

//############################################################################## □

initialization //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ 初期化

finalization //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ 最終化

end. //######################################################################### ■