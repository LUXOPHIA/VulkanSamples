unit LUX.GPU.Vulkan.Window;

interface //#################################################################### ■

uses WinApi.Windows,
     vulkan_core, vulkan_win32,
     LUX.GPU.Vulkan.Surface;

type //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【型】

     TVkWindow<TVkInstan_:class> = class;

     //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【レコード】

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TVkWindow

     TVkWindow<TVkInstan_:class> = class
     private
       type TVkWindow_  = TVkWindow<TVkInstan_>;
            TVkSurface_ = TVkSurface<TVkWindow_>;
     protected
       _Instan  :TVkInstan_;
       _Width   :Integer;
       _Height  :Integer;
       _Proc    :TFNWndProc;
       _Surface :TVkSurface_;
       ///// メソッド
       procedure CreateHandle;
       procedure DestroHandle;
     public
       connection :HINST;   // hInstance - Windows Instance
       name       :String;  // Name to put on the window/icon
       window     :HWND;    // hWnd - window handle

       constructor Create( const Instan_:TVkInstan_; const Width_,Height_:Integer; const Proc_:TFNWndProc );
       procedure AfterConstruction; override;
       destructor Destroy; override;
       ///// プロパティ
       property Instan  :TVkInstan_ read _Instan                 ;
       property Width   :Integer      read _Width    write _Width  ;
       property Height  :Integer      read _Height   write _Height ;
       property Proc    :TFNWndProc   read _Proc     write _Proc   ;
       property Surface :TVkSurface_  read _Surface  write _Surface;
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

procedure TVkWindow<TVkInstan_>.CreateHandle;
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
                               LPCWSTR( name ),                                  // class name
                               LPCWSTR( name ),                                  // app name
                               WS_OVERLAPPEDWINDOW or WS_VISIBLE or WS_SYSMENU,  // window style
                               100, 100,                                         // x/y coords
                               wr.right - wr.left,                               // width
                               wr.bottom - wr.top,                               // height
                               0,                                                // handle to parent
                               0,                                                // handle to menu
                               connection,                                       // hInstance
                               nil );                                            // no extra parameters
     if window = 0 then
     begin
          // It didn't work, so try to give a useful error:
          Log.d( 'Cannot create a window in which to draw!' );
          RunError( 1 );
     end;
     SetWindowLongPtr( window, GWLP_USERDATA, LONG_PTR( @TVkInstan( _Instan ).Vulkan.Info ) );
end;

procedure TVkWindow<TVkInstan_>.DestroHandle;
begin
     DestroyWindow( window );
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TVkWindow<TVkInstan_>.Create( const Instan_:TVkInstan_; const Width_,Height_:Integer; const Proc_:TFNWndProc );
begin
     inherited Create;

     _Instan := Instan_;
     _Width  := Width_ ;
     _Height := Height_;
     _Proc   := Proc_  ;

     TVkInstan( _Instan ).Window := TVkWindow( Self );

     CreateHandle;
end;

procedure TVkWindow<TVkInstan_>.AfterConstruction;
begin
     inherited;

end;

destructor TVkWindow<TVkInstan_>.Destroy;
begin
     DestroHandle;

     inherited;
end;

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【ルーチン】

//############################################################################## □

initialization //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ 初期化

finalization //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ 最終化

end. //######################################################################### ■