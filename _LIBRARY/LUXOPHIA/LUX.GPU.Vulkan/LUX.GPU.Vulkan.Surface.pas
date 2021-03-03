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
       _Handle :VkSurfaceKHR;
       ///// メソッド
       procedure CreateHandle;
       procedure DestroHandle;
     public
       constructor Create( const Window_:TVkWindow_ );
       procedure AfterConstruction; override;
       destructor Destroy; override;
       ///// プロパティ
       property Window :TVkWindow_   read _Window;
       property Handle :VkSurfaceKHR read _Handle;
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
const
     (* Use this surface format if it's available.  This ensures that generated
      * images are similar on different devices and with different drivers.
      *)
     PREFERRED_SURFACE_FORMAT = VK_FORMAT_B8G8R8A8_UNORM;
var
   res              :VkResult;
   createInfo       :VkWin32SurfaceCreateInfoKHR;
   pSupportsPresent :TArray<VkBool32>;
   i                :UInt32;
   formatCount      :UInt32;
   surfFormats      :TArray<VkSurfaceFormatKHR>;
begin
     (* DEPENDS on init_connection() and init_window() *)

     // Construct the surface description:
     createInfo           := Default( VkWin32SurfaceCreateInfoKHR );
     createInfo.sType     := VK_STRUCTURE_TYPE_WIN32_SURFACE_CREATE_INFO_KHR;
     createInfo.pNext     := nil;
     createInfo.hinstance := TVkWindow( _Window ).connection;
     createInfo.hwnd      := TVkWindow( _Window ).window;
     res := vkCreateWin32SurfaceKHR( TVkWindow( _Window ).Device.Devices.Instance.Handle, @createInfo, nil, @_Handle );
     Assert( res = VK_SUCCESS );

     // Iterate over each queue to learn whether it supports presenting:
     SetLength( pSupportsPresent, TVkWindow( _Window ).Device.Devices.Instance.Vulkan.Info.queue_family_count );
     for i := 0 to TVkWindow( _Window ).Device.Devices.Instance.Vulkan.Info.queue_family_count-1
     do vkGetPhysicalDeviceSurfaceSupportKHR( TVkWindow( _Window ).Device.Devices.Instance.Devices[0].PhysHandle, i, _Handle, @pSupportsPresent[i] );

     // Search for a graphics and a present queue in the array of queue
     // families, try to find one that supports both
     TVkWindow( _Window ).Device.Devices.Instance.Vulkan.Info.graphics_queue_family_index := UINT32.MaxValue;
     TVkWindow( _Window ).Device.Devices.Instance.Vulkan.Info.present_queue_family_index  := UINT32.MaxValue;
     for i := 0 to TVkWindow( _Window ).Device.Devices.Instance.Vulkan.Info.queue_family_count-1 do
     begin
          if ( TVkWindow( _Window ).Device.Devices.Instance.Vulkan.Info.queue_props[i].queueFlags and Ord( VK_QUEUE_GRAPHICS_BIT ) ) <> 0 then
          begin
               if TVkWindow( _Window ).Device.Devices.Instance.Vulkan.Info.graphics_queue_family_index = UINT32.MaxValue then TVkWindow( _Window ).Device.Devices.Instance.Vulkan.Info.graphics_queue_family_index := i;

               if pSupportsPresent[i] = VK_TRUE then
               begin
                    TVkWindow( _Window ).Device.Devices.Instance.Vulkan.Info.graphics_queue_family_index := i;
                    TVkWindow( _Window ).Device.Devices.Instance.Vulkan.Info.present_queue_family_index  := i;
                    Break;
               end;
          end;
     end;

     if TVkWindow( _Window ).Device.Devices.Instance.Vulkan.Info.present_queue_family_index = UINT32.MaxValue then
     begin
          // If didn't find a queue that supports both graphics and present, then
          // find a separate present queue.
          for i := 0 to TVkWindow( _Window ).Device.Devices.Instance.Vulkan.Info.queue_family_count-1 do
          begin
               if pSupportsPresent[i] = VK_TRUE then
               begin
                    TVkWindow( _Window ).Device.Devices.Instance.Vulkan.Info.present_queue_family_index := i;
                    Break;
               end;
          end;
     end;
     pSupportsPresent := nil;

     // Generate error if could not find queues that support graphics
     // and present
     if ( TVkWindow( _Window ).Device.Devices.Instance.Vulkan.Info.graphics_queue_family_index = UINT32.MaxValue ) or ( TVkWindow( _Window ).Device.Devices.Instance.Vulkan.Info.present_queue_family_index = UINT32.MaxValue ) then
     begin
          Log.d( 'Could not find a queues for both graphics and present' );
          RunError( 256-1 );
     end;

     // Get the list of VkFormats that are supported:
     res := vkGetPhysicalDeviceSurfaceFormatsKHR( TVkWindow( _Window ).Device.PhysHandle, _Handle, @formatCount, nil );
     Assert( res = VK_SUCCESS );
     SetLength( surfFormats, formatCount );
     res := vkGetPhysicalDeviceSurfaceFormatsKHR( TVkWindow( _Window ).Device.PhysHandle, _Handle, @formatCount, @surfFormats[0] );
     Assert( res = VK_SUCCESS );

     // If the device supports our preferred surface format, use it.
     // Otherwise, use whatever the device's first reported surface
     // format is.
     Assert( formatCount >= 1 );
     TVkWindow( _Window ).Device.Devices.Instance.Vulkan.Info.format := surfFormats[0].format;
     for i := 0 to formatCount-1 do
     begin
          if surfFormats[i].format = PREFERRED_SURFACE_FORMAT then
          begin
               TVkWindow( _Window ).Device.Devices.Instance.Vulkan.Info.format := PREFERRED_SURFACE_FORMAT;
               break;
          end;
     end;

     surfFormats := nil;
end;

procedure TVkSurface<TVkWindow_>.DestroHandle;
begin
     vkDestroySurfaceKHR( TVkWindow( _Window ).Device.Devices.Instance.Handle, _Handle, nil );
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TVkSurface<TVkWindow_>.Create( const Window_:TVkWindow_ );
begin
     inherited Create;

     _Window := Window_;

     TVkWindow( _Window ).Surface := TVkSurface( Self );

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