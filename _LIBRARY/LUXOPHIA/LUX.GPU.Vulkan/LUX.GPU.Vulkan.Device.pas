unit LUX.GPU.Vulkan.Device;

interface //#################################################################### ■

uses System.Generics.Collections,
     vulkan_core, vulkan_win32,
     vulkan.util,
     LUX.GPU.Vulkan.Pipeline,
     LUX.GPU.Vulkan.Surface,
     LUX.GPU.Vulkan.Buffer;

type //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【型】

     TVkDevices<TVkInstance_:class> = class;
     TVkDevice<TVkDevices_:class>   = class;

     //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【レコード】

     //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【クラス】

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TVkDevices

     TVkDevices<TVkInstance_:class> = class( TObjectList<TVkDevice<TVkDevices<TVkInstance_>>> )
     private
       type TVkDevices_ = TVkDevices<TVkInstance_>;
            TVkDevice_  = TVkDevice<TVkDevices_>;
     protected
       _Instance :TVkInstance_;
       ///// アクセス
       ///// メソッド
       procedure GetDevices;
     public
       constructor Create( const Instance_:TVkInstance_ );
       procedure AfterConstruction; override;
       destructor Destroy; override;
       ///// プロパティ
       property Instance :TVkInstance_ read _Instance;
       ///// メソッド
     end;

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TVkDevice

     TVkDevice<TVkDevices_:class> = class
     private
       type TVkDevice_ = TVkDevice<TVkDevices_>;
            TVkBuffer_ = TVkBuffer<TVkDevice_>;
     protected
       _Devices              :TVkDevices_;
       _PhysHandle           :VkPhysicalDevice;
       _Props                :VkPhysicalDeviceProperties;
       _Memorys              :VkPhysicalDeviceMemoryProperties;
       _Handle               :VkDevice;
       _Extensions           :TArray<PAnsiChar>;
       _QueueFamilysN        :UInt32;
       _QueueFamilys         :TArray<VkQueueFamilyProperties>;
       _GraphicsQueueFamilyI :UInt32;
       _PresentQueueFamilyI  :UInt32;
       _Format               :VkFormat;
       _Buffers              :TVkBuffer_;
       ///// アクセス
       function GetQueueFamilys( const I_:Integer ) :VkQueueFamilyProperties;
       ///// メソッド
       function init_device_extension_properties( var L_:T_layer_properties ) :VkResult;
       procedure InitLayers;
       procedure FindQueueFamilys;
       procedure FindQueueFamilyI; overload;
       procedure FindQueueFamilyI( const Surface_:VkSurfaceKHR ); overload;
       procedure FindFormat( const Surface_:VkSurfaceKHR );
       procedure CreateHandle;
       procedure DestroHandle;
       procedure init_device_queue;
     public
       constructor Create( const Devices_:TVkDevices_; const Handle_:VkPhysicalDevice );
       procedure AfterConstruction; override;
       destructor Destroy; override;
       ///// プロパティ
       property Devices                          :TVkDevices_                      read   _Devices             ;
       property PhysHandle                       :VkPhysicalDevice                 read   _PhysHandle          ;
       property Props                            :VkPhysicalDeviceProperties       read   _Props               ;
       property Memorys                          :VkPhysicalDeviceMemoryProperties read   _Memorys             ;
       property Handle                           :VkDevice                         read   _Handle              ;
       property Extensions                       :TArray<PAnsiChar>                read   _Extensions          ;
       property QueueFamilysN                    :UInt32                           read   _QueueFamilysN       ;
       property QueueFamilys[ const I_:Integer ] :VkQueueFamilyProperties          read GetQueueFamilys        ;
       property GraphicsQueueFamilyI             :UInt32                           read   _GraphicsQueueFamilyI;
       property PresentQueueFamilyI              :UInt32                           read   _PresentQueueFamilyI ;
       property Format                           :VkFormat                         read   _Format              ;
       property Buffers                          :TVkBuffer_                       read   _Buffers              write _Buffers;
       ///// メソッド
       function memory_type_from_properties( typeBits:UInt32; requirements_mask:VkFlags; var typeIndex:UInt32 ) :Boolean;
     end;

//const //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【定数】

//var //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【変数】

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【ルーチン】

implementation //############################################################### ■

uses System.SysUtils, System.Classes,
     FMX.Types,
     LUX.GPU.Vulkan;

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【レコード】

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【クラス】

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TVkShader

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

/////////////////////////////////////////////////////////////////////// アクセス

/////////////////////////////////////////////////////////////////////// メソッド

procedure TVkDevices<TVkInstance_>.GetDevices;
var
   DsN :UInt32;
   Ds :TArray<VkPhysicalDevice>;
   D :VkPhysicalDevice;
begin
     Assert( ( vkEnumeratePhysicalDevices( TVkInstance( _Instance ).Handle, @DsN, nil ) = VK_SUCCESS ) and ( DsN > 0 ) );

     SetLength( Ds, DsN );

     Assert( ( vkEnumeratePhysicalDevices( TVkInstance( _Instance ).Handle, @DsN, @Ds[0] ) = VK_SUCCESS ) and ( DsN > 0 ) );

     for D in Ds do Add( TVkDevice_.Create( Self, D ) );
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TVkDevices<TVkInstance_>.Create( const Instance_:TVkInstance_ );
begin
     inherited Create;

     _Instance := Instance_;

     TVkInstance( _Instance ).Devices := TVkDevices( Self );
end;

procedure TVkDevices<TVkInstance_>.AfterConstruction;
begin
     inherited;

     GetDevices;
end;

destructor TVkDevices<TVkInstance_>.Destroy;
begin

     inherited;
end;

/////////////////////////////////////////////////////////////////////// メソッド

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TVkDevice

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

/////////////////////////////////////////////////////////////////////// アクセス

function TVkDevice<TVkDevices_>.GetQueueFamilys( const I_:Integer ) :VkQueueFamilyProperties;
begin
     Result := _QueueFamilys[ I_ ];
end;

/////////////////////////////////////////////////////////////////////// メソッド

function TVkDevice<TVkDevices_>.init_device_extension_properties( var L_:T_layer_properties ) :VkResult;
var
   EsN   :UInt32;
   Lname :PAnsiChar;
begin
     Lname := L_.properties.layerName;

     repeat
           Result := vkEnumerateDeviceExtensionProperties( PhysHandle, Lname, @EsN, nil );
           if Result <> VK_SUCCESS then Exit;

           if EsN = 0 then Exit( VK_SUCCESS );

           SetLength( L_.device_extensions, EsN );
           Result := vkEnumerateDeviceExtensionProperties( PhysHandle, Lname, @EsN, @L_.device_extensions[0] );

     until Result <> VK_INCOMPLETE;
end;

procedure TVkDevice<TVkDevices_>.InitLayers;
var
   I :Integer;
begin
     (* query device extensions for enabled layers *)
     for I := 0 to Length( TVkDevices( Devices ).Instance.Vulkan.Layers )-1
     do init_device_extension_properties( TVkDevices( Devices ).Instance.Vulkan.Layers[I] );
end;

procedure TVkDevice<TVkDevices_>.FindQueueFamilys;
begin
     vkGetPhysicalDeviceQueueFamilyProperties( PhysHandle, @_QueueFamilysN, nil );
     Assert( _QueueFamilysN > 1 );

     SetLength( _QueueFamilys, _QueueFamilysN );
     vkGetPhysicalDeviceQueueFamilyProperties( PhysHandle, @_QueueFamilysN, @_QueueFamilys[0] );
     Assert( _QueueFamilysN > 1 );
end;

procedure TVkDevice<TVkDevices_>.FindQueueFamilyI;
var
   I :UInt32;
begin
     for i := 0 to _QueueFamilysN-1 do
     begin
          if ( _QueueFamilys[I].queueFlags and Ord( VK_QUEUE_GRAPHICS_BIT ) ) <> 0 then
          begin
               _GraphicsQueueFamilyI := I;
               _PresentQueueFamilyI  := I;

               Break;
          end;
     end;
end;

procedure TVkDevice<TVkDevices_>.FindQueueFamilyI( const Surface_:VkSurfaceKHR );
var
   Fs :TArray<VkBool32>;
   I  :UInt32;
begin
     // Iterate over each queue to learn whether it supports presenting:
     SetLength( Fs, _QueueFamilysN );
     for I := 0 to _QueueFamilysN-1 do vkGetPhysicalDeviceSurfaceSupportKHR( _PhysHandle, I, Surface_, @Fs[I] );

     // Search for a graphics and a present queue in the array of queue
     // families, try to find one that supports both
     _GraphicsQueueFamilyI := UINT32.MaxValue;
     _PresentQueueFamilyI  := UINT32.MaxValue;
     for I := 0 to _QueueFamilysN-1 do
     begin
          if ( _QueueFamilys[I].queueFlags and Ord( VK_QUEUE_GRAPHICS_BIT ) ) <> 0 then
          begin
               if _GraphicsQueueFamilyI = UINT32.MaxValue then _GraphicsQueueFamilyI := i;

               if Fs[I] = VK_TRUE then
               begin
                    _GraphicsQueueFamilyI := i;
                    _PresentQueueFamilyI  := i;
                    Break;
               end;
          end;
     end;

     if _PresentQueueFamilyI = UINT32.MaxValue then
     begin
          // If didn't find a queue that supports both graphics and present, then
          // find a separate present queue.
          for I := 0 to _QueueFamilysN-1 do
          begin
               if Fs[I] = VK_TRUE then
               begin
                    _PresentQueueFamilyI := i;
                    Break;
               end;
          end;
     end;

     // Generate error if could not find queues that support graphics
     // and present
     if ( _GraphicsQueueFamilyI = UINT32.MaxValue ) or ( _PresentQueueFamilyI = UINT32.MaxValue ) then
     begin
          Log.d( 'Could not find a queues for both graphics and present' );
          RunError( 256-1 );
     end;
end;

procedure TVkDevice<TVkDevices_>.FindFormat( const Surface_:VkSurfaceKHR );
const
     (* Use this surface format if it's available.  This ensures that generated
      * images are similar on different devices and with different drivers.
      *)
     PREFERRED_SURFACE_FORMAT = VK_FORMAT_B8G8R8A8_UNORM;
var
   FsN :UInt32;
   Fs  :TArray<VkSurfaceFormatKHR>;
   I   :UInt32;
begin
     // Get the list of VkFormats that are supported:
     Assert( vkGetPhysicalDeviceSurfaceFormatsKHR( _PhysHandle, Surface_, @FsN, nil ) = VK_SUCCESS );
     Assert( FsN > 1 );
     SetLength( Fs, FsN );
     Assert( vkGetPhysicalDeviceSurfaceFormatsKHR( _PhysHandle, Surface_, @FsN, @Fs[0] ) = VK_SUCCESS );
     Assert( FsN > 1 );

     // If the device supports our preferred surface format, use it.
     // Otherwise, use whatever the device's first reported surface
     // format is.
     _Format := Fs[0].format;
     for I := 0 to FsN-1 do
     begin
          if Fs[I].format = PREFERRED_SURFACE_FORMAT then
          begin
               _Format := PREFERRED_SURFACE_FORMAT;  Break;
          end;
     end;
end;

procedure TVkDevice<TVkDevices_>.CreateHandle;
var
   queue_priorities :array [ 0..1-1 ] of Single;
   queue_info       :VkDeviceQueueCreateInfo;
   device_info      :VkDeviceCreateInfo;
begin
     queue_priorities[0]         := 0;
     queue_info                  := Default( VkDeviceQueueCreateInfo );
     queue_info.sType            := VK_STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO;
     queue_info.pNext            := nil;
     queue_info.queueCount       := 1;
     queue_info.pQueuePriorities := @queue_priorities[0];
     queue_info.queueFamilyIndex := _GraphicsQueueFamilyI;
     device_info                              := Default( VkDeviceCreateInfo );
     device_info.sType                        := VK_STRUCTURE_TYPE_DEVICE_CREATE_INFO;
     device_info.pNext                        := nil;
     device_info.queueCreateInfoCount         := 1;
     device_info.pQueueCreateInfos            := @queue_info;
     device_info.enabledExtensionCount        := Length( _Extensions );
     if device_info.enabledExtensionCount > 0
     then device_info.ppEnabledExtensionNames := @_Extensions[0]
     else device_info.ppEnabledExtensionNames := nil;
     device_info.pEnabledFeatures             := nil;
     Assert( vkCreateDevice( PhysHandle, @device_info, nil, @_Handle ) = VK_SUCCESS );
end;

procedure TVkDevice<TVkDevices_>.DestroHandle;
begin
     vkDeviceWaitIdle( _Handle );
     vkDestroyDevice( _Handle, nil );
end;

procedure TVkDevice<TVkDevices_>.init_device_queue;
begin
     (* DEPENDS on init_swapchain_extension() *)

     vkGetDeviceQueue( _Handle, _GraphicsQueueFamilyI, 0, @TVkDevices( _Devices ).Instance.Vulkan.Info.graphics_queue );
     if _GraphicsQueueFamilyI = _PresentQueueFamilyI
     then TVkDevices( _Devices ).Instance.Vulkan.Info.present_queue := TVkDevices( _Devices ).Instance.Vulkan.Info.graphics_queue
     else vkGetDeviceQueue( _Handle, _PresentQueueFamilyI, 0, @TVkDevices( _Devices ).Instance.Vulkan.Info.present_queue );
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TVkDevice<TVkDevices_>.Create( const Devices_:TVkDevices_; const Handle_:VkPhysicalDevice );
begin
     inherited Create;

     _Devices    := Devices_;
     _PhysHandle := Handle_;

     vkGetPhysicalDeviceProperties( PhysHandle, @_Props );

     vkGetPhysicalDeviceMemoryProperties( PhysHandle, @Memorys );

     InitLayers;

     FindQueueFamilys;
     FindQueueFamilyI( TVkDevices( _Devices ).Instance.Window.Surface.Handle );

     FindFormat( TVkDevices( _Devices ).Instance.Window.Surface.Handle );

     _Extensions := _Extensions + [ VK_KHR_SWAPCHAIN_EXTENSION_NAME ];

     CreateHandle;

     init_device_queue;
end;

procedure TVkDevice<TVkDevices_>.AfterConstruction;
begin
     inherited;

end;

destructor TVkDevice<TVkDevices_>.Destroy;
begin
     DestroHandle;

     inherited;
end;

/////////////////////////////////////////////////////////////////////// メソッド

function TVkDevice<TVkDevices_>.memory_type_from_properties( typeBits:UInt32; requirements_mask:VkFlags; var typeIndex:UInt32 ) :Boolean;
var
   I :UInt32;
begin
     // Search memtypes to find first index with those properties
     for I := 0 to Memorys.memoryTypeCount-1 do
     begin
          if ( typeBits and 1 ) = 1 then
          begin
               // Type is available, does it match user properties?
               if Memorys.memoryTypes[I].propertyFlags and requirements_mask = requirements_mask then
               begin
                    typeIndex := i;
                    Exit( True );
               end;
          end;
          typeBits := typeBits shr 1;
     end;
     // No memory types matched, return failure
     Result := False;
end;

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【ルーチン】

//############################################################################## □

initialization //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ 初期化

finalization //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ 最終化

end. //######################################################################### ■