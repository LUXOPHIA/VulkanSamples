unit LUX.GPU.Vulkan.Device;

interface //#################################################################### ■

uses System.Generics.Collections,
     vulkan_core, vulkan_win32,
     vulkan.util,
     LUX.GPU.Vulkan.Pipeline,
     LUX.GPU.Vulkan.Surface,
     LUX.GPU.Vulkan.Buffer,
     LUX.GPU.Vulkan.Command,
     LUX.GPU.Vulkan.Swapchain;

type //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【型】

     TVkDevices<TVkInstance_:class>    = class;
       TVkDevice<TVkDevices_:class>    = class;
         TVkDevLays<TVkDevice_:class>  = class;
           TVkDevLay<TVkDevice_:class> = class;
             TVkDevExts                = TArray<VkExtensionProperties>;

     //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【レコード】

     //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【クラス】

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TVkDevLay

     TVkDevLay<TVkDevice_:class> = class
     private
       type TVkDevLays_ = TVkDevLays<TVkDevice_>;
     protected
       _Parent  :TVkDevLays_;
       _LayereI :Integer;
       _Extenss :TVkDevExts;
       ///// メソッド
       function GetExtenss :VkResult;
     public
       constructor Create( const Parent_:TVkDevLays_; const LayereI_:Integer );
       destructor Destroy; override;
       ///// プロパティ
       property Parent  :TVkDevLays_ read _Parent ;
       property Extenss :TVkDevExts  read _Extenss;
     end;

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TVkDevLays

     TVkDevLays<TVkDevice_:class> = class( TObjectList<TVkDevLay<TVkDevice_>> )
     private
       type TVkDevLay_ = TVkDevLay<TVkDevice_>;
     protected
       _Parent :TVkDevice_;
       ///// メソッド
       procedure MakeLayeres;
     public
       constructor Create( const Parent_:TVkDevice_ );
       destructor Destroy; override;
       ///// プロパティ
       property Parent :TVkDevice_ read _Parent;
     end;

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
       type TVkDevice_      = TVkDevice<TVkDevices_>;
            TVkDevLays_     = TVkDevLays<TVkDevice_>;
            TVkBuffer_      = TVkBuffer<TVkDevice_>;
            TVkCommandPool_ = TVkCommandPool<TVkDevice_>;
            TVkSwapchain_   = TVkSwapchain<TVkDevice_>;
     protected
       _Devices    :TVkDevices_;
       _Physic     :VkPhysicalDevice;
       _Props      :VkPhysicalDeviceProperties;
       _Memorys    :VkPhysicalDeviceMemoryProperties;

       _Layeres    :TVkDevLays_;

       _Handle     :VkDevice;
       _Extensions :TArray<PAnsiChar>;
       _QueFamsN   :UInt32;
       _QueFams    :TArray<VkQueueFamilyProperties>;
       _QueFamG    :UInt32;
       _QueFamP    :UInt32;
       _Format     :VkFormat;
       _Buffers    :TVkBuffer_;
       _ComPool    :TVkCommandPool_;
       _Swapchains :TVkSwapchain_;
       _QueuerG    :VkQueue;
       _QueuerP    :VkQueue;
       ///// アクセス
       function GetQueFams( const I_:Integer ) :VkQueueFamilyProperties;
       ///// メソッド
       procedure FindQueFams;
       procedure FindQueFamI; overload;
       procedure FindQueFamI( const Surface_:VkSurfaceKHR ); overload;
       procedure FindFormat( const Surface_:VkSurfaceKHR );
       procedure CreateHandle;
       procedure DestroHandle;
       procedure init_device_queue;
     public
       constructor Create( const Devices_:TVkDevices_; const Physic_:VkPhysicalDevice );
       procedure AfterConstruction; override;
       destructor Destroy; override;
       ///// プロパティ
       property Devices                     :TVkDevices_                      read   _Devices                     ;
       property Physic                      :VkPhysicalDevice                 read   _Physic                      ;
       property Props                       :VkPhysicalDeviceProperties       read   _Props                       ;
       property Memorys                     :VkPhysicalDeviceMemoryProperties read   _Memorys                     ;

       property Layeres                     :TVkDevLays_                      read   _Layeres    write _Layeres   ;

       property Handle                      :VkDevice                         read   _Handle                      ;
       property Extensions                  :TArray<PAnsiChar>                read   _Extensions                  ;
       property QueFamsN                    :UInt32                           read   _QueFamsN                    ;
       property QueFams[ const I_:Integer ] :VkQueueFamilyProperties          read GetQueFams                     ;
       property QueFamG                     :UInt32                           read   _QueFamG                     ;
       property QueFamP                     :UInt32                           read   _QueFamP                     ;
       property Format                      :VkFormat                         read   _Format                      ;
       property Buffers                     :TVkBuffer_                       read   _Buffers    write _Buffers   ;
       property ComPool                     :TVkCommandPool_                  read   _ComPool    write _ComPool   ;
       property Swapchains                  :TVkSwapchain_                    read   _Swapchains write _Swapchains;
       property QueuerG                     :VkQueue                          read   _QueuerG                     ;
       property QueuerP                     :VkQueue                          read   _QueuerP                     ;
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

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TVkDevLay

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

function TVkDevLay<TVkDevice_>.GetExtenss :VkResult;
var
   D :TVkDevice;
   C :PAnsiChar;
   EsN :UInt32;
begin
     D := TVkDevLays( _Parent ).Parent;
     C := D.Devices.Instance.Vulkan.Layeres[ _LayereI ].Inform.layerName;

     repeat
           Result := vkEnumerateDeviceExtensionProperties( D.Physic, C, @EsN, nil );
           if Result <> VK_SUCCESS then Exit;

           if EsN = 0 then Exit( VK_SUCCESS );

           SetLength( _Extenss, EsN );
           Result := vkEnumerateDeviceExtensionProperties( D.Physic, C, @EsN, @_Extenss[0] );

     until Result <> VK_INCOMPLETE;
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TVkDevLay<TVkDevice_>.Create( const Parent_:TVkDevLays_; const LayereI_:Integer );
begin
     inherited Create;

     _Parent  := Parent_ ;
     _LayereI := LayereI_;

     TVkDevLays_( _Parent ).Add( Self );

     GetExtenss;
end;

destructor TVkDevLay<TVkDevice_>.Destroy;
begin

     inherited;
end;

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TVkDevLays

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

procedure TVkDevLays<TVkDevice_>.MakeLayeres;
var
   Ls :TVkLayeres;
   I :Integer;
begin
     Ls := TVkDevice( _Parent ).Devices.Instance.Vulkan.Layeres;

     (* query device extensions for enabled layers *)
     for I := 0 to Ls.Count-1 do TVkDevLay_.Create( Self, I );
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TVkDevLays<TVkDevice_>.Create( const Parent_:TVkDevice_ );
begin
     inherited Create;

     _Parent := Parent_;

     TVkDevice( _Parent ).Layeres := TVkDevLays( Self );

     MakeLayeres;
end;

destructor TVkDevLays<TVkDevice_>.Destroy;
begin

     inherited;
end;

/////////////////////////////////////////////////////////////////////// メソッド

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

function TVkDevice<TVkDevices_>.GetQueFams( const I_:Integer ) :VkQueueFamilyProperties;
begin
     Result := _QueFams[ I_ ];
end;

/////////////////////////////////////////////////////////////////////// メソッド

procedure TVkDevice<TVkDevices_>.FindQueFams;
begin
     vkGetPhysicalDeviceQueueFamilyProperties( Physic, @_QueFamsN, nil );
     Assert( _QueFamsN > 0 );

     SetLength( _QueFams, _QueFamsN );
     vkGetPhysicalDeviceQueueFamilyProperties( Physic, @_QueFamsN, @_QueFams[0] );
     Assert( _QueFamsN > 0 );
end;

procedure TVkDevice<TVkDevices_>.FindQueFamI;
var
   I :UInt32;
begin
     for i := 0 to _QueFamsN-1 do
     begin
          if ( _QueFams[I].queueFlags and Ord( VK_QUEUE_GRAPHICS_BIT ) ) <> 0 then
          begin
               _QueFamG := I;
               _QueFamP := I;

               Break;
          end;
     end;
end;

procedure TVkDevice<TVkDevices_>.FindQueFamI( const Surface_:VkSurfaceKHR );
var
   Fs :TArray<VkBool32>;
   I  :UInt32;
begin
     // Iterate over each queue to learn whether it supports presenting:
     SetLength( Fs, _QueFamsN );
     for I := 0 to _QueFamsN-1 do vkGetPhysicalDeviceSurfaceSupportKHR( _Physic, I, Surface_, @Fs[I] );

     // Search for a graphics and a present queue in the array of queue
     // families, try to find one that supports both
     _QueFamG := UINT32.MaxValue;
     _QueFamP := UINT32.MaxValue;
     for I := 0 to _QueFamsN-1 do
     begin
          if ( _QueFams[I].queueFlags and Ord( VK_QUEUE_GRAPHICS_BIT ) ) <> 0 then
          begin
               if _QueFamG = UINT32.MaxValue then _QueFamG := i;

               if Fs[I] = VK_TRUE then
               begin
                    _QueFamG := i;
                    _QueFamP := i;
                    Break;
               end;
          end;
     end;

     if _QueFamP = UINT32.MaxValue then
     begin
          // If didn't find a queue that supports both graphics and present, then
          // find a separate present queue.
          for I := 0 to _QueFamsN-1 do
          begin
               if Fs[I] = VK_TRUE then
               begin
                    _QueFamP := i;
                    Break;
               end;
          end;
     end;

     // Generate error if could not find queues that support graphics
     // and present
     if ( _QueFamG = UINT32.MaxValue ) or ( _QueFamP = UINT32.MaxValue ) then
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
     Assert( vkGetPhysicalDeviceSurfaceFormatsKHR( _Physic, Surface_, @FsN, nil ) = VK_SUCCESS );
     Assert( FsN > 0 );
     SetLength( Fs, FsN );
     Assert( vkGetPhysicalDeviceSurfaceFormatsKHR( _Physic, Surface_, @FsN, @Fs[0] ) = VK_SUCCESS );
     Assert( FsN > 0 );

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
     queue_info.queueFamilyIndex := _QueFamG;
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
     Assert( vkCreateDevice( Physic, @device_info, nil, @_Handle ) = VK_SUCCESS );
end;

procedure TVkDevice<TVkDevices_>.DestroHandle;
begin
     vkDeviceWaitIdle( _Handle );
     vkDestroyDevice( _Handle, nil );
end;

procedure TVkDevice<TVkDevices_>.init_device_queue;
begin
     vkGetDeviceQueue( _Handle, _QueFamG, 0, @_QueuerG );

     if _QueFamG = _QueFamP then _QueuerP := _QueuerG
                            else vkGetDeviceQueue( _Handle, _QueFamP, 0, @_QueuerP );
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TVkDevice<TVkDevices_>.Create( const Devices_:TVkDevices_; const Physic_:VkPhysicalDevice );
begin
     inherited Create;

     _Devices    := Devices_;
     _Physic := Physic_;

     vkGetPhysicalDeviceProperties( Physic, @_Props );

     vkGetPhysicalDeviceMemoryProperties( Physic, @Memorys );

     _Layeres := TVkDevLays_.Create( Self );

     FindQueFams;
     FindQueFamI( TVkDevices( _Devices ).Instance.Window.Surface.Handle );

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
     if Assigned( _Buffers ) then _Buffers.Free;

     DestroHandle;

     _Layeres.Free;

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