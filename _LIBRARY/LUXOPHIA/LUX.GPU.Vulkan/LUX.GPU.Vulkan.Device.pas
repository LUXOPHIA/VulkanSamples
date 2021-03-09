unit LUX.GPU.Vulkan.Device;

interface //#################################################################### ■

uses System.Generics.Collections,
     vulkan_core,
     LUX.GPU.Vulkan.Layere,
     LUX.GPU.Vulkan.Pipeline,
     LUX.GPU.Vulkan.Surfac,
     LUX.GPU.Vulkan.Buffer,
     LUX.GPU.Vulkan.Command,
     LUX.GPU.Vulkan.Swapchain;

type //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【型】

     TVkDevices<TVkInstan_:class>  = class;
       TVkDevice<TVkInstan_:class> = class;

     //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【レコード】

     //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【クラス】

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TVkDevice

     TVkDevice<TVkInstan_:class> = class
     private
       type TVkDevice_      = TVkDevice<TVkInstan_>;
            TVkSurfac_      = TVkSurfac<TVkInstan_>;
            TVkDevLays_     = TVkDevLays<TVkDevice_>;
            TVkBuffer_      = TVkBuffer<TVkDevice_>;
            TVkCommandPool_ = TVkCommandPool<TVkDevice_>;
            TVkSwapchain_   = TVkSwapchain<TVkDevice_>;
     protected
       _Instan   :TVkInstan_;
       _Physic   :VkPhysicalDevice;
       _Propers  :VkPhysicalDeviceProperties;
       _Memorys  :VkPhysicalDeviceMemoryProperties;
       _Layeres  :TVkDevLays_;
       _Surfac   :TVkSurfac_;
       _Handle   :VkDevice;
       _Extenss  :TArray<PAnsiChar>;
       _FamilysN :UInt32;
       _Familys  :TArray<VkQueueFamilyProperties>;
       _FamilyG  :UInt32;
       _FamilyP  :UInt32;
       _Format   :VkFormat;
       _Buffers  :TVkBuffer_;
       _Poolers  :TVkCommandPool_;
       _Swapchs  :TVkSwapchain_;
       _QueuerG  :VkQueue;
       _QueuerP  :VkQueue;
       ///// アクセス
       function GetQueFams( const I_:Integer ) :VkQueueFamilyProperties;
       function GetHandle :VkDevice;
       procedure SetHandle( const Handle_:VkDevice );
       ///// メソッド
       procedure FindFamilys;
       procedure FindFamilyI; overload;
       procedure FindFamilyI( const Surfac_:VkSurfaceKHR ); overload;
       procedure FindFormat( const Surfac_:VkSurfaceKHR );
       procedure CreateHandle;
       procedure DestroHandle;
       procedure init_device_queue;
     public
       constructor Create; overload;
       constructor Create( const Instan_:TVkInstan_ ); overload;
       constructor Create( const Instan_:TVkInstan_; const Physic_:VkPhysicalDevice ); overload;
       constructor Create( const Instan_:TVkInstan_; const Physic_:VkPhysicalDevice; const Surfac_:TVkSurfac_ ); overload;
       procedure AfterConstruction; override;
       destructor Destroy; override;
       ///// プロパティ
       property Instan                      :TVkInstan_                       read   _Instan                   ;
       property Physic                      :VkPhysicalDevice                 read   _Physic                   ;
       property Propers                     :VkPhysicalDeviceProperties       read   _Propers                  ;
       property Memorys                     :VkPhysicalDeviceMemoryProperties read   _Memorys                  ;
       property Layeres                     :TVkDevLays_                      read   _Layeres  write   _Layeres;
       property Handle                      :VkDevice                         read GetHandle   write SetHandle ;
       property Extenss                     :TArray<PAnsiChar>                read   _Extenss                  ;
       property FamilysN                    :UInt32                           read   _FamilysN                 ;
       property Familys[ const I_:Integer ] :VkQueueFamilyProperties          read GetQueFams                  ;
       property FamilyG                     :UInt32                           read   _FamilyG                  ;
       property FamilyP                     :UInt32                           read   _FamilyP                  ;
       property Format                      :VkFormat                         read   _Format                   ;
       property Buffers                     :TVkBuffer_                       read   _Buffers  write   _Buffers;
       property Pooler                      :TVkCommandPool_                  read   _Poolers  write   _Poolers;
       property Swapchs                     :TVkSwapchain_                    read   _Swapchs  write   _Swapchs;
       property QueuerG                     :VkQueue                          read   _QueuerG                  ;
       property QueuerP                     :VkQueue                          read   _QueuerP                  ;
       ///// メソッド
       function memory_type_from_properties( typeBits:UInt32; const requirements_mask:VkFlags; var typeIndex:UInt32 ) :Boolean;
     end;

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TVkDevices

     TVkDevices<TVkInstan_:class> = class( TObjectList<TVkDevice<TVkInstan_>> )
     private
       type TVkDevices_ = TVkDevices<TVkInstan_>;
            TVkDevice_  = TVkDevice<TVkInstan_>;
     protected
       _Instan :TVkInstan_;
       ///// アクセス
       ///// メソッド
       procedure FindDevices;
     public
       constructor Create( const Instan_:TVkInstan_ );
       procedure AfterConstruction; override;
       destructor Destroy; override;
       ///// プロパティ
       property Instan :TVkInstan_ read _Instan;
       ///// メソッド
       function Add( const Physic_:VkPhysicalDevice ) :TVkDevice_; overload;
     end;

//const //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【定数】

//var //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【変数】

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【ルーチン】

implementation //############################################################### ■

uses System.SysUtils,
     FMX.Types,
     LUX.GPU.Vulkan;

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【レコード】

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【クラス】

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TVkDevice

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

/////////////////////////////////////////////////////////////////////// アクセス

function TVkDevice<TVkInstan_>.GetQueFams( const I_:Integer ) :VkQueueFamilyProperties;
begin
     Result := _Familys[ I_ ];
end;

function TVkDevice<TVkInstan_>.GetHandle :VkDevice;
begin
     if not Assigned( _Handle ) then
     begin
          CreateHandle;

          init_device_queue;
     end;

     Result := _Handle;
end;

procedure TVkDevice<TVkInstan_>.SetHandle( const Handle_:VkDevice );
begin
     if Assigned( _Handle ) then DestroHandle;

     _Handle := Handle_;
end;

/////////////////////////////////////////////////////////////////////// メソッド

procedure TVkDevice<TVkInstan_>.FindFamilys;
begin
     vkGetPhysicalDeviceQueueFamilyProperties( _Physic, @_FamilysN, nil );
     Assert( _FamilysN > 0 );

     SetLength( _Familys, _FamilysN );
     vkGetPhysicalDeviceQueueFamilyProperties( _Physic, @_FamilysN, @_Familys[0] );
     Assert( _FamilysN > 0 );
end;

procedure TVkDevice<TVkInstan_>.FindFamilyI;
var
   I :UInt32;
begin
     for i := 0 to _FamilysN-1 do
     begin
          if ( _Familys[I].queueFlags and Ord( VK_QUEUE_GRAPHICS_BIT ) ) <> 0 then
          begin
               _FamilyG := I;
               _FamilyP := I;

               Break;
          end;
     end;
end;

procedure TVkDevice<TVkInstan_>.FindFamilyI( const Surfac_:VkSurfaceKHR );
var
   Fs :TArray<VkBool32>;
   I  :UInt32;
begin
     // Iterate over each queue to learn whether it supports presenting:
     SetLength( Fs, _FamilysN );
     for I := 0 to _FamilysN-1 do vkGetPhysicalDeviceSurfaceSupportKHR( _Physic, I, Surfac_, @Fs[I] );

     // Search for a graphics and a present queue in the array of queue
     // families, try to find one that supports both
     _FamilyG := UINT32.MaxValue;
     _FamilyP := UINT32.MaxValue;
     for I := 0 to _FamilysN-1 do
     begin
          if ( _Familys[I].queueFlags and Ord( VK_QUEUE_GRAPHICS_BIT ) ) <> 0 then
          begin
               if _FamilyG = UINT32.MaxValue then _FamilyG := I;

               if Fs[I] = VK_TRUE then
               begin
                    _FamilyG := I;
                    _FamilyP := I;
                    Break;
               end;
          end;
     end;

     if _FamilyP = UINT32.MaxValue then
     begin
          // If didn't find a queue that supports both graphics and present, then
          // find a separate present queue.
          for I := 0 to _FamilysN-1 do
          begin
               if Fs[I] = VK_TRUE then
               begin
                    _FamilyP := I;
                    Break;
               end;
          end;
     end;

     // Generate error if could not find queues that support graphics
     // and present
     if ( _FamilyG = UINT32.MaxValue ) or ( _FamilyP = UINT32.MaxValue ) then
     begin
          Log.d( 'Could not find a queues for both graphics and present' );
          RunError( 256-1 );
     end;
end;

procedure TVkDevice<TVkInstan_>.FindFormat( const Surfac_:VkSurfaceKHR );
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
     Assert( vkGetPhysicalDeviceSurfaceFormatsKHR( _Physic, Surfac_, @FsN, nil ) = VK_SUCCESS );
     Assert( FsN > 0 );
     SetLength( Fs, FsN );
     Assert( vkGetPhysicalDeviceSurfaceFormatsKHR( _Physic, Surfac_, @FsN, @Fs[0] ) = VK_SUCCESS );
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

procedure TVkDevice<TVkInstan_>.CreateHandle;
var
   queue_priorities :array [ 0..1-1 ] of Single;
   queue_info       :VkDeviceQueueCreateInfo;
   device_info      :VkDeviceCreateInfo;
begin
     queue_priorities[0] := 0;

     queue_info                  := Default( VkDeviceQueueCreateInfo );
     queue_info.sType            := VK_STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO;
     queue_info.pNext            := nil;
     queue_info.queueCount       := 1;
     queue_info.pQueuePriorities := @queue_priorities[0];
     queue_info.queueFamilyIndex := _FamilyG;

     device_info                              := Default( VkDeviceCreateInfo );
     device_info.sType                        := VK_STRUCTURE_TYPE_DEVICE_CREATE_INFO;
     device_info.pNext                        := nil;
     device_info.queueCreateInfoCount         := 1;
     device_info.pQueueCreateInfos            := @queue_info;
     device_info.enabledExtensionCount        := Length( _Extenss );
     if device_info.enabledExtensionCount > 0
     then device_info.ppEnabledExtensionNames := @_Extenss[0]
     else device_info.ppEnabledExtensionNames := nil;
     device_info.pEnabledFeatures             := nil;

     Assert( vkCreateDevice( _Physic, @device_info, nil, @_Handle ) = VK_SUCCESS );
end;

procedure TVkDevice<TVkInstan_>.DestroHandle;
begin
     vkDeviceWaitIdle( _Handle );
     vkDestroyDevice( _Handle, nil );
end;

procedure TVkDevice<TVkInstan_>.init_device_queue;
begin
     vkGetDeviceQueue( _Handle, _FamilyG, 0, @_QueuerG );

     if _FamilyG = _FamilyP then _QueuerP := _QueuerG
                            else vkGetDeviceQueue( _Handle, _FamilyP, 0, @_QueuerP );
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TVkDevice<TVkInstan_>.Create;
begin
     inherited;

     _Handle := nil;

     _Extenss := _Extenss + [ VK_KHR_SWAPCHAIN_EXTENSION_NAME ];
end;

constructor TVkDevice<TVkInstan_>.Create( const Instan_:TVkInstan_ );
begin
     Create;

     _Instan := Instan_;

     TVkInstan( _Instan ).Devices.Add( TVkDevice( Self ) );
end;

constructor TVkDevice<TVkInstan_>.Create( const Instan_:TVkInstan_; const Physic_:VkPhysicalDevice );
begin
     Create( Instan_ );

     _Physic := Physic_;

     vkGetPhysicalDeviceProperties      ( _Physic, @_Propers );
     vkGetPhysicalDeviceMemoryProperties( _Physic, @_Memorys );

     _Layeres := TVkDevLays_.Create( Self );

     FindFamilys;
end;

constructor TVkDevice<TVkInstan_>.Create( const Instan_:TVkInstan_; const Physic_:VkPhysicalDevice; const Surfac_:TVkSurfac_ );
begin
     Create( Instan_, Physic_ );

     _Surfac := Surfac_;

     FindFamilyI( _Surfac.Handle );

     FindFormat( _Surfac.Handle );
end;

procedure TVkDevice<TVkInstan_>.AfterConstruction;
begin
     inherited;

end;

destructor TVkDevice<TVkInstan_>.Destroy;
begin
     if Assigned( _Buffers ) then _Buffers.Free;

      Handle := nil;

     _Layeres.Free;

     inherited;
end;

/////////////////////////////////////////////////////////////////////// メソッド

function TVkDevice<TVkInstan_>.memory_type_from_properties( typeBits:UInt32; const requirements_mask:VkFlags; var typeIndex:UInt32 ) :Boolean;
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

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TVkDevices

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

/////////////////////////////////////////////////////////////////////// アクセス

/////////////////////////////////////////////////////////////////////// メソッド

procedure TVkDevices<TVkInstan_>.FindDevices;
var
   PsN :UInt32;
   Ps :TArray<VkPhysicalDevice>;
   P :VkPhysicalDevice;
begin
     Assert( vkEnumeratePhysicalDevices( TVkInstan( _Instan ).Handle, @PsN, nil ) = VK_SUCCESS );
     Assert( PsN > 0 );

     SetLength( Ps, PsN );

     Assert( vkEnumeratePhysicalDevices( TVkInstan( _Instan ).Handle, @PsN, @Ps[0] ) = VK_SUCCESS );
     Assert( PsN > 0 );

     for P in Ps do Add( P );
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TVkDevices<TVkInstan_>.Create( const Instan_:TVkInstan_ );
begin
     inherited Create;

     _Instan := Instan_;

     TVkInstan( _Instan ).Devices := TVkDevices( Self );
end;

procedure TVkDevices<TVkInstan_>.AfterConstruction;
begin
     inherited;

     FindDevices;
end;

destructor TVkDevices<TVkInstan_>.Destroy;
begin

     inherited;
end;

/////////////////////////////////////////////////////////////////////// メソッド

function TVkDevices<TVkInstan_>.Add( const Physic_:VkPhysicalDevice ) :TVkDevice_;
begin
     Result := TVkDevice_.Create( _Instan, Physic_, TVkSurfac<TVkInstan_>( TVkInstan( _Instan ).Surfacs[0] ) );
end;

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【ルーチン】

//############################################################################## □

initialization //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ 初期化

finalization //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ 最終化

end. //######################################################################### ■