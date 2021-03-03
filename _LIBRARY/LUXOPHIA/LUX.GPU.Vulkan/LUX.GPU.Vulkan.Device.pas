unit LUX.GPU.Vulkan.Device;

interface //#################################################################### ■

uses System.Generics.Collections,
     vulkan_core, vulkan_win32,
     vulkan.util,
     LUX.GPU.Vulkan.Pipeline,
     LUX.GPU.Vulkan.Window;

type //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【型】

     TVkDevices<TVkInstance_:class> = class;
     TVkDevice<TVkDevices_:class>  = class;

     //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【レコード】

     //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【クラス】

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TVkDevices

     TVkDevices<TVkInstance_:class> = class
     private
       type TVkDevices_ = TVkDevices<TVkInstance_>;
            TVkDevice_  = TVkDevice<TVkDevices_>;
     protected
       _Instance :TVkInstance_;
       _Devices  :TObjectList<TVkDevice_>;
       ///// アクセス
       ///// メソッド
       procedure GetDevices;
     public
       constructor Create( const Instance_:TVkInstance_ );
       procedure AfterConstruction; override;
       destructor Destroy; override;
       ///// プロパティ
       property Instance :TVkInstance_            read _Instance;
       property Devices  :TObjectList<TVkDevice_> read _Devices ;
       ///// メソッド
     end;

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TVkDevice

     TVkDevice<TVkDevices_:class> = class
     private
       type TVkDevice_  = TVkDevice<TVkDevices_>;
            TVkWindow_  = TVkWindow<TVkDevice_>;
     protected
       _Devices    :TVkDevices_;
       _PhysHandle :VkPhysicalDevice;
       _Props      :VkPhysicalDeviceProperties;
       _Handle     :VkDevice;
       _Extensions :TArray<PAnsiChar>;
       _Window     :TVkWindow_;
       /////
       ///// メソッド
       function init_device_extension_properties( var layer_props_:T_layer_properties ) :VkResult;
       procedure GetQueueFamilys;
       procedure CreateHandle;
       procedure DestroHandle;
     public
       constructor Create( const Devices_:TVkDevices_; const Handle_:VkPhysicalDevice );
       procedure AfterConstruction; override;
       destructor Destroy; override;
       ///// プロパティ
       property Devices    :TVkDevices_                read _Devices   ;
       property PhysHandle :VkPhysicalDevice           read _PhysHandle;
       property Props      :VkPhysicalDeviceProperties read _Props     ;
       property Handle     :VkDevice                   read _Handle    ;
       property Extensions :TArray<PAnsiChar>          read _Extensions;
       property Window     :TVkWindow_                 read _Window     write _Window;
     end;

//const //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【定数】

//var //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【変数】

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【ルーチン】

implementation //############################################################### ■

uses System.Classes,
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

     for D in Ds do _Devices.Add( TVkDevice_.Create( Self, D ) );
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TVkDevices<TVkInstance_>.Create( const Instance_:TVkInstance_ );
begin
     inherited Create;

     _Instance := Instance_;

     _Devices := TObjectList<TVkDevice_>.Create;
end;

procedure TVkDevices<TVkInstance_>.AfterConstruction;
begin
     inherited;

     GetDevices;
end;

destructor TVkDevices<TVkInstance_>.Destroy;
begin
     _Devices.Free;

     inherited;
end;

/////////////////////////////////////////////////////////////////////// メソッド

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TVkDevice

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

function TVkDevice<TVkDevices_>.init_device_extension_properties( var layer_props_:T_layer_properties ) :VkResult;
var
   device_extension_count :UInt32;
   layer_name             :PAnsiChar;
begin
     layer_name := layer_props_.properties.layerName;

     repeat
           Result := vkEnumerateDeviceExtensionProperties( PhysHandle, layer_name, @device_extension_count, nil );
           if Result <> VK_SUCCESS then Exit;

           if device_extension_count = 0 then Exit( VK_SUCCESS );

           SetLength( layer_props_.device_extensions, device_extension_count );
           Result := vkEnumerateDeviceExtensionProperties( PhysHandle, layer_name, @device_extension_count, @layer_props_.device_extensions[0] );

     until Result <> VK_INCOMPLETE;
end;

procedure TVkDevice<TVkDevices_>.GetQueueFamilys;
var
   I :Integer;
begin
     vkGetPhysicalDeviceQueueFamilyProperties( PhysHandle, @TVkDevices( Devices ).Instance.Vulkan.Info.queue_family_count, nil );
     Assert( TVkDevices( Devices ).Instance.Vulkan.Info.queue_family_count > 1 );

     SetLength( TVkDevices( Devices ).Instance.Vulkan.Info.queue_props, TVkDevices( Devices ).Instance.Vulkan.Info.queue_family_count );
     vkGetPhysicalDeviceQueueFamilyProperties( PhysHandle, @TVkDevices( Devices ).Instance.Vulkan.Info.queue_family_count, @TVkDevices( Devices ).Instance.Vulkan.Info.queue_props[0] );
     Assert( TVkDevices( Devices ).Instance.Vulkan.Info.queue_family_count > 1 );

     (* This is as good a place as any to do this *)
     vkGetPhysicalDeviceMemoryProperties( PhysHandle, @TVkDevices( Devices ).Instance.Vulkan.Info.memory_properties );
     vkGetPhysicalDeviceProperties( PhysHandle, @_Props );
     (* query device extensions for enabled layers *)
     for I := 0 to Length( TVkDevices( Devices ).Instance.Vulkan.Layers )-1
     do init_device_extension_properties( TVkDevices( Devices ).Instance.Vulkan.Layers[I] );
end;

procedure TVkDevice<TVkDevices_>.CreateHandle;
var
   queue_info       :VkDeviceQueueCreateInfo;
   queue_priorities :array [ 0..1-1 ] of Single;
   device_info      :VkDeviceCreateInfo;
begin
     queue_priorities[0]         := 0;
     queue_info                  := Default( VkDeviceQueueCreateInfo );
     queue_info.sType            := VK_STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO;
     queue_info.pNext            := nil;
     queue_info.queueCount       := 1;
     queue_info.pQueuePriorities := @queue_priorities[0];
     queue_info.queueFamilyIndex := TVkDevices( Devices ).Instance.Vulkan.Info.graphics_queue_family_index;
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

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TVkDevice<TVkDevices_>.Create( const Devices_:TVkDevices_; const Handle_:VkPhysicalDevice );
begin
     inherited Create;

     _Devices     := Devices_;
     _PhysHandle  := Handle_;
end;

procedure TVkDevice<TVkDevices_>.AfterConstruction;
begin
     inherited;

     GetQueueFamilys;

     _Extensions := _Extensions + [ VK_KHR_SWAPCHAIN_EXTENSION_NAME ];

     CreateHandle;
end;

destructor TVkDevice<TVkDevices_>.Destroy;
begin
     DestroHandle;

     inherited;
end;

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【ルーチン】

//############################################################################## □

initialization //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ 初期化

finalization //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ 最終化

end. //######################################################################### ■