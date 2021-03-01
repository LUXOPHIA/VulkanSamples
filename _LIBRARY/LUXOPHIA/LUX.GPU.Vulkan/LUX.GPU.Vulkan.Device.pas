unit LUX.GPU.Vulkan.Device;

interface //#################################################################### ■

uses System.Generics.Collections,
     vulkan_core, vulkan_win32,
     vulkan.util,
     LUX, LUX.D1, LUX.D2, LUX.D3, LUX.D4, LUX.D4x4,
     LUX.GPU.Vulkan;

type //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【型】

     TVkDevices = class;
     TVkDevice  = class;

     //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【レコード】

     //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【クラス】

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TVkDevices

     TVkDevices = class( TVkObject )
     private
     protected
       _Devices :TObjectList<TVkDevice>;
       ///// アクセス
       ///// メソッド
       procedure GetDevices;
     public
       constructor Create( const Vulkan_:TVulkan );
       procedure AfterConstruction; override;
       destructor Destroy; override;
       ///// プロパティ
       ///// メソッド
     end;

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TVkDevice

     TVkDevice = class
     private
     protected
       _Devices :TVkDevices;
       _Handle  :VkPhysicalDevice;
       /////
       ///// メソッド
       function init_device_extension_properties( var layer_props_:T_layer_properties ) :VkResult;
     public
       constructor Create( const Devices_:TVkDevices; const Handle_:VkPhysicalDevice );
       procedure AfterConstruction; override;
       destructor Destroy; override;
       ///// プロパティ
       property Devices :TVkDevices       read _Devices;
       property Handle  :VkPhysicalDevice read _Handle ;
     end;

//const //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【定数】

//var //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【変数】

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【ルーチン】

implementation //############################################################### ■

uses System.Classes;

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【レコード】

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【クラス】

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TVkShader

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

/////////////////////////////////////////////////////////////////////// アクセス

/////////////////////////////////////////////////////////////////////// メソッド

procedure TVkDevices.GetDevices;
var
   gpu_count_, req_count :UInt32;
   G :VkPhysicalDevice;
begin
     req_count := 1;
     Assert( ( vkEnumeratePhysicalDevices( _Vulkan.Info.inst, @gpu_count_, nil ) = VK_SUCCESS ) and ( gpu_count_ > 0 ) );
     SetLength( _Vulkan.Info.gpus, gpu_count_ );

     Assert( ( vkEnumeratePhysicalDevices( _Vulkan.Info.inst, @gpu_count_, @_Vulkan.Info.gpus[0] ) = VK_SUCCESS ) and ( gpu_count_ >= req_count ) );

     for G in _Vulkan.Info.gpus do _Devices.Add( TVkDevice.Create( Self, G ) );
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TVkDevices.Create( const Vulkan_:TVulkan );
begin
     inherited;

     _Devices := TObjectList<TVkDevice>.Create;
end;

procedure TVkDevices.AfterConstruction;
begin
     inherited;

     GetDevices;
end;

destructor TVkDevices.Destroy;
begin
     _Devices.Free;

     inherited;
end;

/////////////////////////////////////////////////////////////////////// メソッド

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TVkDevice

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TVkDevice.Create( const Devices_:TVkDevices; const Handle_:VkPhysicalDevice );
begin
     inherited Create;

     _Devices := Devices_;
     _Handle  := Handle_;
end;

function TVkDevice.init_device_extension_properties( var layer_props_:T_layer_properties ) :VkResult;
var
   device_extensions      :P_VkExtensionProperties;
   device_extension_count :UInt32;
   layer_name             :PAnsiChar;
begin
     layer_name := layer_props_.properties.layerName;

     repeat
           Result := vkEnumerateDeviceExtensionProperties( Handle, layer_name, @device_extension_count, nil );
           if Result <> VK_SUCCESS then Exit;

           if device_extension_count = 0 then Exit( VK_SUCCESS );

           SetLength( layer_props_.device_extensions, device_extension_count );
           device_extensions := @layer_props_.device_extensions[0];
           Result := vkEnumerateDeviceExtensionProperties( Handle, layer_name, @device_extension_count, device_extensions );

     until Result <> VK_INCOMPLETE;
end;

procedure TVkDevice.AfterConstruction;
var
   I :Integer;
begin
     inherited;

     vkGetPhysicalDeviceQueueFamilyProperties( Handle, @Devices.Vulkan.Info.queue_family_count, nil );
     Assert( Devices.Vulkan.Info.queue_family_count > 1 );

     SetLength( Devices.Vulkan.Info.queue_props, Devices.Vulkan.Info.queue_family_count );
     vkGetPhysicalDeviceQueueFamilyProperties( Handle, @Devices.Vulkan.Info.queue_family_count, @Devices.Vulkan.Info.queue_props[0] );
     Assert( Devices.Vulkan.Info.queue_family_count > 1 );

     (* This is as good a place as any to do this *)
     vkGetPhysicalDeviceMemoryProperties( Handle, @Devices.Vulkan.Info.memory_properties );
     vkGetPhysicalDeviceProperties( Handle, @Devices.Vulkan.Info.gpu_props );
     (* query device extensions for enabled layers *)
     for I := 0 to Length( Devices.Vulkan.Info.instance_layer_properties )-1
     do init_device_extension_properties( Devices.Vulkan.Info.instance_layer_properties[I] );
end;

destructor TVkDevice.Destroy;
begin

     inherited;
end;

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【ルーチン】

//############################################################################## □

initialization //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ 初期化

finalization //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ 最終化

end. //######################################################################### ■