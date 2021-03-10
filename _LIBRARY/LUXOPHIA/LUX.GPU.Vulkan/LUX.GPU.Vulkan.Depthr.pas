unit LUX.GPU.Vulkan.Depthr;

interface //#################################################################### ■

uses vulkan_core;

type //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【型】

     TVkDepthr<TVkDevice_:class> = class;

     //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【レコード】

     //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【クラス】

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TVkDepthr

     TVkDepthr<TVkDevice_:class> = class
     private
       type TVkDepthr_  = TVkDepthr<TVkDevice_>;
     protected
       _Device :TVkDevice_;
       _Inform :VkImageCreateInfo;
       _Handle :VkImage;
       ///// アクセス
       function GetHandle :VkImage;
       procedure SetHandle( const Handle_:VkImage );
       ///// メソッド
       procedure CreateHandle;
       procedure DestroHandle;
     public
       constructor Create; overload;
       constructor Create( const Device_:TVkDevice_ ); overload;
       destructor Destroy; override;
       ///// プロパティ
       property Device  :TVkDevice_        read   _Device                ;
       property Inform  :VkImageCreateInfo read   _Inform                ;
       property Handle  :VkImage           read GetHandle write SetHandle;
     end;

//const //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【定数】

//var //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【変数】

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【ルーチン】

implementation //############################################################### ■

uses System.SysUtils,
     FMX.Types,
     vulkan.util,
     LUX.GPU.Vulkan;

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【レコード】

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【クラス】

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TVkDepthr

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

/////////////////////////////////////////////////////////////////////// アクセス

function TVkDepthr<TVkDevice_>.GetHandle :VkImage;
begin
     if _Handle = 0 then CreateHandle;

     Result := _Handle;
end;

procedure TVkDepthr<TVkDevice_>.SetHandle( const Handle_:VkImage );
begin
     if _Handle <> 0 then DestroHandle;

     _Handle := Handle_;
end;

/////////////////////////////////////////////////////////////////////// メソッド

procedure TVkDepthr<TVkDevice_>.CreateHandle;
var
   res          :VkResult;
   pass         :Boolean;
   image_info   :VkImageCreateInfo;
   props        :VkFormatProperties;
   depth_format :VkFormat;
   mem_alloc    :VkMemoryAllocateInfo;
   view_info    :VkImageViewCreateInfo;
   mem_reqs     :VkMemoryRequirements;
begin
     (* allow custom depth formats *)
     if TVkDevice( Device ).Instan.Vulkan.Info.depth.format = VK_FORMAT_UNDEFINED then TVkDevice( Device ).Instan.Vulkan.Info.depth.format := VK_FORMAT_D16_UNORM;

     depth_format := TVkDevice( Device ).Instan.Vulkan.Info.depth.format;
     vkGetPhysicalDeviceFormatProperties( TVkDevice( Device ).Physic, depth_format, @props );
     if ( props.linearTilingFeatures and Ord( VK_FORMAT_FEATURE_DEPTH_STENCIL_ATTACHMENT_BIT ) ) <> 0
     then image_info.tiling := VK_IMAGE_TILING_LINEAR
     else
     if ( props.optimalTilingFeatures and Ord( VK_FORMAT_FEATURE_DEPTH_STENCIL_ATTACHMENT_BIT ) ) <> 0
     then image_info.tiling := VK_IMAGE_TILING_OPTIMAL
     else
     begin
          (* Try other depth formats? *)
          Log.d( 'depth_format ' + Ord( depth_format ).ToString + ' Unsupported.' );
          RunError( 256-1 );
     end;

     image_info                       := Default( VkImageCreateInfo );
     image_info.sType                 := VK_STRUCTURE_TYPE_IMAGE_CREATE_INFO;
     image_info.pNext                 := nil;
     image_info.imageType             := VK_IMAGE_TYPE_2D;
     image_info.format                := depth_format;
     image_info.extent.width          := TVkDevice( Device ).Surfac.PxSizeX;
     image_info.extent.height         := TVkDevice( Device ).Surfac.PxSizeY;
     image_info.extent.depth          := 1;
     image_info.mipLevels             := 1;
     image_info.arrayLayers           := 1;
     image_info.samples               := NUM_SAMPLES;
     image_info.initialLayout         := VK_IMAGE_LAYOUT_UNDEFINED;
     image_info.queueFamilyIndexCount := 0;
     image_info.pQueueFamilyIndices   := nil;
     image_info.sharingMode           := VK_SHARING_MODE_EXCLUSIVE;
     image_info.usage                 := Ord( VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT );
     image_info.flags                 := 0;

     mem_alloc                 := Default( VkMemoryAllocateInfo );
     mem_alloc.sType           := VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO;
     mem_alloc.pNext           := nil;
     mem_alloc.allocationSize  := 0;
     mem_alloc.memoryTypeIndex := 0;

     view_info                                 := Default( VkImageViewCreateInfo );
     view_info.sType                           := VK_STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO;
     view_info.pNext                           := nil;
     view_info.image                           := VK_NULL_HANDLE;
     view_info.format                          := depth_format;
     view_info.components.r                    := VK_COMPONENT_SWIZZLE_R;
     view_info.components.g                    := VK_COMPONENT_SWIZZLE_G;
     view_info.components.b                    := VK_COMPONENT_SWIZZLE_B;
     view_info.components.a                    := VK_COMPONENT_SWIZZLE_A;
     view_info.subresourceRange.aspectMask     := Ord( VK_IMAGE_ASPECT_DEPTH_BIT );
     view_info.subresourceRange.baseMipLevel   := 0;
     view_info.subresourceRange.levelCount     := 1;
     view_info.subresourceRange.baseArrayLayer := 0;
     view_info.subresourceRange.layerCount     := 1;
     view_info.viewType                        := VK_IMAGE_VIEW_TYPE_2D;
     view_info.flags                           := 0;

     if ( depth_format = VK_FORMAT_D16_UNORM_S8_UINT ) or ( depth_format = VK_FORMAT_D24_UNORM_S8_UINT ) or
        ( depth_format = VK_FORMAT_D32_SFLOAT_S8_UINT )
     then view_info.subresourceRange.aspectMask := view_info.subresourceRange.aspectMask or Ord( VK_IMAGE_ASPECT_STENCIL_BIT );

     (* Create image *)
     res := vkCreateImage( TVkDevice( Device ).Handle, @image_info, nil, @TVkDevice( Device ).Instan.Vulkan.Info.depth.image );
     Assert( res = VK_SUCCESS );

     vkGetImageMemoryRequirements( TVkDevice( Device ).Handle, TVkDevice( Device ).Instan.Vulkan.Info.depth.image, @mem_reqs );

     mem_alloc.allocationSize := mem_reqs.size;
     (* Use the memory properties to determine the type of memory required *)
     pass := TVkDevice( Device ).memory_type_from_properties( mem_reqs.memoryTypeBits, Ord( VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT ), mem_alloc.memoryTypeIndex );
     Assert( pass );

     (* Allocate memory *)
     res := vkAllocateMemory( TVkDevice( Device ).Handle, @mem_alloc, nil, @TVkDevice( Device ).Instan.Vulkan.Info.depth.mem );
     Assert( res = VK_SUCCESS );

     (* Bind memory *)
     res := vkBindImageMemory( TVkDevice( Device ).Handle, TVkDevice( Device ).Instan.Vulkan.Info.depth.image, TVkDevice( Device ).Instan.Vulkan.Info.depth.mem, 0 );
     Assert( res = VK_SUCCESS );

     (* Create image view *)
     view_info.image := TVkDevice( Device ).Instan.Vulkan.Info.depth.image;
     res := vkCreateImageView( TVkDevice( Device ).Handle, @view_info, nil, @TVkDevice( Device ).Instan.Vulkan.Info.depth.view );
     Assert( res = VK_SUCCESS );
end;

procedure TVkDepthr<TVkDevice_>.DestroHandle;
begin
     vkDestroyImageView( TVkDevice( Device ).Handle, TVkDevice( Device ).Instan.Vulkan.Info.depth.view , nil );
     vkDestroyImage    ( TVkDevice( Device ).Handle, TVkDevice( Device ).Instan.Vulkan.Info.depth.image, nil );
     vkFreeMemory      ( TVkDevice( Device ).Handle, TVkDevice( Device ).Instan.Vulkan.Info.depth.mem  , nil );
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TVkDepthr<TVkDevice_>.Create;
begin
     inherited Create;

     _Handle := 0;
end;

constructor TVkDepthr<TVkDevice_>.Create( const Device_:TVkDevice_ );
begin
     Create;

     _Device := Device_;

     TVkDevice( _Device ).Depthr := TVkDepthr( Self );

     CreateHandle;
end;

destructor TVkDepthr<TVkDevice_>.Destroy;
begin
     DestroHandle;

      Handle := 0;

     inherited;
end;

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【ルーチン】

//############################################################################## □

initialization //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ 初期化

finalization //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ 最終化

end. //######################################################################### ■