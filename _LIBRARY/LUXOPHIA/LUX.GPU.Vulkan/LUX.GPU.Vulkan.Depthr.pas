unit LUX.GPU.Vulkan.Depthr;

interface //#################################################################### ■

uses vulkan_core;

type //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【型】

     TVkDepthr<TVkDevice_:class>   = class;
       TVkDepMem<TVkDevice_:class> = class;
       TVkDepVie<TVkDevice_:class> = class;

     //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【レコード】

     //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【クラス】

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TVkDepVie

     TVkDepVie<TVkDevice_:class> = class
     private
       type TVkDepthr_ = TVkDepthr<TVkDevice_>;
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

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TVkDepMem

     TVkDepMem<TVkDevice_:class> = class
     private
       type TVkDepthr_ = TVkDepthr<TVkDevice_>;
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
       mem    :VkDeviceMemory;
       view   :VkImageView;
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

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TVkDepVie

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

/////////////////////////////////////////////////////////////////////// アクセス

function TVkDepVie<TVkDevice_>.GetHandle :VkImage;
begin
     if _Handle = 0 then CreateHandle;

     Result := _Handle;
end;

procedure TVkDepVie<TVkDevice_>.SetHandle( const Handle_:VkImage );
begin
     if _Handle <> 0 then DestroHandle;

     _Handle := Handle_;
end;

/////////////////////////////////////////////////////////////////////// メソッド

procedure TVkDepVie<TVkDevice_>.CreateHandle;
var
   mem_reqs  :VkMemoryRequirements;
   mem_alloc :VkMemoryAllocateInfo;
   view_info :VkImageViewCreateInfo;
begin
     Assert( vkCreateImage( TVkDevice( Device ).Handle, @_Inform, nil, @_Handle ) = VK_SUCCESS );

     vkGetImageMemoryRequirements( TVkDevice( Device ).Handle, _Handle, @mem_reqs );

     mem_alloc.sType          := VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO;
     mem_alloc.pNext          := nil;
     mem_alloc.allocationSize := mem_reqs.size;
     Assert( TVkDevice( Device ).memory_type_from_properties( mem_reqs.memoryTypeBits, Ord( VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT ), mem_alloc.memoryTypeIndex ) );

     (* Allocate memory *)
     Assert( vkAllocateMemory( TVkDevice( Device ).Handle, @mem_alloc, nil, @TVkDevice( Device ).Depthr.mem ) = VK_SUCCESS );

     (* Bind memory *)
     Assert( vkBindImageMemory( TVkDevice( Device ).Handle, _Handle, TVkDevice( Device ).Depthr.mem, 0 ) = VK_SUCCESS );

     view_info.sType                           := VK_STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO;
     view_info.pNext                           := nil;
     view_info.flags                           := 0;
     view_info.image                           := _Handle;
     view_info.viewType                        := VK_IMAGE_VIEW_TYPE_2D;
     view_info.format                          := _Inform.format;
     view_info.components.r                    := VK_COMPONENT_SWIZZLE_R;
     view_info.components.g                    := VK_COMPONENT_SWIZZLE_G;
     view_info.components.b                    := VK_COMPONENT_SWIZZLE_B;
     view_info.components.a                    := VK_COMPONENT_SWIZZLE_A;
     view_info.subresourceRange.aspectMask     := Ord( VK_IMAGE_ASPECT_DEPTH_BIT );
     if ( _Inform.format = VK_FORMAT_D16_UNORM_S8_UINT  )
     or ( _Inform.format = VK_FORMAT_D24_UNORM_S8_UINT  )
     or ( _Inform.format = VK_FORMAT_D32_SFLOAT_S8_UINT ) then
     view_info.subresourceRange.aspectMask     := view_info.subresourceRange.aspectMask or Ord( VK_IMAGE_ASPECT_STENCIL_BIT );
     view_info.subresourceRange.baseMipLevel   := 0;
     view_info.subresourceRange.levelCount     := 1;
     view_info.subresourceRange.baseArrayLayer := 0;
     view_info.subresourceRange.layerCount     := 1;

     (* Create image view *)
     Assert( vkCreateImageView( TVkDevice( Device ).Handle, @view_info, nil, @TVkDevice( Device ).Depthr.view ) = VK_SUCCESS );
end;

procedure TVkDepVie<TVkDevice_>.DestroHandle;
begin
     vkDestroyImageView( TVkDevice( Device ).Handle, TVkDevice( Device ).Depthr.view , nil );
     vkFreeMemory      ( TVkDevice( Device ).Handle, TVkDevice( Device ).Depthr.mem  , nil );
     vkDestroyImage    ( TVkDevice( Device ).Handle, _Handle, nil );
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TVkDepVie<TVkDevice_>.Create;
begin
     inherited Create;

     _Handle := 0;
end;

constructor TVkDepVie<TVkDevice_>.Create( const Device_:TVkDevice_ );
var
   P :VkFormatProperties;
begin
     Create;

     _Device := Device_;
end;

destructor TVkDepVie<TVkDevice_>.Destroy;
begin
      Handle := 0;

     inherited;
end;

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TVkDepMem

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

/////////////////////////////////////////////////////////////////////// アクセス

function TVkDepMem<TVkDevice_>.GetHandle :VkImage;
begin
     if _Handle = 0 then CreateHandle;

     Result := _Handle;
end;

procedure TVkDepMem<TVkDevice_>.SetHandle( const Handle_:VkImage );
begin
     if _Handle <> 0 then DestroHandle;

     _Handle := Handle_;
end;

/////////////////////////////////////////////////////////////////////// メソッド

procedure TVkDepMem<TVkDevice_>.CreateHandle;
var
   mem_reqs  :VkMemoryRequirements;
   mem_alloc :VkMemoryAllocateInfo;
   view_info :VkImageViewCreateInfo;
begin
     Assert( vkCreateImage( TVkDevice( Device ).Handle, @_Inform, nil, @_Handle ) = VK_SUCCESS );

     vkGetImageMemoryRequirements( TVkDevice( Device ).Handle, _Handle, @mem_reqs );

     mem_alloc.sType          := VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO;
     mem_alloc.pNext          := nil;
     mem_alloc.allocationSize := mem_reqs.size;
     Assert( TVkDevice( Device ).memory_type_from_properties( mem_reqs.memoryTypeBits, Ord( VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT ), mem_alloc.memoryTypeIndex ) );

     (* Allocate memory *)
     Assert( vkAllocateMemory( TVkDevice( Device ).Handle, @mem_alloc, nil, @TVkDevice( Device ).Depthr.mem ) = VK_SUCCESS );

     (* Bind memory *)
     Assert( vkBindImageMemory( TVkDevice( Device ).Handle, _Handle, TVkDevice( Device ).Depthr.mem, 0 ) = VK_SUCCESS );

     view_info.sType                           := VK_STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO;
     view_info.pNext                           := nil;
     view_info.flags                           := 0;
     view_info.image                           := _Handle;
     view_info.viewType                        := VK_IMAGE_VIEW_TYPE_2D;
     view_info.format                          := _Inform.format;
     view_info.components.r                    := VK_COMPONENT_SWIZZLE_R;
     view_info.components.g                    := VK_COMPONENT_SWIZZLE_G;
     view_info.components.b                    := VK_COMPONENT_SWIZZLE_B;
     view_info.components.a                    := VK_COMPONENT_SWIZZLE_A;
     view_info.subresourceRange.aspectMask     := Ord( VK_IMAGE_ASPECT_DEPTH_BIT );
     if ( _Inform.format = VK_FORMAT_D16_UNORM_S8_UINT  )
     or ( _Inform.format = VK_FORMAT_D24_UNORM_S8_UINT  )
     or ( _Inform.format = VK_FORMAT_D32_SFLOAT_S8_UINT ) then
     view_info.subresourceRange.aspectMask     := view_info.subresourceRange.aspectMask or Ord( VK_IMAGE_ASPECT_STENCIL_BIT );
     view_info.subresourceRange.baseMipLevel   := 0;
     view_info.subresourceRange.levelCount     := 1;
     view_info.subresourceRange.baseArrayLayer := 0;
     view_info.subresourceRange.layerCount     := 1;

     (* Create image view *)
     Assert( vkCreateImageView( TVkDevice( Device ).Handle, @view_info, nil, @TVkDevice( Device ).Depthr.view ) = VK_SUCCESS );
end;

procedure TVkDepMem<TVkDevice_>.DestroHandle;
begin
     vkDestroyImageView( TVkDevice( Device ).Handle, TVkDevice( Device ).Depthr.view , nil );
     vkFreeMemory      ( TVkDevice( Device ).Handle, TVkDevice( Device ).Depthr.mem  , nil );
     vkDestroyImage    ( TVkDevice( Device ).Handle, _Handle, nil );
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TVkDepMem<TVkDevice_>.Create;
begin
     inherited Create;

     _Handle := 0;
end;

constructor TVkDepMem<TVkDevice_>.Create( const Device_:TVkDevice_ );
var
   P :VkFormatProperties;
begin
     Create;

     _Device := Device_;
end;

destructor TVkDepMem<TVkDevice_>.Destroy;
begin
      Handle := 0;

     inherited;
end;

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
   mem_reqs  :VkMemoryRequirements;
   mem_alloc :VkMemoryAllocateInfo;
   view_info :VkImageViewCreateInfo;
begin
     Assert( vkCreateImage( TVkDevice( Device ).Handle, @_Inform, nil, @_Handle ) = VK_SUCCESS );

     vkGetImageMemoryRequirements( TVkDevice( Device ).Handle, _Handle, @mem_reqs );

     mem_alloc.sType          := VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO;
     mem_alloc.pNext          := nil;
     mem_alloc.allocationSize := mem_reqs.size;
     Assert( TVkDevice( Device ).memory_type_from_properties( mem_reqs.memoryTypeBits, Ord( VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT ), mem_alloc.memoryTypeIndex ) );

     (* Allocate memory *)
     Assert( vkAllocateMemory( TVkDevice( Device ).Handle, @mem_alloc, nil, @TVkDevice( Device ).Depthr.mem ) = VK_SUCCESS );

     (* Bind memory *)
     Assert( vkBindImageMemory( TVkDevice( Device ).Handle, _Handle, TVkDevice( Device ).Depthr.mem, 0 ) = VK_SUCCESS );

     view_info.sType                           := VK_STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO;
     view_info.pNext                           := nil;
     view_info.flags                           := 0;
     view_info.image                           := _Handle;
     view_info.viewType                        := VK_IMAGE_VIEW_TYPE_2D;
     view_info.format                          := _Inform.format;
     view_info.components.r                    := VK_COMPONENT_SWIZZLE_R;
     view_info.components.g                    := VK_COMPONENT_SWIZZLE_G;
     view_info.components.b                    := VK_COMPONENT_SWIZZLE_B;
     view_info.components.a                    := VK_COMPONENT_SWIZZLE_A;
     view_info.subresourceRange.aspectMask     := Ord( VK_IMAGE_ASPECT_DEPTH_BIT );
     if ( _Inform.format = VK_FORMAT_D16_UNORM_S8_UINT  )
     or ( _Inform.format = VK_FORMAT_D24_UNORM_S8_UINT  )
     or ( _Inform.format = VK_FORMAT_D32_SFLOAT_S8_UINT ) then
     view_info.subresourceRange.aspectMask     := view_info.subresourceRange.aspectMask or Ord( VK_IMAGE_ASPECT_STENCIL_BIT );
     view_info.subresourceRange.baseMipLevel   := 0;
     view_info.subresourceRange.levelCount     := 1;
     view_info.subresourceRange.baseArrayLayer := 0;
     view_info.subresourceRange.layerCount     := 1;

     (* Create image view *)
     Assert( vkCreateImageView( TVkDevice( Device ).Handle, @view_info, nil, @TVkDevice( Device ).Depthr.view ) = VK_SUCCESS );
end;

procedure TVkDepthr<TVkDevice_>.DestroHandle;
begin
     vkDestroyImageView( TVkDevice( Device ).Handle, TVkDevice( Device ).Depthr.view , nil );
     vkFreeMemory      ( TVkDevice( Device ).Handle, TVkDevice( Device ).Depthr.mem  , nil );
     vkDestroyImage    ( TVkDevice( Device ).Handle, _Handle, nil );
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TVkDepthr<TVkDevice_>.Create;
begin
     inherited Create;

     _Handle := 0;
end;

constructor TVkDepthr<TVkDevice_>.Create( const Device_:TVkDevice_ );
var
   P :VkFormatProperties;
begin
     Create;

     _Device := Device_;

     TVkDevice( _Device ).Depthr := TVkDepthr( Self );

     with _Inform do
     begin
          sType                 := VK_STRUCTURE_TYPE_IMAGE_CREATE_INFO;
          pNext                 := nil;
          flags                 := 0;
          imageType             := VK_IMAGE_TYPE_2D;
          format                := VK_FORMAT_D16_UNORM;
          extent.width          := TVkDevice( Device ).Surfac.PxSizeX;
          extent.height         := TVkDevice( Device ).Surfac.PxSizeY;
          extent.depth          := 1;
          mipLevels             := 1;
          arrayLayers           := 1;
          samples               := NUM_SAMPLES;

          vkGetPhysicalDeviceFormatProperties( TVkDevice( Device ).Physic, format, @P );
          if ( P.linearTilingFeatures and Ord( VK_FORMAT_FEATURE_DEPTH_STENCIL_ATTACHMENT_BIT ) ) <> 0
          then tiling := VK_IMAGE_TILING_LINEAR
          else
          if ( P.optimalTilingFeatures and Ord( VK_FORMAT_FEATURE_DEPTH_STENCIL_ATTACHMENT_BIT ) ) <> 0
          then tiling := VK_IMAGE_TILING_OPTIMAL
          else
          begin
               (* Try other depth formats? *)
               Log.d( 'image_info.format ' + Ord( format ).ToString + ' Unsupported.' );
               RunError( 256-1 );
          end;

          usage                 := Ord( VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT );
          sharingMode           := VK_SHARING_MODE_EXCLUSIVE;
          queueFamilyIndexCount := 0;
          pQueueFamilyIndices   := nil;
          initialLayout         := VK_IMAGE_LAYOUT_UNDEFINED;
     end;

     CreateHandle;
end;

destructor TVkDepthr<TVkDevice_>.Destroy;
begin
      Handle := 0;

     inherited;
end;

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【ルーチン】

//############################################################################## □

initialization //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ 初期化

finalization //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ 最終化

end. //######################################################################### ■