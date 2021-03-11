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
       _Depthr :TVkDepthr_;
       _Inform :VkImageViewCreateInfo;
       _Handle :VkImageView;
       ///// アクセス
       function GetDevice :TVkDevice_;
       function GetHandle :VkImageView;
       procedure SetHandle( const Handle_:VkImageView );
       ///// メソッド
       procedure CreateHandle;
       procedure DestroHandle;
     public
       constructor Create; overload;
       constructor Create( const Depthr_:TVkDepthr_ ); overload;
       destructor Destroy; override;
       ///// プロパティ
       property Device :TVkDevice_            read GetDevice                ;
       property Depthr :TVkDepthr_            read   _Depthr                ;
       property Inform :VkImageViewCreateInfo read   _Inform                ;
       property Handle :VkImageView           read GetHandle write SetHandle;
     end;

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TVkDepMem

     TVkDepMem<TVkDevice_:class> = class
     private
       type TVkDepthr_ = TVkDepthr<TVkDevice_>;
     protected
       _Depthr :TVkDepthr_;
       _Inform :VkMemoryAllocateInfo;
       _Handle :VkDeviceMemory;
       ///// アクセス
       function GetDevice :TVkDevice_;
       function GetHandle :VkDeviceMemory;
       procedure SetHandle( const Handle_:VkDeviceMemory );
       ///// メソッド
       procedure CreateHandle;
       procedure DestroHandle;
     public
       constructor Create; overload;
       constructor Create( const Depthr_:TVkDepthr_ ); overload;
       destructor Destroy; override;
       ///// プロパティ
       property Device :TVkDevice_           read GetDevice                ;
       property Depthr :TVkDepthr_           read   _Depthr                ;
       property Inform :VkMemoryAllocateInfo read   _Inform                ;
       property Handle :VkDeviceMemory       read GetHandle write SetHandle;
     end;

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TVkDepthr

     TVkDepthr<TVkDevice_:class> = class
     private
       type TVkDepthr_ = TVkDepthr<TVkDevice_>;
            TVkDepMem_ = TVkDepMem<TVkDevice_>;
            TVkDepVie_ = TVkDepVie<TVkDevice_>;
     protected
       _Device :TVkDevice_;
       _Inform :VkImageCreateInfo;
       _Handle :VkImage;
       _Memory :TVkDepMem_;
       _Viewer :TVkDepVie_;
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
       property Device :TVkDevice_        read   _Device                ;
       property Inform :VkImageCreateInfo read   _Inform                ;
       property Handle :VkImage           read GetHandle write SetHandle;
       property Memory :TVkDepMem_        read   _Memory                ;
       property Viewer :TVkDepVie_        read   _Viewer                ;
       ///// メソッド
       function GetRequir :VkMemoryRequirements;
       function Bind( const Memory_:TVkDepMem_ ) :Boolean;
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

function TVkDepVie<TVkDevice_>.GetDevice :TVkDevice_;
begin
     Result := _Depthr.Device;
end;

//------------------------------------------------------------------------------

function TVkDepVie<TVkDevice_>.GetHandle :VkImageView;
begin
     if _Handle = 0 then CreateHandle;

     Result := _Handle;
end;

procedure TVkDepVie<TVkDevice_>.SetHandle( const Handle_:VkImageView );
begin
     if _Handle <> 0 then DestroHandle;

     _Handle := Handle_;
end;

/////////////////////////////////////////////////////////////////////// メソッド

procedure TVkDepVie<TVkDevice_>.CreateHandle;
var
   D :TVkDepthr;
   F :VkFormat;
begin
     D := TVkDepthr( Depthr );
     F := D.Inform.format;

     with _Inform do
     begin
          sType    := VK_STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO;
          pNext    := nil;
          flags    := 0;
          image    := D.Handle;
          viewType := VK_IMAGE_VIEW_TYPE_2D;
          format   := F;
          with components do
          begin
               r := VK_COMPONENT_SWIZZLE_R;
               g := VK_COMPONENT_SWIZZLE_G;
               b := VK_COMPONENT_SWIZZLE_B;
               a := VK_COMPONENT_SWIZZLE_A;
          end;
          with subresourceRange do
          begin
               aspectMask     := Ord( VK_IMAGE_ASPECT_DEPTH_BIT );
               if ( F = VK_FORMAT_D16_UNORM_S8_UINT  )
               or ( F = VK_FORMAT_D24_UNORM_S8_UINT  )
               or ( F = VK_FORMAT_D32_SFLOAT_S8_UINT ) then
               aspectMask     := aspectMask or Ord( VK_IMAGE_ASPECT_STENCIL_BIT );
               baseMipLevel   := 0;
               levelCount     := 1;
               baseArrayLayer := 0;
               layerCount     := 1;
          end;
     end;

     Assert( vkCreateImageView( TVkDevice( Device ).Handle, @_Inform, nil, @_Handle ) = VK_SUCCESS );
end;

procedure TVkDepVie<TVkDevice_>.DestroHandle;
begin
     vkDestroyImageView( TVkDevice( Device ).Handle, _Handle, nil );
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TVkDepVie<TVkDevice_>.Create;
begin
     inherited Create;

     _Handle := 0;
end;

constructor TVkDepVie<TVkDevice_>.Create( const Depthr_:TVkDepthr_ );
begin
     Create;

     _Depthr := Depthr_;
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

function TVkDepMem<TVkDevice_>.GetDevice :TVkDevice_;
begin
     Result := _Depthr.Device;
end;

//------------------------------------------------------------------------------

function TVkDepMem<TVkDevice_>.GetHandle :VkDeviceMemory;
begin
     if _Handle = 0 then CreateHandle;

     Result := _Handle;
end;

procedure TVkDepMem<TVkDevice_>.SetHandle( const Handle_:VkDeviceMemory );
begin
     if _Handle <> 0 then DestroHandle;

     _Handle := Handle_;
end;

/////////////////////////////////////////////////////////////////////// メソッド

procedure TVkDepMem<TVkDevice_>.CreateHandle;
var
   R :VkMemoryRequirements;
begin
     R := TVkDepthr( Depthr ).GetRequir;

     with _Inform do
     begin
          sType          := VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO;
          pNext          := nil;
          allocationSize := R.size;
          Assert( TVkDevice( Device ).memory_type_from_properties( R.memoryTypeBits, Ord( VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT ), memoryTypeIndex ) );
     end;

     Assert( vkAllocateMemory( TVkDevice( Device ).Handle, @_Inform, nil, @_Handle ) = VK_SUCCESS );
end;

procedure TVkDepMem<TVkDevice_>.DestroHandle;
begin
     vkFreeMemory( TVkDevice( Device ).Handle, _Handle, nil );
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TVkDepMem<TVkDevice_>.Create;
begin
     inherited Create;

     _Handle := 0;
end;

constructor TVkDepMem<TVkDevice_>.Create( const Depthr_:TVkDepthr_ );
begin
     Create;

     _Depthr := Depthr_;
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
     if _Handle = 0 then
     begin
          CreateHandle;

          Bind( _Memory );
     end;

     Result := _Handle;
end;

procedure TVkDepthr<TVkDevice_>.SetHandle( const Handle_:VkImage );
begin
     if _Handle <> 0 then DestroHandle;

     _Handle := Handle_;
end;

/////////////////////////////////////////////////////////////////////// メソッド

procedure TVkDepthr<TVkDevice_>.CreateHandle;
begin
     Assert( vkCreateImage( TVkDevice( Device ).Handle, @_Inform, nil, @_Handle ) = VK_SUCCESS );
end;

procedure TVkDepthr<TVkDevice_>.DestroHandle;
begin
     vkDestroyImage( TVkDevice( Device ).Handle, _Handle, nil );
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TVkDepthr<TVkDevice_>.Create;
begin
     inherited Create;

     _Handle := 0;

     _Memory := TVkDepMem_.Create( Self );
     _Viewer := TVkDepVie_.Create( Self );
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
end;

destructor TVkDepthr<TVkDevice_>.Destroy;
begin
     _Viewer.Free;
     _Memory.Free;

      Handle := 0;

     inherited;
end;

/////////////////////////////////////////////////////////////////////// メソッド

function TVkDepthr<TVkDevice_>.GetRequir :VkMemoryRequirements;
begin
     vkGetImageMemoryRequirements( TVkDevice( Device ).Handle, Handle, @Result );
end;

function TVkDepthr<TVkDevice_>.Bind( const Memory_:TVkDepMem_ ) :Boolean;
begin
     Result := vkBindImageMemory( TVkDevice( Device ).Handle, Handle, Memory_.Handle, 0 ) = VK_SUCCESS;
end;

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【ルーチン】

//############################################################################## □

initialization //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ 初期化

finalization //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ 最終化

end. //######################################################################### ■