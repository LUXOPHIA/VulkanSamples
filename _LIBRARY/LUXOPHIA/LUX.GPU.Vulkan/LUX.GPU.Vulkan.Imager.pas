unit LUX.GPU.Vulkan.Imager;

interface //#################################################################### ■

uses System.Generics.Collections,
     vulkan_core,
     vulkan.util;

type //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【型】

     TVkImager<TVkDevice_,TParent_:class>   = class;
       TVkViewer<TVkDevice_,TParent_:class> = class;

     //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【レコード】

     //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【クラス】

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TVkViewer<TVkDevice,TParent>

     TVkViewer<TVkDevice_,TParent_:class> = class
     private
       type TVkImager_ = TVkImager<TVkDevice_,TParent_>;
     protected
       _Imager :TVkImager_;
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
       constructor Create( const Parent_:TVkImager_ ); overload;
       destructor Destroy; override;
       ///// プロパティ
       property Device :TVkDevice_            read GetDevice;
       property Imager :TVkImager_            read   _Imager;
       property Inform :VkImageViewCreateInfo read   _Inform;
       property Handle :VkImageView           read GetHandle write SetHandle;
     end;

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TVkImager<TVkDevice,TParent>

     TVkImager<TVkDevice_,TParent_:class> = class
     private
       type TVkViewer_ = TVkViewer<TVkDevice_,TParent_>;
       ///// メソッド
       procedure init_buffer;
     protected
       _Parent  :TParent_;
       _Inform  :VkImageCreateInfo;
       _Handle  :VkImage;
       _MemoryInform :VkMemoryAllocateInfo;
       _MemoryHandle :VkDeviceMemory;

       _Viewer  :TVkViewer_;

       _needs_staging :Boolean;

       _BufferInform       :VkBufferCreateInfo;
       _BufferHandle       :VkBuffer;
       _BufferMemoryInform :VkMemoryAllocateInfo;
       _BufferMemoryHandle :VkDeviceMemory;

        extraUsages   :VkImageUsageFlags;
        extraFeatures :VkFormatFeatureFlags;
       ///// アクセス
       function GetDevice :TVkDevice_; virtual; abstract;
       function GetPixelsW :UInt32;
       procedure SetPixelsW( const PixelsW_:UInt32 );
       function GetPixelsH :UInt32;
       procedure SetPixelsH( const PixelsH_:UInt32 );
       function GetHandle :VkImage;
       procedure SetHandle( const Handle_:VkImage );
       ///// メソッド
       procedure CreateHandle;
       procedure DestroHandle;
     public
       constructor Create; overload;
       constructor Create( const Parent_:TParent_ ); overload;
       destructor Destroy; override;
       ///// プロパティ
       property Device  :TVkDevice_        read GetDevice ;
       property Parent  :TParent_          read   _Parent ;
       property Inform  :VkImageCreateInfo read   _Inform ;
       property PixelsW :UInt32            read GetPixelsW write SetPixelsW;
       property PixelsH :UInt32            read GetPixelsH write SetPixelsH;
       property Handle  :VkImage           read GetHandle  write SetHandle;
       property Viewer  :TVkViewer_        read   _Viewer ;
       ///// メソッド
       procedure LoadFromFile( const FileName_:String );
     end;

//const //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【定数】

//var //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【変数】

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【ルーチン】

implementation //############################################################### ■

uses FMX.Types,
     vulkan.util_init,
     LUX.GPU.Vulkan;

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【レコード】

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【クラス】

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TVkViewer<TVkDevice,TParent>

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

/////////////////////////////////////////////////////////////////////// アクセス

function TVkViewer<TVkDevice_,TParent_>.GetDevice :TVkDevice_;
begin
     Result := _Imager.Device;
end;

//------------------------------------------------------------------------------

function TVkViewer<TVkDevice_,TParent_>.GetHandle :VkImageView;
begin
     if _Handle = VK_NULL_HANDLE then CreateHandle;

     Result := _Handle;
end;

procedure TVkViewer<TVkDevice_,TParent_>.SetHandle( const Handle_:VkImageView );
begin
     if _Handle <> VK_NULL_HANDLE then DestroHandle;

     _Handle := Handle_;
end;

/////////////////////////////////////////////////////////////////////// メソッド

procedure TVkViewer<TVkDevice_,TParent_>.CreateHandle;
begin
     with _Inform do
     begin
          sType    := VK_STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO;
          pNext    := nil;
          flags    := 0;
          image    := _Imager.Handle;
          viewType := VK_IMAGE_VIEW_TYPE_2D;
          format   := VK_FORMAT_R8G8B8A8_UNORM;
          with components do
          begin
               r := VK_COMPONENT_SWIZZLE_R;
               g := VK_COMPONENT_SWIZZLE_G;
               b := VK_COMPONENT_SWIZZLE_B;
               a := VK_COMPONENT_SWIZZLE_A;
          end;
          with subresourceRange do
          begin
               aspectMask     := Ord( VK_IMAGE_ASPECT_COLOR_BIT );
               baseMipLevel   := 0;
               levelCount     := 1;
               baseArrayLayer := 0;
               layerCount     := 1;
          end;
     end;

     Assert( vkCreateImageView( TVkDevice( Device ).Handle, @_Inform, nil, @_Handle ) = VK_SUCCESS );
end;

procedure TVkViewer<TVkDevice_,TParent_>.DestroHandle;
begin
     vkDestroyImageView( TVkDevice( Device ).Handle, _Handle, nil );
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TVkViewer<TVkDevice_,TParent_>.Create;
begin
     inherited;

     _Handle := VK_NULL_HANDLE;
end;

constructor TVkViewer<TVkDevice_,TParent_>.Create( const Parent_:TVkImager_ );
begin
     Create;

     _Imager := Parent_;
end;

destructor TVkViewer<TVkDevice_,TParent_>.Destroy;
begin
      Handle := VK_NULL_HANDLE;

     inherited;
end;

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TVkImager<TVkDevice,TParent>

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

procedure TVkImager<TVkDevice_,TParent_>.init_buffer;
var
   mem_reqs           :VkMemoryRequirements;
   requirements       :VkFlags;
begin
     _BufferInform                       := Default( VkBufferCreateInfo );
     _BufferInform.sType                 := VK_STRUCTURE_TYPE_BUFFER_CREATE_INFO;
     _BufferInform.pNext                 := nil;
     _BufferInform.flags                 := 0;
     _BufferInform.size                  := PixelsW * PixelsH * 4;
     _BufferInform.usage                 := Ord( VK_BUFFER_USAGE_TRANSFER_SRC_BIT );
     _BufferInform.sharingMode           := VK_SHARING_MODE_EXCLUSIVE;
     _BufferInform.queueFamilyIndexCount := 0;
     _BufferInform.pQueueFamilyIndices   := nil;
     Assert( vkCreateBuffer( TVkDevice( Device ).Handle, @_BufferInform, nil, @_BufferHandle ) = VK_SUCCESS );

     vkGetBufferMemoryRequirements( TVkDevice( Device ).Handle, _BufferHandle, @mem_reqs );

     _BufferMemoryInform                 := Default( VkMemoryAllocateInfo );
     _BufferMemoryInform.sType           := VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO;
     _BufferMemoryInform.pNext           := nil;
     _BufferMemoryInform.allocationSize  := 0;
     _BufferMemoryInform.memoryTypeIndex := 0;
     _BufferMemoryInform.allocationSize := mem_reqs.size;

     requirements := Ord( VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT ) or Ord( VK_MEMORY_PROPERTY_HOST_COHERENT_BIT );
     Assert( TVkDevice( Device ).memory_type_from_properties( mem_reqs.memoryTypeBits, requirements, _BufferMemoryInform.memoryTypeIndex ), '"No mappable, coherent memory' );

     (* allocate memory *)
     Assert( vkAllocateMemory(TVkDevice( Device ).Handle, @_BufferMemoryInform, nil, @_BufferMemoryHandle ) = VK_SUCCESS );

     (* bind memory *)
     Assert( vkBindBufferMemory( TVkDevice( Device ).Handle, _BufferHandle, _BufferMemoryHandle, 0 ) = VK_SUCCESS );
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

/////////////////////////////////////////////////////////////////////// アクセス

function TVkImager<TVkDevice_,TParent_>.GetPixelsW :UInt32;
begin
     Result := _Inform.extent.width;
end;

procedure TVkImager<TVkDevice_,TParent_>.SetPixelsW( const PixelsW_:UInt32 );
begin
     _Inform.extent.width := PixelsW_;

     Handle := VK_NULL_HANDLE;
end;

function TVkImager<TVkDevice_,TParent_>.GetPixelsH :UInt32;
begin
     Result := _Inform.extent.height;
end;

procedure TVkImager<TVkDevice_,TParent_>.SetPixelsH( const PixelsH_:UInt32 );
begin
     _Inform.extent.height := PixelsH_;

     Handle := VK_NULL_HANDLE;
end;

//------------------------------------------------------------------------------

function TVkImager<TVkDevice_,TParent_>.GetHandle :VkImage;
begin
     if _Handle = VK_NULL_HANDLE then CreateHandle;

     Result := _Handle;
end;

procedure TVkImager<TVkDevice_,TParent_>.SetHandle( const Handle_:VkImage );
begin
     if _Handle <> VK_NULL_HANDLE then DestroHandle;

     _Handle := Handle_;
end;

/////////////////////////////////////////////////////////////////////// メソッド

procedure TVkImager<TVkDevice_,TParent_>.CreateHandle;
var
   formatProps  :VkFormatProperties;
   allFeatures  :VkFormatFeatureFlags;
   mem_reqs     :VkMemoryRequirements;
   requirements :VkFlags;
begin
     vkGetPhysicalDeviceFormatProperties( TVkDevice( Device ).Physic, VK_FORMAT_R8G8B8A8_UNORM, @formatProps );

     (* See if we can use a linear tiled image for a texture, if not, we will
      * need a staging buffer for the texture data *)
     allFeatures := Ord( VK_FORMAT_FEATURE_SAMPLED_IMAGE_BIT ) or extraFeatures;
     _needs_staging := ( ( formatProps.linearTilingFeatures and allFeatures ) <> allFeatures );

     //////////

     if _needs_staging then
     begin
          Assert( ( formatProps.optimalTilingFeatures and allFeatures ) = allFeatures );
          init_buffer;
          extraUsages := extraUsages or Ord( VK_IMAGE_USAGE_TRANSFER_DST_BIT );
     end
     else
     begin
          _BufferHandle       := VK_NULL_HANDLE;
          _BufferMemoryHandle := VK_NULL_HANDLE;
     end;

     //////////

     with _Inform do
     begin
          sType                 := VK_STRUCTURE_TYPE_IMAGE_CREATE_INFO;
          pNext                 := nil;
          flags                 := 0;
          imageType             := VK_IMAGE_TYPE_2D;
          format                := VK_FORMAT_R8G8B8A8_UNORM;
          with extent do
          begin
            // width             = PixelsW;
            // height            = PixelsH;
               depth            := 1;
          end;
          mipLevels             := 1;
          arrayLayers           := 1;
          samples               := NUM_SAMPLES;
          if _needs_staging
          then tiling           := VK_IMAGE_TILING_OPTIMAL
          else tiling           := VK_IMAGE_TILING_LINEAR;
          usage                 := Ord( VK_IMAGE_USAGE_SAMPLED_BIT ) or extraUsages;
          sharingMode           := VK_SHARING_MODE_EXCLUSIVE;
          queueFamilyIndexCount := 0;
          pQueueFamilyIndices   := nil;
          if _needs_staging
          then initialLayout    := VK_IMAGE_LAYOUT_UNDEFINED
          else initialLayout    := VK_IMAGE_LAYOUT_PREINITIALIZED;
     end;

     Assert( vkCreateImage( TVkDevice( Device ).Handle, @_Inform, nil, @_Handle ) = VK_SUCCESS );

     //////////

     vkGetImageMemoryRequirements( TVkDevice( Device ).Handle, _Handle, @mem_reqs );

     _MemoryInform                 := Default( VkMemoryAllocateInfo );
     _MemoryInform.sType           := VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO;
     _MemoryInform.pNext           := nil;
     _MemoryInform.allocationSize  := 0;
     _MemoryInform.memoryTypeIndex := 0;
     _MemoryInform.allocationSize  := mem_reqs.size;
     if _needs_staging
     then requirements := 0
     else requirements := Ord( VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT ) or Ord( VK_MEMORY_PROPERTY_HOST_COHERENT_BIT );
     Assert( TVkDevice( Device ).memory_type_from_properties( mem_reqs.memoryTypeBits, requirements, _MemoryInform.memoryTypeIndex ) );

     (* allocate memory *)
     Assert( vkAllocateMemory(TVkDevice( Device ).Handle, @_MemoryInform, nil, @( _MemoryHandle) ) = VK_SUCCESS );

     //////////

     (* bind memory *)
     Assert( vkBindImageMemory( TVkDevice( Device ).Handle, _Handle, _MemoryHandle, 0 ) = VK_SUCCESS );
end;

procedure TVkImager<TVkDevice_,TParent_>.DestroHandle;
begin
     vkDestroyImage ( TVkDevice( Device ).Handle, _Handle            , nil );
     vkFreeMemory   ( TVkDevice( Device ).Handle, _MemoryHandle      , nil );
     vkDestroyBuffer( TVkDevice( Device ).Handle, _BufferHandle      , nil );
     vkFreeMemory   ( TVkDevice( Device ).Handle, _BufferMemoryHandle, nil );
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TVkImager<TVkDevice_,TParent_>.Create;
begin
     inherited;

     _Handle := VK_NULL_HANDLE;

     _Viewer := TVkViewer_.Create( Self );

     extraUsages   := 0;
     extraFeatures := 0;
end;

constructor TVkImager<TVkDevice_,TParent_>.Create( const Parent_:TParent_ );
begin
     Create;

     _Parent := Parent_;
end;

destructor TVkImager<TVkDevice_,TParent_>.Destroy;
begin
     _Viewer.Free;

      Handle := VK_NULL_HANDLE;

     inherited;
end;

/////////////////////////////////////////////////////////////////////// メソッド

procedure TVkImager<TVkDevice_,TParent_>.LoadFromFile( const FileName_:String );
var
   PsW          :Int32;
   PsH          :Int32;
   res          :VkResult;
   pass         :Boolean;
   cmd_bufs     :array [ 0..1-1 ] of VkCommandBuffer;
   fenceInfo    :VkFenceCreateInfo;
   cmdFence     :VkFence;
   submit_info  :array [ 0..1-1 ] of VkSubmitInfo;
   subres       :VkImageSubresource;
   layout       :VkSubresourceLayout;
   data         :Pointer;
   rowPitch     :UInt64;
   copy_region  :VkBufferImageCopy;
   _imageLayout :VkImageLayout;
begin
     if not read_ppm( FileName_, PsW, PsH, 0, nil ) then
     begin
          Log.d( 'Could not read texture file ' + FileName_ );
          RunError( 256-1 );
     end;

     PixelsW := PsW;
     PixelsH := PsH;

     //////////

     TVkDevice( Device ).Poolers[0].Commans[0].EndRecord;
     cmd_bufs[0] := TVkDevice( Device ).Poolers[0].Commans[0].Handle;
     fenceInfo.sType := VK_STRUCTURE_TYPE_FENCE_CREATE_INFO;
     fenceInfo.pNext := nil;
     fenceInfo.flags := 0;
     vkCreateFence( TVkDevice( Device ).Handle, @fenceInfo, nil, @cmdFence );

     submit_info[0]                      := Default( VkSubmitInfo );
     submit_info[0].pNext                := nil;
     submit_info[0].sType                := VK_STRUCTURE_TYPE_SUBMIT_INFO;
     submit_info[0].waitSemaphoreCount   := 0;
     submit_info[0].pWaitSemaphores      := nil;
     submit_info[0].pWaitDstStageMask    := nil;
     submit_info[0].commandBufferCount   := 1;
     submit_info[0].pCommandBuffers      := @cmd_bufs[0];
     submit_info[0].signalSemaphoreCount := 0;
     submit_info[0].pSignalSemaphores    := nil;

     (* Queue the command buffer for execution *)
     Assert( vkQueueSubmit( TVkDevice( Device ).QueuerG, 1, @submit_info[0], cmdFence ) = VK_SUCCESS );

     subres            := Default( VkImageSubresource );
     subres.aspectMask := Ord( VK_IMAGE_ASPECT_COLOR_BIT );
     subres.mipLevel   := 0;
     subres.arrayLayer := 0;

     layout := Default( VkSubresourceLayout );
     if not _needs_staging then
     begin
          (* Get the subresource layout so we know what the row pitch is *)
          vkGetImageSubresourceLayout( TVkDevice( Device ).Handle, Handle, @subres, @layout );
     end;

     (* Make sure command buffer is finished before mapping *)
     repeat
           res := vkWaitForFences( TVkDevice( Device ).Handle, 1, @cmdFence, VK_TRUE, FENCE_TIMEOUT );

     until res <> VK_TIMEOUT;
     Assert( res = VK_SUCCESS );

     vkDestroyFence( TVkDevice( Device ).Handle, cmdFence, nil );

     //////////

     if _needs_staging
     then res := vkMapMemory( TVkDevice( Device ).Handle, _BufferMemoryHandle, 0, _BufferMemoryInform.allocationSize, 0, @data )
     else res := vkMapMemory( TVkDevice( Device ).Handle, _MemoryHandle , 0, _MemoryInform.allocationSize, 0, @data );
     Assert( res = VK_SUCCESS );

     (* Read the ppm file into the mappable image's memory *)
     if _needs_staging then rowPitch := PixelsW * 4
                       else rowPitch := layout.rowPitch;
     if not read_ppm( FileName_, PsW, PsH, rowPitch, data ) then
     begin
          Log.d( 'Could not load texture file lunarg.ppm' );
          RunError( 256-1 );
     end;

     if _needs_staging
     then vkUnmapMemory( TVkDevice( Device ).Handle, _BufferMemoryHandle )
     else vkUnmapMemory( TVkDevice( Device ).Handle, _MemoryHandle );

     //////////

     Assert( vkResetCommandBuffer( TVkDevice( Device ).Poolers[0].Commans[0].Handle, 0 ) = VK_SUCCESS );
     TVkDevice( Device ).Poolers[0].Commans[0].BeginRecord;

     if not _needs_staging then
     begin
          (* If we can use the linear tiled image as a texture, just do it *)
          _imageLayout := VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL;
          set_image_layout( TVkDevice( Device ).Instan.Vulkan, Handle, Ord( VK_IMAGE_ASPECT_COLOR_BIT ), VK_IMAGE_LAYOUT_PREINITIALIZED, _imageLayout,
                            Ord( VK_PIPELINE_STAGE_HOST_BIT ), Ord( VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT ) );
     end
     else
     begin
          (* Since we're going to blit to the texture image, set its layout to
           * DESTINATION_OPTIMAL *)
          set_image_layout( TVkDevice( Device ).Instan.Vulkan, Handle, Ord( VK_IMAGE_ASPECT_COLOR_BIT ), VK_IMAGE_LAYOUT_UNDEFINED,
                            VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL, Ord( VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT ), Ord( VK_PIPELINE_STAGE_TRANSFER_BIT ) );

          copy_region.bufferOffset                    := 0;
          copy_region.bufferRowLength                 := PixelsW;
          copy_region.bufferImageHeight               := PixelsH;
          copy_region.imageSubresource.aspectMask     := Ord( VK_IMAGE_ASPECT_COLOR_BIT );
          copy_region.imageSubresource.mipLevel       := 0;
          copy_region.imageSubresource.baseArrayLayer := 0;
          copy_region.imageSubresource.layerCount     := 1;
          copy_region.imageOffset.x                   := 0;
          copy_region.imageOffset.y                   := 0;
          copy_region.imageOffset.z                   := 0;
          copy_region.imageExtent.width               := PixelsW;
          copy_region.imageExtent.height              := PixelsH;
          copy_region.imageExtent.depth               := 1;

          (* Put the copy command into the command buffer *)
          vkCmdCopyBufferToImage( TVkDevice( Device ).Poolers[0].Commans[0].Handle, _BufferHandle, Handle, VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL, 1, @copy_region );

          (* Set the layout for the texture image from DESTINATION_OPTIMAL to
           * SHADER_READ_ONLY *)
          _imageLayout := VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL;
          set_image_layout( TVkDevice( Device ).Instan.Vulkan, Handle, Ord( VK_IMAGE_ASPECT_COLOR_BIT ), VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL, _imageLayout,
                            Ord( VK_PIPELINE_STAGE_TRANSFER_BIT ), Ord( VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT ) );
     end;
end;

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【ルーチン】

//############################################################################## □

initialization //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ 初期化

finalization //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ 最終化

end. //######################################################################### ■