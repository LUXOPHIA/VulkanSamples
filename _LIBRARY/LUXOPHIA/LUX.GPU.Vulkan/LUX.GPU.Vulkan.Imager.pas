unit LUX.GPU.Vulkan.Imager;

interface //#################################################################### ■

uses System.Generics.Collections,
     vulkan_core,
     vulkan.util,
     LUX.GPU.Vulkan.root,
     LUX.GPU.Vulkan.Buffer;

type //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【型】

     TVkImager<TVkDevice_,TVkParent_:class>   = class;
       TVkViewer<TVkDevice_,TVkParent_:class> = class;
       TVkImaBuf<TVkDevice_,TVkParent_:class> = class;

     //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【レコード】

     //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【クラス】

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TVkImaBuf<TVkDevice_,TVkParent_>

     TVkImaBuf<TVkDevice_,TVkParent_:class> = class( TVkBuffer<TVkDevice_,TVkImager<TVkDevice_,TVkParent_>> )
     private
       type TVkImager_ = TVkImager<TVkDevice_,TVkParent_>;
     protected
       ///// アクセス
       function GetDevice :TVkDevice_; override;
     public
       constructor Create; override;
       ///// プロパティ
       property Imager :TVkImager_ read GetParent;
     end;

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TVkViewer<TVkDevice_,TVkParent_>

     TVkViewer<TVkDevice_,TVkParent_:class> = class( TVkDeviceObject<TVkDevice_,TVkImager<TVkDevice_,TVkParent_>> )
     private
       type TVkImager_ = TVkImager<TVkDevice_,TVkParent_>;
     protected
       _Inform :VkImageViewCreateInfo;
       _Handle :VkImageView;
       ///// アクセス
       function GetDevice :TVkDevice_; override;
       function GetHandle :VkImageView;
       procedure SetHandle( const Handle_:VkImageView );
       ///// メソッド
       procedure CreateHandle;
       procedure DestroHandle;
     public
       constructor Create; override;
       destructor Destroy; override;
       ///// プロパティ
       property Imager :TVkImager_            read GetParent;
       property Inform :VkImageViewCreateInfo read   _Inform;
       property Handle :VkImageView           read GetHandle write SetHandle;
     end;

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TVkImager<TVkDevice_,TVkParent_>

     TVkImager<TVkDevice_,TVkParent_:class> = class( TVkDeviceObject<TVkDevice_,TVkParent_> )
     private
       type TVkViewer_ = TVkViewer<TVkDevice_,TVkParent_>;
            TVkImaBuf_ = TVkImaBuf<TVkDevice_,TVkParent_>;
       ///// メソッド
       procedure InitBufferSize;
     protected
       _Usagers :VkImageUsageFlags;
       _Featurs :VkFormatFeatureFlags;
       _Inform  :VkImageCreateInfo;
       _Handle  :VkImage;
       _MemoryInform :VkMemoryAllocateInfo;
       _MemoryHandle :VkDeviceMemory;
       _Viewer  :TVkViewer_;
       _Buffer :TVkImaBuf_;
       ///// アクセス
       function GetStaging :Boolean;
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
       constructor Create; override;
       destructor Destroy; override;
       ///// プロパティ
       property Usagers :VkImageUsageFlags    read   _Usagers write   _Usagers;
       property Featurs :VkFormatFeatureFlags read   _Featurs write   _Featurs;
       property Staging :Boolean              read GetStaging;
       property Inform  :VkImageCreateInfo    read   _Inform ;
       property PixelsW :UInt32               read GetPixelsW write SetPixelsW;
       property PixelsH :UInt32               read GetPixelsH write SetPixelsH;
       property Handle  :VkImage              read GetHandle  write SetHandle ;
       property Viewer  :TVkViewer_           read   _Viewer ;
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

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TVkImaBuf<TVkDevice_,TVkParent_>

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

/////////////////////////////////////////////////////////////////////// アクセス

function TVkImaBuf<TVkDevice_,TVkParent_>.GetDevice :TVkDevice_;
begin
     Result := Imager.Device;
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TVkImaBuf<TVkDevice_,TVkParent_>.Create;
begin
     inherited;

     Usage := Ord( VK_BUFFER_USAGE_TRANSFER_SRC_BIT );
end;

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TVkViewer<TVkDevice_,TVkParent_>

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

/////////////////////////////////////////////////////////////////////// アクセス

function TVkViewer<TVkDevice_,TVkParent_>.GetDevice :TVkDevice_;
begin
     Result := Imager.Device;
end;

//------------------------------------------------------------------------------

function TVkViewer<TVkDevice_,TVkParent_>.GetHandle :VkImageView;
begin
     if _Handle = VK_NULL_HANDLE then CreateHandle;

     Result := _Handle;
end;

procedure TVkViewer<TVkDevice_,TVkParent_>.SetHandle( const Handle_:VkImageView );
begin
     if _Handle <> VK_NULL_HANDLE then DestroHandle;

     _Handle := Handle_;
end;

/////////////////////////////////////////////////////////////////////// メソッド

procedure TVkViewer<TVkDevice_,TVkParent_>.CreateHandle;
begin
     with _Inform do
     begin
          sType    := VK_STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO;
          pNext    := nil;
          flags    := 0;
          image    := Imager.Handle;
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

procedure TVkViewer<TVkDevice_,TVkParent_>.DestroHandle;
begin
     vkDestroyImageView( TVkDevice( Device ).Handle, _Handle, nil );
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TVkViewer<TVkDevice_,TVkParent_>.Create;
begin
     inherited;

     _Handle := VK_NULL_HANDLE;
end;

destructor TVkViewer<TVkDevice_,TVkParent_>.Destroy;
begin
      Handle := VK_NULL_HANDLE;

     inherited;
end;

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TVkImager<TVkDevice_,TVkParent_>

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

/////////////////////////////////////////////////////////////////////// メソッド

procedure TVkImager<TVkDevice_,TVkParent_>.InitBufferSize;
begin
     _Buffer.Size := 4{Byte} * PixelsH * PixelsW;
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

/////////////////////////////////////////////////////////////////////// アクセス

function TVkImager<TVkDevice_,TVkParent_>.GetStaging :Boolean;
var
   Ps :VkFormatProperties;
   Fs :VkFormatFeatureFlags;
begin
     vkGetPhysicalDeviceFormatProperties( TVkDevice( Device ).Physic, VK_FORMAT_R8G8B8A8_UNORM, @Ps );

     (* See if we can use a linear tiled image for a texture, if not, we will
      * need a staging buffer for the texture data *)
     Fs := Ord( VK_FORMAT_FEATURE_SAMPLED_IMAGE_BIT ) or Featurs;

     Result := ( Ps.linearTilingFeatures and Fs ) <> Fs;

     if Result then Assert( ( Ps.optimalTilingFeatures and Fs ) = Fs );
end;

//------------------------------------------------------------------------------

function TVkImager<TVkDevice_,TVkParent_>.GetPixelsW :UInt32;
begin
     Result := _Inform.extent.width;
end;

procedure TVkImager<TVkDevice_,TVkParent_>.SetPixelsW( const PixelsW_:UInt32 );
begin
     _Inform.extent.width := PixelsW_;

     Handle := VK_NULL_HANDLE;

     InitBufferSize;
end;

function TVkImager<TVkDevice_,TVkParent_>.GetPixelsH :UInt32;
begin
     Result := _Inform.extent.height;
end;

procedure TVkImager<TVkDevice_,TVkParent_>.SetPixelsH( const PixelsH_:UInt32 );
begin
     _Inform.extent.height := PixelsH_;

     Handle := VK_NULL_HANDLE;

     InitBufferSize;
end;

//------------------------------------------------------------------------------

function TVkImager<TVkDevice_,TVkParent_>.GetHandle :VkImage;
begin
     if _Handle = VK_NULL_HANDLE then CreateHandle;

     Result := _Handle;
end;

procedure TVkImager<TVkDevice_,TVkParent_>.SetHandle( const Handle_:VkImage );
begin
     if _Handle <> VK_NULL_HANDLE then DestroHandle;

     _Handle := Handle_;
end;

/////////////////////////////////////////////////////////////////////// メソッド

procedure TVkImager<TVkDevice_,TVkParent_>.CreateHandle;
var
   mem_reqs     :VkMemoryRequirements;
   requirements :VkFlags;
begin
     if Staging then Usagers := Usagers or Ord( VK_IMAGE_USAGE_TRANSFER_DST_BIT );

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
          if Staging
          then tiling           := VK_IMAGE_TILING_OPTIMAL
          else tiling           := VK_IMAGE_TILING_LINEAR;
          usage                 := Ord( VK_IMAGE_USAGE_SAMPLED_BIT ) or Usagers;
          sharingMode           := VK_SHARING_MODE_EXCLUSIVE;
          queueFamilyIndexCount := 0;
          pQueueFamilyIndices   := nil;
          if Staging
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
     if Staging
     then requirements := 0
     else requirements := Ord( VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT ) or Ord( VK_MEMORY_PROPERTY_HOST_COHERENT_BIT );
     Assert( TVkDevice( Device ).memory_type_from_properties( mem_reqs.memoryTypeBits, requirements, _MemoryInform.memoryTypeIndex ) );

     (* allocate memory *)
     Assert( vkAllocateMemory(TVkDevice( Device ).Handle, @_MemoryInform, nil, @( _MemoryHandle) ) = VK_SUCCESS );

     //////////

     (* bind memory *)
     Assert( vkBindImageMemory( TVkDevice( Device ).Handle, _Handle, _MemoryHandle, 0 ) = VK_SUCCESS );
end;

procedure TVkImager<TVkDevice_,TVkParent_>.DestroHandle;
begin
     vkDestroyImage ( TVkDevice( Device ).Handle, _Handle            , nil );
     vkFreeMemory   ( TVkDevice( Device ).Handle, _MemoryHandle      , nil );
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TVkImager<TVkDevice_,TVkParent_>.Create;
begin
     inherited;

     _Handle := VK_NULL_HANDLE;

     _Viewer := TVkViewer_.Create( Self );
     _Buffer := TVkImaBuf_.Create( Self );

     Usagers := 0;
     Featurs := 0;
end;

destructor TVkImager<TVkDevice_,TVkParent_>.Destroy;
begin
     _Buffer.Free;
     _Viewer.Free;

      Handle := VK_NULL_HANDLE;

     inherited;
end;

/////////////////////////////////////////////////////////////////////// メソッド

procedure TVkImager<TVkDevice_,TVkParent_>.LoadFromFile( const FileName_:String );
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
     if not Staging then
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

     if Staging
     then res := vkMapMemory( TVkDevice( Device ).Handle, _Buffer.Memory.Handle, 0, _Buffer.Memory.Size, 0, @data )
     else res := vkMapMemory( TVkDevice( Device ).Handle, _MemoryHandle , 0, _MemoryInform.allocationSize, 0, @data );
     Assert( res = VK_SUCCESS );

     (* Read the ppm file into the mappable image's memory *)
     if Staging then rowPitch := PixelsW * 4
                       else rowPitch := layout.rowPitch;
     if not read_ppm( FileName_, PsW, PsH, rowPitch, data ) then
     begin
          Log.d( 'Could not load texture file lunarg.ppm' );
          RunError( 256-1 );
     end;

     if Staging
     then vkUnmapMemory( TVkDevice( Device ).Handle, _Buffer.Memory.Handle )
     else vkUnmapMemory( TVkDevice( Device ).Handle, _MemoryHandle );

     //////////

     Assert( vkResetCommandBuffer( TVkDevice( Device ).Poolers[0].Commans[0].Handle, 0 ) = VK_SUCCESS );
     TVkDevice( Device ).Poolers[0].Commans[0].BeginRecord;

     if not Staging then
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
          vkCmdCopyBufferToImage( TVkDevice( Device ).Poolers[0].Commans[0].Handle, _Buffer.Handle, Handle, VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL, 1, @copy_region );

          (* Set the layout for the texture image from DESTINATION_OPTIMAL to
           * SHADER_READ_ONLY *)
          _imageLayout := VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL;
          set_image_layout( TVkDevice( Device ).Instan.Vulkan, Handle, Ord( VK_IMAGE_ASPECT_COLOR_BIT ), VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL, _imageLayout,
                            Ord( VK_PIPELINE_STAGE_TRANSFER_BIT ), Ord( VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT ) );
     end;
end;

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【ルーチン】

end. //######################################################################### ■