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
       _Parent :TVkImager_;
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
       property Parent :TVkImager_            read   _Parent;
       property Inform :VkImageViewCreateInfo read   _Inform;
       property Handle :VkImageView           read GetHandle write SetHandle;
     end;

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TVkImager<TVkDevice,TParent>

     TVkImager<TVkDevice_,TParent_:class> = class
     private
       type TVkViewer_ = TVkViewer<TVkDevice_,TParent_>;
       ///// メソッド
       procedure init_buffer;
       procedure init_image;
     protected
       _Parent :TParent_;
       _Inform :VkImageCreateInfo;
       _Handle :VkImage;
       _Viewer :TVkViewer_;
        texObj :T_texture_object;
        textureName   :String;
        extraUsages   :VkImageUsageFlags;
        extraFeatures :VkFormatFeatureFlags;
       ///// アクセス
       function GetDevice :TVkDevice_; virtual; abstract;
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
       property Device :TVkDevice_        read GetDevice;
       property Parent :TParent_          read   _Parent;
       property Inform :VkImageCreateInfo read   _Inform;
       property Handle :VkImage           read GetHandle write SetHandle;
       property Viewer :TVkViewer_        read   _Viewer;
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
     Result := _Parent.Device;
end;

//------------------------------------------------------------------------------

function TVkViewer<TVkDevice_,TParent_>.GetHandle :VkImageView;
begin
     if _Handle = 0 then CreateHandle;

     Result := _Handle;
end;

procedure TVkViewer<TVkDevice_,TParent_>.SetHandle( const Handle_:VkImageView );
begin
     if _Handle <> 0 then DestroHandle;

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
          image    := _Parent.Handle;
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

     _Handle := 0;
end;

constructor TVkViewer<TVkDevice_,TParent_>.Create( const Parent_:TVkImager_ );
begin
     Create;

     _Parent := Parent_;
end;

destructor TVkViewer<TVkDevice_,TParent_>.Destroy;
begin
      Handle := 0;

     inherited;
end;

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TVkImager<TVkDevice,TParent>

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

procedure TVkImager<TVkDevice_,TParent_>.init_buffer;
var
   res                :VkResult;
   pass               :Boolean;
   buffer_create_info :VkBufferCreateInfo;
   mem_alloc          :VkMemoryAllocateInfo;
   mem_reqs           :VkMemoryRequirements;
   requirements       :VkFlags;
begin
     buffer_create_info                       := Default( VkBufferCreateInfo );
     buffer_create_info.sType                 := VK_STRUCTURE_TYPE_BUFFER_CREATE_INFO;
     buffer_create_info.pNext                 := nil;
     buffer_create_info.flags                 := 0;
     buffer_create_info.size                  := texObj.tex_width * texObj.tex_height * 4;
     buffer_create_info.usage                 := Ord( VK_BUFFER_USAGE_TRANSFER_SRC_BIT );
     buffer_create_info.sharingMode           := VK_SHARING_MODE_EXCLUSIVE;
     buffer_create_info.queueFamilyIndexCount := 0;
     buffer_create_info.pQueueFamilyIndices   := nil;
     res := vkCreateBuffer( TVkDevice( Device ).Handle, @buffer_create_info, nil, @texObj.buffer );
     Assert( res = VK_SUCCESS );

     mem_alloc                 := Default( VkMemoryAllocateInfo );
     mem_alloc.sType           := VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO;
     mem_alloc.pNext           := nil;
     mem_alloc.allocationSize  := 0;
     mem_alloc.memoryTypeIndex := 0;

     vkGetBufferMemoryRequirements( TVkDevice( Device ).Handle, texObj.buffer, @mem_reqs );
     mem_alloc.allocationSize := mem_reqs.size;
     texObj.buffer_size := mem_reqs.size;

     requirements := Ord( VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT ) or Ord( VK_MEMORY_PROPERTY_HOST_COHERENT_BIT );
     pass := TVkDevice( Device ).memory_type_from_properties( mem_reqs.memoryTypeBits, requirements, mem_alloc.memoryTypeIndex );
     Assert( pass, '"No mappable, coherent memory' );

     (* allocate memory *)
     res := vkAllocateMemory(TVkDevice( Device ).Handle, @mem_alloc, nil, @( texObj.buffer_memory) );
     Assert( res = VK_SUCCESS );

     (* bind memory *)
     res := vkBindBufferMemory( TVkDevice( Device ).Handle, texObj.buffer, texObj.buffer_memory, 0 );
     Assert( res = VK_SUCCESS );
end;

procedure TVkImager<TVkDevice_,TParent_>.init_image;
var
   res               :VkResult;
   pass              :Boolean;
   filename          :String;
   formatProps       :VkFormatProperties;
   allFeatures       :VkFormatFeatureFlags;
   image_create_info :VkImageCreateInfo;
   mem_alloc         :VkMemoryAllocateInfo;
   mem_reqs          :VkMemoryRequirements;
   requirements      :VkFlags;
   cmd_bufs          :array [ 0..1-1 ] of VkCommandBuffer;
   fenceInfo         :VkFenceCreateInfo;
   cmdFence          :VkFence;
   submit_info       :array [ 0..1-1 ] of VkSubmitInfo;
   subres            :VkImageSubresource;
   layout            :VkSubresourceLayout;
   data              :Pointer;
   rowPitch          :UInt64;
   copy_region       :VkBufferImageCopy;
begin
     filename := '../../_DATA/';

     if textureName = '' then filename := filename + 'lunarg.ppm'
                         else filename := filename + textureName;

     if not read_ppm( filename, texObj.tex_width, texObj.tex_height, 0, nil ) then
     begin
          Log.d( 'Try relative path' );
          filename := '../../_DATA/';
          if textureName ='' then filename := filename + 'lunarg.ppm'
                             else filename := filename + textureName;
          if not read_ppm( filename, texObj.tex_width, texObj.tex_height, 0, nil ) then
          begin
               Log.d( 'Could not read texture file ' + filename );
               RunError( 256-1 );
          end;
     end;

     vkGetPhysicalDeviceFormatProperties( TVkDevice( Device ).Physic, VK_FORMAT_R8G8B8A8_UNORM, @formatProps );

     (* See if we can use a linear tiled image for a texture, if not, we will
      * need a staging buffer for the texture data *)
     allFeatures := Ord( VK_FORMAT_FEATURE_SAMPLED_IMAGE_BIT ) or extraFeatures;
     texObj.needs_staging := ( ( formatProps.linearTilingFeatures and allFeatures ) <> allFeatures );

     if texObj.needs_staging then
     begin
          Assert( ( formatProps.optimalTilingFeatures and allFeatures ) = allFeatures );
          init_buffer;
          extraUsages := extraUsages or Ord( VK_IMAGE_USAGE_TRANSFER_DST_BIT );
     end
     else
     begin
          texObj.buffer        := VK_NULL_HANDLE;
          texObj.buffer_memory := VK_NULL_HANDLE;
     end;

     image_create_info                       := Default( VkImageCreateInfo );
     image_create_info.sType                 := VK_STRUCTURE_TYPE_IMAGE_CREATE_INFO;
     image_create_info.pNext                 := nil;
     image_create_info.imageType             := VK_IMAGE_TYPE_2D;
     image_create_info.format                := VK_FORMAT_R8G8B8A8_UNORM;
     image_create_info.extent.width          := texObj.tex_width;
     image_create_info.extent.height         := texObj.tex_height;
     image_create_info.extent.depth          := 1;
     image_create_info.mipLevels             := 1;
     image_create_info.arrayLayers           := 1;
     image_create_info.samples               := NUM_SAMPLES;
     if texObj.needs_staging
     then image_create_info.tiling           := VK_IMAGE_TILING_OPTIMAL
     else image_create_info.tiling           := VK_IMAGE_TILING_LINEAR;
     if texObj.needs_staging
     then image_create_info.initialLayout    := VK_IMAGE_LAYOUT_UNDEFINED
     else image_create_info.initialLayout    := VK_IMAGE_LAYOUT_PREINITIALIZED;
     image_create_info.usage                 := Ord( VK_IMAGE_USAGE_SAMPLED_BIT ) or extraUsages;
     image_create_info.queueFamilyIndexCount := 0;
     image_create_info.pQueueFamilyIndices   := nil;
     image_create_info.sharingMode           := VK_SHARING_MODE_EXCLUSIVE;
     image_create_info.flags                 := 0;

     mem_alloc                 := Default( VkMemoryAllocateInfo );
     mem_alloc.sType           := VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO;
     mem_alloc.pNext           := nil;
     mem_alloc.allocationSize  := 0;
     mem_alloc.memoryTypeIndex := 0;

     res := vkCreateImage( TVkDevice( Device ).Handle, @image_create_info, nil, @texObj.image );
     Assert( res = VK_SUCCESS );

     vkGetImageMemoryRequirements( TVkDevice( Device ).Handle, texObj.image, @mem_reqs );

     mem_alloc.allocationSize := mem_reqs.size;

     if texObj.needs_staging
     then requirements := 0
     else requirements := Ord( VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT ) or Ord( VK_MEMORY_PROPERTY_HOST_COHERENT_BIT );
     pass := TVkDevice( Device ).memory_type_from_properties( mem_reqs.memoryTypeBits, requirements, mem_alloc.memoryTypeIndex );
     Assert( pass );

     (* allocate memory *)
     res := vkAllocateMemory(TVkDevice( Device ).Handle, @mem_alloc, nil, @( texObj.image_memory) );
     Assert( res = VK_SUCCESS );

     (* bind memory *)
     res := vkBindImageMemory( TVkDevice( Device ).Handle, texObj.image, texObj.image_memory, 0 );
     Assert( res = VK_SUCCESS );

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
     res := vkQueueSubmit( TVkDevice( Device ).QueuerG, 1, @submit_info[0], cmdFence );
     Assert( res = VK_SUCCESS );

     subres            := Default( VkImageSubresource );
     subres.aspectMask := Ord( VK_IMAGE_ASPECT_COLOR_BIT );
     subres.mipLevel   := 0;
     subres.arrayLayer := 0;

     layout := Default( VkSubresourceLayout );
     if not texObj.needs_staging then
     begin
          (* Get the subresource layout so we know what the row pitch is *)
          vkGetImageSubresourceLayout( TVkDevice( Device ).Handle, texObj.image, @subres, @layout );
     end;

     (* Make sure command buffer is finished before mapping *)
     repeat
           res := vkWaitForFences( TVkDevice( Device ).Handle, 1, @cmdFence, VK_TRUE, FENCE_TIMEOUT );

     until res <> VK_TIMEOUT;
     Assert( res = VK_SUCCESS );

     vkDestroyFence( TVkDevice( Device ).Handle, cmdFence, nil );

     if texObj.needs_staging
     then res := vkMapMemory( TVkDevice( Device ).Handle, texObj.buffer_memory, 0, texObj.buffer_size, 0, @data )
     else res := vkMapMemory( TVkDevice( Device ).Handle, texObj.image_memory , 0, mem_reqs.size     , 0, @data );
     Assert( res = VK_SUCCESS );

     (* Read the ppm file into the mappable image's memory *)
     if texObj.needs_staging then rowPitch := texObj.tex_width * 4
                             else rowPitch := layout.rowPitch;
     if not read_ppm( filename, texObj.tex_width, texObj.tex_height, rowPitch, data ) then
     begin
          Log.d( 'Could not load texture file lunarg.ppm' );
          RunError( 256-1 );
     end;

     if texObj.needs_staging
     then vkUnmapMemory( TVkDevice( Device ).Handle, texObj.buffer_memory )
     else vkUnmapMemory( TVkDevice( Device ).Handle, texObj.image_memory  );

     res := vkResetCommandBuffer( TVkDevice( Device ).Poolers[0].Commans[0].Handle, 0 );
     Assert( res = VK_SUCCESS );
     TVkDevice( Device ).Poolers[0].Commans[0].BeginRecord;

     if not texObj.needs_staging then
     begin
          (* If we can use the linear tiled image as a texture, just do it *)
          texObj.imageLayout := VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL;
          set_image_layout( TVkDevice( Device ).Instan.Vulkan, texObj.image, Ord( VK_IMAGE_ASPECT_COLOR_BIT ), VK_IMAGE_LAYOUT_PREINITIALIZED, texObj.imageLayout,
                            Ord( VK_PIPELINE_STAGE_HOST_BIT ), Ord( VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT ) );
     end
     else
     begin
          (* Since we're going to blit to the texture image, set its layout to
           * DESTINATION_OPTIMAL *)
          set_image_layout( TVkDevice( Device ).Instan.Vulkan, texObj.image, Ord( VK_IMAGE_ASPECT_COLOR_BIT ), VK_IMAGE_LAYOUT_UNDEFINED,
                            VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL, Ord( VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT ), Ord( VK_PIPELINE_STAGE_TRANSFER_BIT ) );

          copy_region.bufferOffset                    := 0;
          copy_region.bufferRowLength                 := texObj.tex_width;
          copy_region.bufferImageHeight               := texObj.tex_height;
          copy_region.imageSubresource.aspectMask     := Ord( VK_IMAGE_ASPECT_COLOR_BIT );
          copy_region.imageSubresource.mipLevel       := 0;
          copy_region.imageSubresource.baseArrayLayer := 0;
          copy_region.imageSubresource.layerCount     := 1;
          copy_region.imageOffset.x                   := 0;
          copy_region.imageOffset.y                   := 0;
          copy_region.imageOffset.z                   := 0;
          copy_region.imageExtent.width               := texObj.tex_width;
          copy_region.imageExtent.height              := texObj.tex_height;
          copy_region.imageExtent.depth               := 1;

          (* Put the copy command into the command buffer *)
          vkCmdCopyBufferToImage( TVkDevice( Device ).Poolers[0].Commans[0].Handle, texObj.buffer, texObj.image, VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL, 1, @copy_region );

          (* Set the layout for the texture image from DESTINATION_OPTIMAL to
           * SHADER_READ_ONLY *)
          texObj.imageLayout := VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL;
          set_image_layout( TVkDevice( Device ).Instan.Vulkan, texObj.image, Ord( VK_IMAGE_ASPECT_COLOR_BIT ), VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL, texObj.imageLayout,
                            Ord( VK_PIPELINE_STAGE_TRANSFER_BIT ), Ord( VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT ) );
     end;
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

/////////////////////////////////////////////////////////////////////// アクセス

function TVkImager<TVkDevice_,TParent_>.GetHandle :VkImage;
begin
     if _Handle = 0 then CreateHandle;

     Result := _Handle;
end;

procedure TVkImager<TVkDevice_,TParent_>.SetHandle( const Handle_:VkImage );
begin
     if _Handle <> 0 then DestroHandle;

     _Handle := Handle_;
end;

/////////////////////////////////////////////////////////////////////// メソッド

procedure TVkImager<TVkDevice_,TParent_>.CreateHandle;
begin
     textureName   := '';
     extraUsages   := 0;
     extraFeatures := 0;

     (* create image *)
     init_image;

     _Handle := texObj.image;
end;

procedure TVkImager<TVkDevice_,TParent_>.DestroHandle;
begin
     vkDestroyImage ( TVkDevice( Device ).Handle, texObj.image        , nil );
     vkFreeMemory   ( TVkDevice( Device ).Handle, texObj.image_memory , nil );
     vkDestroyBuffer( TVkDevice( Device ).Handle, texObj.buffer       , nil );
     vkFreeMemory   ( TVkDevice( Device ).Handle, texObj.buffer_memory, nil );
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TVkImager<TVkDevice_,TParent_>.Create;
begin
     inherited;

     _Handle := 0;

     _Viewer := TVkViewer_.Create( Self );
end;

constructor TVkImager<TVkDevice_,TParent_>.Create( const Parent_:TParent_ );
begin
     Create;

     _Parent := Parent_;
end;

destructor TVkImager<TVkDevice_,TParent_>.Destroy;
begin
     _Viewer.Free;

      Handle := 0;

     inherited;
end;

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【ルーチン】

//############################################################################## □

initialization //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ 初期化

finalization //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ 最終化

end. //######################################################################### ■