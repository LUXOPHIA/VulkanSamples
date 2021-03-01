unit LUX.GPU.Vulkan;

interface //#################################################################### ■

uses System.Classes,
     vulkan_core, vulkan_win32,
     vulkan.util,
     LUX.Code.C,
     LUX.GPU.Vulkan.root,
     LUX.GPU.Vulkan.Device,
     LUX.GPU.Vulkan.Buffer,
     LUX.GPU.Vulkan.Shader,
     LUX.GPU.Vulkan.Pipeline;

type //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【型】

     TVulkan       = class;
     TVkInstance   = TVkInstance<TVulkan>;
     TVkDevices    = TVkDevices<TVulkan>;
     TVkDevice     = TVkDevice<TVulkan>;
     TVkPipeline   = TVkPipeline<TVkDevice>;
     TVkShader     = TVkShader<TVkPipeline>;
     TVkShaderVert = TVkShaderVert<TVkPipeline>;
     TVkShaderFrag = TVkShaderFrag<TVkPipeline>;

     TVkLayers = TArray<T_layer_properties>;

     //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【レコード】

     //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【クラス】

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TVulkan

     TVulkan = class
     private
     protected
       _Instance :TVkInstance;
       _Devices  :TVkDevices;
       _Layers   :TVkLayers;
       ///// メソッド
       function init_global_extension_properties( var L_:T_layer_properties ) :VkResult;
       function init_global_layer_properties :VkResult;
     public
       Info :T_sample_info;
       constructor Create;
       procedure AfterConstruction; override;
       destructor Destroy; override;
       ///// プロパティ
       property Instance :TVkInstance read _Instance write _Instance;
       property Devices  :TVkDevices  read _Devices  write _Devices ;
       property Layers   :TVkLayers   read _Layers   write _Layers  ;
     end;

//const //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【定数】

//var //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【変数】

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【ルーチン】

function memory_type_from_properties( var info:T_sample_info; typeBits:T_uint32_t; requirements_mask:VkFlags; var typeIndex:T_uint32_t ) :T_bool;

//////////////////////////////////////////////////////////////////////////////// 15-draw_cube

procedure wait_seconds( seconds_:T_int );
procedure set_image_layout( Vulkan_:TVulkan; image_:VkImage; aspectMask_:VkImageAspectFlags; old_image_layout_:VkImageLayout;
                            new_image_layout_:VkImageLayout; src_stages_:VkPipelineStageFlags; dest_stages_:VkPipelineStageFlags );
procedure write_ppm( Vulkan_:TVulkan; const basename_:String );

//////////////////////////////////////////////////////////////////////////////// draw_textured_cube

function read_ppm( const filename_:String; var width_:T_int; var height_:T_int; rowPitch_:T_uint64_t; dataPtr_:P_unsigned_char ) :T_bool;

implementation //############################################################### ■

uses System.SysUtils,
     FMX.Types;

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【レコード】

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【クラス】

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TVulkan

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

function TVulkan.init_global_extension_properties( var L_:T_layer_properties ) :VkResult;
var
   EsN :UInt32;
begin
     repeat
           Result := vkEnumerateInstanceExtensionProperties( L_.properties.layerName, @EsN, nil );
           if Result <> VK_SUCCESS then Exit;

           if EsN = 0 then Exit( VK_SUCCESS );

           SetLength( L_.instance_extensions, EsN );
           Result := vkEnumerateInstanceExtensionProperties( L_.properties.layerName, @EsN, @L_.instance_extensions[0] );

     until Result <> VK_INCOMPLETE;
end;

function TVulkan.init_global_layer_properties :VkResult;
var
   LsN, I :UInt32;
   Ls :TArray<VkLayerProperties>;
   L :T_layer_properties;
begin
     (*
      * It's possible, though very rare, that the number of
      * instance layers could change. For example, installing something
      * could include new layers that the loader would pick up
      * between the initial query for the count and the
      * request for VkLayerProperties. The loader indicates that
      * by returning a VK_INCOMPLETE status and will update the
      * the count parameter.
      * The count parameter will be updated with the number of
      * entries loaded into the data pointer - in case the number
      * of layers went down or is smaller than the size given.
      *)
     repeat
           Result := vkEnumerateInstanceLayerProperties( @LsN, nil );
           if Result <> VK_SUCCESS then Exit;

           if LsN = 0 then Exit( VK_SUCCESS );

           SetLength( Ls, LsN );

           Result := vkEnumerateInstanceLayerProperties( @LsN, @Ls[0] );

     until Result <> VK_INCOMPLETE;

     (*
      * Now gather the extension list for each instance layer.
      *)
     for I := 0 to LsN-1 do
     begin
          L.properties := Ls[I];
          Result := init_global_extension_properties( L );
          if Result <> VK_SUCCESS then Exit;
          _Layers := _Layers + [ L ];
     end;
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TVulkan.Create;
begin
     inherited;

     init_global_layer_properties;
end;

procedure TVulkan.AfterConstruction;
begin
     inherited;

end;

destructor TVulkan.Destroy;
begin

     inherited;
end;

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【ルーチン】

function memory_type_from_properties( var info:T_sample_info; typeBits:T_uint32_t; requirements_mask:VkFlags; var typeIndex:T_uint32_t ) :T_bool;
var
   i :T_uint32_t;
begin
     // Search memtypes to find first index with those properties
     for i := 0 to info.memory_properties.memoryTypeCount-1 do
     begin
          if ( typeBits and 1 ) = 1 then
          begin
               // Type is available, does it match user properties?
               if info.memory_properties.memoryTypes[i].propertyFlags and requirements_mask = requirements_mask then
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

//////////////////////////////////////////////////////////////////////////////// 15-draw_cube

procedure wait_seconds( seconds_:T_int );
begin
     Sleep( seconds_ * 1000 );
end;

procedure set_image_layout( Vulkan_:TVulkan; image_:VkImage; aspectMask_:VkImageAspectFlags; old_image_layout_:VkImageLayout;
                            new_image_layout_:VkImageLayout; src_stages_:VkPipelineStageFlags; dest_stages_:VkPipelineStageFlags );
var
   image_memory_barrier :VkImageMemoryBarrier;
begin
     (* DEPENDS on info.cmd and info.queue initialized *)

     Assert( NativeInt( Vulkan_.Info.cmd            ) <> VK_NULL_HANDLE );
     Assert( NativeInt( Vulkan_.Info.graphics_queue ) <> VK_NULL_HANDLE );

     image_memory_barrier.sType                           := VK_STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER;
     image_memory_barrier.pNext                           := nil;
     image_memory_barrier.srcAccessMask                   := 0;
     image_memory_barrier.dstAccessMask                   := 0;
     image_memory_barrier.oldLayout                       := old_image_layout_;
     image_memory_barrier.newLayout                       := new_image_layout_;
     image_memory_barrier.srcQueueFamilyIndex             := VK_QUEUE_FAMILY_IGNORED;
     image_memory_barrier.dstQueueFamilyIndex             := VK_QUEUE_FAMILY_IGNORED;
     image_memory_barrier.image                           := image_;
     image_memory_barrier.subresourceRange.aspectMask     := aspectMask_;
     image_memory_barrier.subresourceRange.baseMipLevel   := 0;
     image_memory_barrier.subresourceRange.levelCount     := 1;
     image_memory_barrier.subresourceRange.baseArrayLayer := 0;
     image_memory_barrier.subresourceRange.layerCount     := 1;

     case old_image_layout_ of
       VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL:
          image_memory_barrier.srcAccessMask := Ord( VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT );

       VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL:
          image_memory_barrier.srcAccessMask := Ord( VK_ACCESS_TRANSFER_WRITE_BIT         );

       VK_IMAGE_LAYOUT_PREINITIALIZED:
          image_memory_barrier.srcAccessMask := Ord( VK_ACCESS_HOST_WRITE_BIT             );
     end;

     case new_image_layout_ of
       VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL:
          image_memory_barrier.dstAccessMask := Ord( VK_ACCESS_TRANSFER_WRITE_BIT                 );

       VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL:
          image_memory_barrier.dstAccessMask := Ord( VK_ACCESS_TRANSFER_READ_BIT                  );

       VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL:
          image_memory_barrier.dstAccessMask := Ord( VK_ACCESS_SHADER_READ_BIT                    );

       VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL:
          image_memory_barrier.dstAccessMask := Ord( VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT         );

       VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL:
          image_memory_barrier.dstAccessMask := Ord( VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT );
     end;

     vkCmdPipelineBarrier( Vulkan_.Info.cmd, src_stages_, dest_stages_, 0, 0, nil, 0, nil, 1, @image_memory_barrier );
end;

procedure write_ppm( Vulkan_:TVulkan; const basename_:String );
var
   filename          :String;
   x, y              :T_int;
   res               :VkResult;
   image_create_info :VkImageCreateInfo;
   mem_alloc         :VkMemoryAllocateInfo;
   mappableImage     :VkImage;
   mappableMemory    :VkDeviceMemory;
   mem_reqs          :VkMemoryRequirements;
   pass              :T_bool;
   cmd_buf_info      :VkCommandBufferBeginInfo;
   copy_region       :VkImageCopy;
   cmd_bufs          :array [ 0..1-1 ] of VkCommandBuffer;
   fenceInfo         :VkFenceCreateInfo;
   cmdFence          :VkFence;
   submit_info       :array [ 0..1-1 ] of VkSubmitInfo;
   subres            :VkImageSubresource;
   sr_layout         :VkSubresourceLayout;
   ptr               :P_char;
   F                 :TFileStream;
   S                 :String;
   row               :P_uint32_t;
   swapped           :T_uint32_t;
begin
     image_create_info.sType                 := VK_STRUCTURE_TYPE_IMAGE_CREATE_INFO;
     image_create_info.pNext                 := nil;
     image_create_info.imageType             := VK_IMAGE_TYPE_2D;
     image_create_info.format                := Vulkan_.Info.format;
     image_create_info.extent.width          := Vulkan_.Info.width;
     image_create_info.extent.height         := Vulkan_.Info.height;
     image_create_info.extent.depth          := 1;
     image_create_info.mipLevels             := 1;
     image_create_info.arrayLayers           := 1;
     image_create_info.samples               := VK_SAMPLE_COUNT_1_BIT;
     image_create_info.tiling                := VK_IMAGE_TILING_LINEAR;
     image_create_info.initialLayout         := VK_IMAGE_LAYOUT_UNDEFINED;
     image_create_info.usage                 := Ord( VK_IMAGE_USAGE_TRANSFER_DST_BIT );
     image_create_info.queueFamilyIndexCount := 0;
     image_create_info.pQueueFamilyIndices   := nil;
     image_create_info.sharingMode           := VK_SHARING_MODE_EXCLUSIVE;
     image_create_info.flags                 := 0;

     mem_alloc.sType           := VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO;
     mem_alloc.pNext           := nil;
     mem_alloc.allocationSize  := 0;
     mem_alloc.memoryTypeIndex := 0;

     (* Create a mappable image *)
     res := vkCreateImage( Vulkan_.Devices.Devices[ 0 ].Handle, @image_create_info, nil, @mappableImage );
     Assert( res = VK_SUCCESS );

     vkGetImageMemoryRequirements( Vulkan_.Devices.Devices[ 0 ].Handle, mappableImage, @mem_reqs );

     mem_alloc.allocationSize := mem_reqs.size;

     (* Find the memory type that is host mappable *)
     pass := memory_type_from_properties(
                  Vulkan_.Info, mem_reqs.memoryTypeBits, Ord( VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT ) or Ord( VK_MEMORY_PROPERTY_HOST_COHERENT_BIT ),
                  mem_alloc.memoryTypeIndex );
     Assert( pass, 'No mappable, coherent memory' );

     (* allocate memory *)
     res := vkAllocateMemory( Vulkan_.Devices.Devices[ 0 ].Handle, @mem_alloc, nil, @mappableMemory );
     Assert( res = VK_SUCCESS );

     (* bind memory *)
     res := vkBindImageMemory( Vulkan_.Devices.Devices[ 0 ].Handle, mappableImage, mappableMemory, 0 );
     Assert( res = VK_SUCCESS );

     cmd_buf_info.sType            := VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO;
     cmd_buf_info.pNext            := nil;
     cmd_buf_info.flags            := 0;
     cmd_buf_info.pInheritanceInfo := nil;

     res := vkBeginCommandBuffer( Vulkan_.Info.cmd, @cmd_buf_info );
     Assert( res = VK_SUCCESS );
     set_image_layout( Vulkan_, mappableImage, Ord( VK_IMAGE_ASPECT_COLOR_BIT ), VK_IMAGE_LAYOUT_UNDEFINED,
                       VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL, Ord( VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT ), Ord( VK_PIPELINE_STAGE_TRANSFER_BIT ) );

     set_image_layout( Vulkan_, Vulkan_.Info.buffers[Vulkan_.Info.current_buffer].image, Ord( VK_IMAGE_ASPECT_COLOR_BIT ), VK_IMAGE_LAYOUT_PRESENT_SRC_KHR,
                       VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL, Ord( VK_PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT ), Ord( VK_PIPELINE_STAGE_TRANSFER_BIT ) );

     copy_region.srcSubresource.aspectMask     := Ord( VK_IMAGE_ASPECT_COLOR_BIT );
     copy_region.srcSubresource.mipLevel       := 0;
     copy_region.srcSubresource.baseArrayLayer := 0;
     copy_region.srcSubresource.layerCount     := 1;
     copy_region.srcOffset.x                   := 0;
     copy_region.srcOffset.y                   := 0;
     copy_region.srcOffset.z                   := 0;
     copy_region.dstSubresource.aspectMask     := Ord( VK_IMAGE_ASPECT_COLOR_BIT );
     copy_region.dstSubresource.mipLevel       := 0;
     copy_region.dstSubresource.baseArrayLayer := 0;
     copy_region.dstSubresource.layerCount     := 1;
     copy_region.dstOffset.x                   := 0;
     copy_region.dstOffset.y                   := 0;
     copy_region.dstOffset.z                   := 0;
     copy_region.extent.width                  := Vulkan_.Info.width;
     copy_region.extent.height                 := Vulkan_.Info.height;
     copy_region.extent.depth                  := 1;

     (* Put the copy command into the command buffer *)
     vkCmdCopyImage( Vulkan_.Info.cmd, Vulkan_.Info.buffers[Vulkan_.Info.current_buffer].image, VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL, mappableImage,
                     VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL, 1, @copy_region);

     set_image_layout( Vulkan_, mappableImage, Ord( VK_IMAGE_ASPECT_COLOR_BIT ), VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL, VK_IMAGE_LAYOUT_GENERAL,
                       Ord( VK_PIPELINE_STAGE_TRANSFER_BIT ), Ord( VK_PIPELINE_STAGE_HOST_BIT ) );

     res := vkEndCommandBuffer( Vulkan_.Info.cmd );
     Assert( res = VK_SUCCESS );
     cmd_bufs[0] := Vulkan_.Info.cmd;
     fenceInfo.sType := VK_STRUCTURE_TYPE_FENCE_CREATE_INFO;
     fenceInfo.pNext := nil;
     fenceInfo.flags := 0;
     vkCreateFence( Vulkan_.Devices.Devices[ 0 ].Handle, @fenceInfo, nil, @cmdFence );

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
     res := vkQueueSubmit( Vulkan_.Info.graphics_queue, 1, @submit_info[0], cmdFence );
     Assert( res = VK_SUCCESS );

     (* Make sure command buffer is finished before mapping *)
     repeat
           res := vkWaitForFences( Vulkan_.Devices.Devices[ 0 ].Handle, 1, @cmdFence, VK_TRUE, FENCE_TIMEOUT );

     until res <> VK_TIMEOUT;
     Assert( res = VK_SUCCESS );

     vkDestroyFence( Vulkan_.Devices.Devices[ 0 ].Handle, cmdFence, nil );

     filename := basename_ + '.ppm';

     subres.aspectMask := Ord( VK_IMAGE_ASPECT_COLOR_BIT );
     subres.mipLevel   := 0;
     subres.arrayLayer := 0;
     vkGetImageSubresourceLayout( Vulkan_.Devices.Devices[ 0 ].Handle, mappableImage, @subres, @sr_layout );

     res := vkMapMemory( Vulkan_.Devices.Devices[ 0 ].Handle, mappableMemory, 0, mem_reqs.size, 0, @ptr );
     Assert( res = VK_SUCCESS );

     Inc( ptr, sr_layout.offset );
     F := TFileStream.Create( filename, fmCreate );

     S := 'P6'                                               + #13#10;  F.Write( BytesOf( S ), Length( S ) );
     S := Vulkan_.Info.width.ToString + ' ' + Vulkan_.Info.height.ToString + #13#10;  F.Write( BytesOf( S ), Length( S ) );
     S := '255'                                              + #13#10;  F.Write( BytesOf( S ), Length( S ) );

     for y := 0 to Vulkan_.Info.height-1 do
     begin
          row := P_uint32_t( ptr );

          if ( Vulkan_.Info.format = VK_FORMAT_B8G8R8A8_UNORM ) or ( Vulkan_.Info.format = VK_FORMAT_B8G8R8A8_SRGB ) then
          begin
               for x := 0 to Vulkan_.Info.width-1 do
               begin
                    swapped := ( row^ and $ff00ff00 ) or ( row^ and $000000ff ) shl 16 or ( row^ and $00ff0000 ) shr 16;
                    F.Write( swapped, 3 );
                    Inc( row );
               end;
          end
          else
          if Vulkan_.Info.format = VK_FORMAT_R8G8B8A8_UNORM then
          begin
               for x := 0 to Vulkan_.Info.width-1 do
               begin
                    F.Write( row^, 3 );
                    Inc( row );
               end;
          end
          else
          begin
               Log.d( 'Unrecognized image format - will not write image files' );
               Break;
          end;

          Inc( ptr, sr_layout.rowPitch );
     end;

     F.Free;
     vkUnmapMemory( Vulkan_.Devices.Devices[ 0 ].Handle, mappableMemory );
     vkDestroyImage( Vulkan_.Devices.Devices[ 0 ].Handle, mappableImage, nil );
     vkFreeMemory( Vulkan_.Devices.Devices[ 0 ].Handle, mappableMemory, nil );
end;

//////////////////////////////////////////////////////////////////////////////// draw_textured_cube

function read_ppm( const filename_:String; var width_:T_int; var height_:T_int; rowPitch_:T_uint64_t; dataPtr_:P_unsigned_char ) :T_bool;
type
    T_ReadHeader = reference to function( var S:String ) :Boolean;
const
     saneDimension :T_int = 32768;  //??
var
    magicStr  :String;
   heightStr  :String;
    widthStr  :String;
   formatStr  :String;
   fPtr       :TFileStream;
   ReadHeader :T_ReadHeader;
   count      :T_int;
   x, y       :T_int;
   rowPtr     :P_unsigned_char;
begin
     // PPM format expected from http://netpbm.sourceforge.net/doc/ppm.html
     //  1. magic number
     //  2. whitespace
     //  3. width
     //  4. whitespace
     //  5. height
     //  6. whitespace
     //  7. max color value
     //  8. whitespace
     //  7. data

     // Comments are not supported, but are detected and we kick out
     // Only 8 bits per channel is supported
     // If dataPtr is nullptr, only width and height are returned

     // Read in values from the PPM file as characters to check for comments
      magicStr := '';
      widthStr := '';
     heightStr := '';
     formatStr := '';

     try
          fPtr := TFileStream.Create( filename_, fmOpenRead or fmShareDenyWrite );

          try
               // Read the four values from file, accounting with any and all whitepace
               ReadHeader := function( var S:String ) :Boolean
               var
                  C :T_char;
               begin
                    while fPtr.Read( C, 1 ) = 1 do
                    begin
                         if C in [ #09, #10, #13, #32 ] then Exit( True );
                         S := S + Char( C );
                    end;
                    Result := False;
               end;
               Assert( ReadHeader(  magicStr ) );
               Assert( ReadHeader(  widthStr ) );
               Assert( ReadHeader( heightStr ) );
               Assert( ReadHeader( formatStr ) );

               // Kick out if comments present
               if ( magicStr.Chars[0] = '#' ) or ( widthStr.Chars[0] = '#' ) or ( heightStr.Chars[0] = '#' ) or ( formatStr.Chars[0] = '#' ) then
               begin
                    Log.d( 'Unhandled comment in PPM file' );
                    Exit( False );
               end;

               // Only one magic value is valid
               if magicStr <> 'P6' then
               begin
                    Log.d( 'Unhandled PPM magic number: ' + magicStr );
                    Exit( False );
               end;

               width_  := StrToInt(  widthStr );
               height_ := StrToInt( heightStr );

               // Ensure we got something sane for width/height
               if (  width_ <= 0 ) or (  width_ > saneDimension ) then
               begin
                    Log.d( 'Width seems wrong.  Update read_ppm if not: ' + width_.ToString );
                    Exit( False );
               end;
               if ( height_ <= 0 ) or ( height_ > saneDimension ) then
               begin
                    Log.d( 'Height seems wrong.  Update read_ppm if not: ' + height_.ToString );
                    Exit( False );
               end;

               if dataPtr_ = nil then
               begin
                    // If no destination pointer, caller only wanted dimensions
                    Exit( True );
               end;

               // Now read the data
               for y := 0 to height_-1 do
               begin
                    rowPtr := dataPtr_;
                    for x := 0 to width_-1 do
                    begin
                         count := fPtr.Read( rowPtr^, 3 );
                         Assert( count = 3 );
                         Inc( rowPtr, 3 ); rowPtr^ := 255; (* Alpha of 1 *)
                         Inc( rowPtr );
                    end;
                    Inc( dataPtr_, rowPitch_ );
               end;

               Result := True;

          finally
                 fPtr.Free;
          end;

     except
           Log.d( 'Bad filename in read_ppm: ' + filename_ );

           Result := False;
     end;
end;

//############################################################################## □

initialization //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ 初期化

finalization //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ 最終化

end. //######################################################################### ■