unit LUX.GPU.Vulkan.Swapchain;

interface //#################################################################### ■

uses System.Generics.Collections,
     vulkan_core, vulkan_win32;

type //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【型】

     TVkSwapchain<TDevice_:class>       = class;
     TVkImageViews<TVkSwapchain_:class> = class;
     TVkImageView<TVkImageViews_:class> = class;

     //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【レコード】

     //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【クラス】

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TVkSwapchain

     TVkSwapchain<TDevice_:class> = class
     private
       type TVkSwapchain_  = TVkSwapchain<TDevice_>;
            TVkImageViews_ = TVkImageViews<TVkSwapchain_>;
     protected
       _Device     :TDevice_;
       _Handle     :VkSwapchainKHR;
       _ImageViews :TVkImageViews_;
       ///// メソッド
       procedure CreateHandle;
       procedure DestroHandle;
     public
       constructor Create( const Device_:TDevice_ );
       procedure AfterConstruction; override;
       destructor Destroy; override;
       ///// プロパティ
       property Device     :TDevice_       read _Device                      ;
       property Handle     :VkSwapchainKHR read _Handle                      ;
       property ImageViews :TVkImageViews_ read _ImageViews write _ImageViews;
     end;

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TVkImageViews

     TVkImageViews<TVkSwapchain_:class> = class( TObjectList<TVkImageView<TVkImageViews<TVkSwapchain_>>> )
     private
     protected
       _Swapchain :TVkSwapchain_;
       ///// メソッド
       procedure FindImages;
     public
       constructor Create( const Swapchain_:TVkSwapchain_ );
       procedure AfterConstruction; override;
       destructor Destroy; override;
       ///// プロパティ
       property Swapchain :TVkSwapchain_   read _Swapchain;
     end;

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TVkImageView

     TVkImageView<TVkImageViews_:class> = class
     private
     protected
       _ImageViews :TVkImageViews_;
       _Inform     :VkImageViewCreateInfo;
       _Handle     :VkImageView;
       ///// アクセス
       function GetImage :VkImage;
       ///// メソッド
       procedure CreateHandle;
       procedure DestroHandle;
     public
       constructor Create( const ImageViews_:TVkImageViews_; const Image_:VkImage );
       procedure AfterConstruction; override;
       destructor Destroy; override;
       ///// プロパティ
       property ImageViews :TVkImageViews_        read   _ImageViews;
       property Inform     :VkImageViewCreateInfo read   _Inform    ;
       property Image      :VkImage               read GetImage     ;
       property Handle     :VkImageView           read   _Handle    ;
     end;

//const //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【定数】

//var //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【変数】

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【ルーチン】

implementation //############################################################### ■

uses LUX.GPU.Vulkan,
     vulkan.util;

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【レコード】

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【クラス】

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TVkSwapchain

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

procedure TVkSwapchain<TDevice_>.CreateHandle;
var
   res                            :VkResult;
   surfCapabilities               :VkSurfaceCapabilitiesKHR;
   presentModeCount               :UInt32;
   presentModes                   :TArray<VkPresentModeKHR>;
   swapchainExtent                :VkExtent2D;
   swapchainPresentMode           :VkPresentModeKHR;
   desiredNumberOfSwapChainImages :UInt32;
   preTransform                   :VkSurfaceTransformFlagBitsKHR;
   compositeAlpha                 :VkCompositeAlphaFlagBitsKHR;
   compositeAlphaFlags            :array [ 0..4-1 ] of VkCompositeAlphaFlagBitsKHR;
   i                              :UInt32;
   swapchain_ci                   :VkSwapchainCreateInfoKHR;
   queueFamilyIndices             :array [ 0..2-1 ] of UInt32;
begin
     (* DEPENDS on info.cmd and info.queue initialized *)

     res := vkGetPhysicalDeviceSurfaceCapabilitiesKHR( TVkDevice( _Device ).PhysHandle, TVkDevice( _Device ).Devices.Instance.Window.Surface.Handle, @surfCapabilities );
     Assert( res = VK_SUCCESS );

     res := vkGetPhysicalDeviceSurfacePresentModesKHR( TVkDevice( _Device ).PhysHandle, TVkDevice( _Device ).Devices.Instance.Window.Surface.Handle, @presentModeCount, nil );
     Assert( res = VK_SUCCESS );
     SetLength( presentModes, presentModeCount );
     Assert( Length( presentModes ) > 0 );
     res := vkGetPhysicalDeviceSurfacePresentModesKHR( TVkDevice( _Device ).PhysHandle, TVkDevice( _Device ).Devices.Instance.Window.Surface.Handle, @presentModeCount, @presentModes[0] );
     Assert( res = VK_SUCCESS );

     // width and height are either both 0xFFFFFFFF, or both not 0xFFFFFFFF.
     if surfCapabilities.currentExtent.width = $FFFFFFFF then
     begin
          // If the surface size is undefined, the size is set to
          // the size of the images requested.
          swapchainExtent.width  := TVkDevice( _Device ).Devices.Instance.Window.width;
          swapchainExtent.height := TVkDevice( _Device ).Devices.Instance.Window.height;
          if swapchainExtent.width < surfCapabilities.minImageExtent.width
          then swapchainExtent.width := surfCapabilities.minImageExtent.width
          else
          if swapchainExtent.width > surfCapabilities.maxImageExtent.width
          then swapchainExtent.width := surfCapabilities.maxImageExtent.width;

          if swapchainExtent.height < surfCapabilities.minImageExtent.height
          then swapchainExtent.height := surfCapabilities.minImageExtent.height
          else
          if swapchainExtent.height > surfCapabilities.maxImageExtent.height
          then swapchainExtent.height := surfCapabilities.maxImageExtent.height;
     end
     else
     begin
          // If the surface size is defined, the swap chain size must match
          swapchainExtent := surfCapabilities.currentExtent;
     end;

     // The FIFO present mode is guaranteed by the spec to be supported
     // Also note that current Android driver only supports FIFO
     swapchainPresentMode := VK_PRESENT_MODE_FIFO_KHR;

     // Determine the number of VkImage's to use in the swap chain.
     // We need to acquire only 1 presentable image at at time.
     // Asking for minImageCount images ensures that we can acquire
     // 1 presentable image as long as we present it before attempting
     // to acquire another.
     desiredNumberOfSwapChainImages := surfCapabilities.minImageCount;

     if ( surfCapabilities.supportedTransforms and Ord( VK_SURFACE_TRANSFORM_IDENTITY_BIT_KHR ) ) <> 0
     then preTransform := VK_SURFACE_TRANSFORM_IDENTITY_BIT_KHR
     else preTransform := surfCapabilities.currentTransform;

     // Find a supported composite alpha mode - one of these is guaranteed to be set
     compositeAlpha         := VK_COMPOSITE_ALPHA_OPAQUE_BIT_KHR;
     compositeAlphaFlags[0] := VK_COMPOSITE_ALPHA_OPAQUE_BIT_KHR;
     compositeAlphaFlags[1] := VK_COMPOSITE_ALPHA_PRE_MULTIPLIED_BIT_KHR;
     compositeAlphaFlags[2] := VK_COMPOSITE_ALPHA_POST_MULTIPLIED_BIT_KHR;
     compositeAlphaFlags[3] := VK_COMPOSITE_ALPHA_INHERIT_BIT_KHR;

     for i := 0 to Length( compositeAlphaFlags )-1 do
     begin
          if ( surfCapabilities.supportedCompositeAlpha and Ord( compositeAlphaFlags[i] ) ) <> 0 then
          begin
               compositeAlpha := compositeAlphaFlags[i];
               Break;
          end;
     end;

     swapchain_ci                       := Default( VkSwapchainCreateInfoKHR );
     swapchain_ci.sType                 := VK_STRUCTURE_TYPE_SWAPCHAIN_CREATE_INFO_KHR;
     swapchain_ci.pNext                 := nil;
     swapchain_ci.surface               := TVkDevice( _Device ).Devices.Instance.Window.Surface.Handle;
     swapchain_ci.minImageCount         := desiredNumberOfSwapChainImages;
     swapchain_ci.imageFormat           := TVkDevice( _Device ).Format;
     swapchain_ci.imageExtent.width     := swapchainExtent.width;
     swapchain_ci.imageExtent.height    := swapchainExtent.height;
     swapchain_ci.preTransform          := preTransform;
     swapchain_ci.compositeAlpha        := compositeAlpha;
     swapchain_ci.imageArrayLayers      := 1;
     swapchain_ci.presentMode           := swapchainPresentMode;
     swapchain_ci.oldSwapchain          := VK_NULL_HANDLE;
     swapchain_ci.clipped               := 1;
     swapchain_ci.imageColorSpace       := VK_COLORSPACE_SRGB_NONLINEAR_KHR;
     swapchain_ci.imageUsage            := Ord( VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT ) or Ord( VK_IMAGE_USAGE_TRANSFER_SRC_BIT );
     swapchain_ci.imageSharingMode      := VK_SHARING_MODE_EXCLUSIVE;
     swapchain_ci.queueFamilyIndexCount := 0;
     swapchain_ci.pQueueFamilyIndices   := nil;
     queueFamilyIndices[0] := TVkDevice( _Device ).GraphicsQueueFamilyI;
     queueFamilyIndices[1] := TVkDevice( _Device ).PresentQueueFamilyI;
     if TVkDevice( _Device ).GraphicsQueueFamilyI <> TVkDevice( _Device ).PresentQueueFamilyI then
     begin
          // If the graphics and present queues are from different queue families,
          // we either have to explicitly transfer ownership of images between the
          // queues, or we have to create the swapchain with imageSharingMode
          // as VK_SHARING_MODE_CONCURRENT
          swapchain_ci.imageSharingMode      := VK_SHARING_MODE_CONCURRENT;
          swapchain_ci.queueFamilyIndexCount := 2;
          swapchain_ci.pQueueFamilyIndices   := @queueFamilyIndices[0];
     end;

     res := vkCreateSwapchainKHR( TVkDevice( _Device ).Handle, @swapchain_ci, nil, @_Handle );
     Assert( res = VK_SUCCESS );
end;

procedure TVkSwapchain<TDevice_>.DestroHandle;
begin
     vkDestroySwapchainKHR( TVkDevice( Device ).Handle, _Handle, nil );
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TVkSwapchain<TDevice_>.Create( const Device_:TDevice_ );
begin
     inherited Create;

     _Device := Device_;

     TVkDevice( _Device ).Swapchains := TVkSwapchain( Self );

     CreateHandle;

     _ImageViews := TVkImageViews_.Create( Self );
end;

procedure TVkSwapchain<TDevice_>.AfterConstruction;
begin
     inherited;

end;

destructor TVkSwapchain<TDevice_>.Destroy;
begin
     _ImageViews.Free;

     DestroHandle;

     inherited;
end;

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TVkImageViews

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

procedure TVkImageViews<TVkSwapchain_>.FindImages;
var
   res                            :VkResult;
   i                              :UInt32;
   swapchainImages                :TArray<VkImage>;
   color_image_view      :VkImageViewCreateInfo;
   sc_buffer                      :T_swap_chain_buffer;
begin
     res := vkGetSwapchainImagesKHR( TVkSwapchain( _Swapchain ).Device.Handle, TVkSwapchain( _Swapchain ).Handle, @TVkSwapchain( _Swapchain ).Device.Devices.Instance.Vulkan.Info.swapchainImageCount, nil );
     Assert( res = VK_SUCCESS );

     SetLength( swapchainImages, TVkSwapchain( _Swapchain ).Device.Devices.Instance.Vulkan.Info.swapchainImageCount );
     Assert( Length( swapchainImages ) > 0 );
     res := vkGetSwapchainImagesKHR( TVkSwapchain( _Swapchain ).Device.Handle, TVkSwapchain( _Swapchain ).Handle, @TVkSwapchain( _Swapchain ).Device.Devices.Instance.Vulkan.Info.swapchainImageCount, @swapchainImages[0] );
     Assert( res = VK_SUCCESS );

     for i := 0 to TVkSwapchain( _Swapchain ).Device.Devices.Instance.Vulkan.Info.swapchainImageCount-1 do
     begin
          TVkImageView.Create( TVkImageViews( Self ), swapchainImages[i] );
     end;
     TVkSwapchain( _Swapchain ).Device.Devices.Instance.Vulkan.Info.current_buffer := 0;
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TVkImageViews<TVkSwapchain_>.Create( const Swapchain_:TVkSwapchain_ );
begin
     inherited Create;

     _Swapchain := Swapchain_;

     TVkSwapchain( _Swapchain ).ImageViews := TVkImageViews( Self );

     FindImages;
end;

procedure TVkImageViews<TVkSwapchain_>.AfterConstruction;
begin
     inherited;

end;

destructor TVkImageViews<TVkSwapchain_>.Destroy;
begin

     inherited;
end;

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TVkImageView

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

/////////////////////////////////////////////////////////////////////// アクセス

function TVkImageView<TVkImageViews_>.GetImage :VkImage;
begin
     Result := _Inform.image;
end;

/////////////////////////////////////////////////////////////////////// メソッド

procedure TVkImageView<TVkImageViews_>.CreateHandle;
begin
     Assert( vkCreateImageView( TVkImageViews( _ImageViews ).Swapchain.Device.Handle, @_Inform, nil, @_Handle ) = VK_SUCCESS );
end;

procedure TVkImageView<TVkImageViews_>.DestroHandle;
begin
     vkDestroyImageView( TVkImageViews( _ImageViews ).Swapchain.Device.Handle, _Handle, nil );
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TVkImageView<TVkImageViews_>.Create( const ImageViews_:TVkImageViews_; const Image_:VkImage );
var
   B :T_swap_chain_buffer;
begin
     inherited Create;

     _ImageViews  := ImageViews_;

     TVkImageViews( _ImageViews ).Add( TVkImageView( Self ) );

     with _Inform do
     begin
          sType    := VK_STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO;
          pNext    := nil;
          flags    := 0;
          image    := Image_;
          viewType := VK_IMAGE_VIEW_TYPE_2D;
          format   := TVkImageViews( _ImageViews ).Swapchain.Device.Format;

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

     CreateHandle;
end;

procedure TVkImageView<TVkImageViews_>.AfterConstruction;
begin
     inherited;

end;

destructor TVkImageView<TVkImageViews_>.Destroy;
begin
     DestroHandle;

     inherited;
end;

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【ルーチン】

//############################################################################## □

initialization //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ 初期化

finalization //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ 最終化

end. //######################################################################### ■
