unit LUX.GPU.Vulkan.Swapch;

interface //#################################################################### ■

uses System.Generics.Collections,
     vulkan_core, vulkan_win32;

type //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【型】

     TVkSwapch<TVkDevice_:class>      = class;
       TVkFramers<TVkSwapch_:class>   = class;
         TVkFramer<TVkFramers_:class> = class;

     //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【レコード】

     //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【クラス】

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TVkFramer

     TVkFramer<TVkFramers_:class> = class
     private
     protected
       _Viewers :TVkFramers_;
       _Inform  :VkImageViewCreateInfo;
       _Handle  :VkImageView;
       ///// アクセス
       function GetImage :VkImage;
       ///// メソッド
       procedure CreateHandle;
       procedure DestroHandle;
     public
       constructor Create( const Viewers_:TVkFramers_; const Image_:VkImage );
       destructor Destroy; override;
       ///// プロパティ
       property Viewers :TVkFramers_           read   _Viewers;
       property Inform  :VkImageViewCreateInfo read   _Inform ;
       property Image   :VkImage               read GetImage  ;
       property Handle  :VkImageView           read   _Handle ;
     end;

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TVkFramers

     TVkFramers<TVkSwapch_:class> = class( TObjectList<TVkFramer<TVkFramers<TVkSwapch_>>> )
     private
       type TVkFramers_ = TVkFramers<TVkSwapch_>;
            TVkFramer_  = TVkFramer<TVkFramers_>;
     protected
       _Swapch  :TVkSwapch_;
       _ViewerI :UInt32;
       ///// アクセス
       function GetViewer :TVkFramer_;
       ///// メソッド
       procedure FindImages;
     public
       constructor Create( const Swapch_:TVkSwapch_ );
       destructor Destroy; override;
       ///// プロパティ
       property Swapch  :TVkSwapch_ read   _Swapch                ;
       property ViewerI :UInt32     read   _ViewerI write _ViewerI;
       property Viewer  :TVkFramer_ read GetViewer                ;
     end;

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TVkSwapch

     TVkSwapch<TVkDevice_:class> = class
     private
       type TVkSwapch_  = TVkSwapch<TVkDevice_>;
            TVkFramers_ = TVkFramers<TVkSwapch_>;
     protected
       _Device  :TVkDevice_;
       _Inform  :VkSwapchainCreateInfoKHR;
       _Handle  :VkSwapchainKHR;
       _Viewers :TVkFramers_;
       ///// メソッド
       procedure CreateHandle;
       procedure DestroHandle;
     public
       constructor Create( const Device_:TVkDevice_ );
       destructor Destroy; override;
       ///// プロパティ
       property Device  :TVkDevice_               read _Device ;
       property Inform  :VkSwapchainCreateInfoKHR read _Inform ;
       property Handle  :VkSwapchainKHR           read _Handle ;
       property Viewers :TVkFramers_              read _Viewers;
     end;

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TVkSwapchs

     TVkSwapchs<TVkDevice_:class> = class( TObjectList<TVkSwapch<TVkDevice_>> )
     private
       type TVkSwapch_ = TVkSwapch<TVkDevice_>;
     protected
       _Device :TVkDevice_;
     public
       constructor Create( const Device_:TVkDevice_ );
       destructor Destroy; override;
       ///// プロパティ
       property Device :TVkDevice_ read _Device;
       ///// メソッド
       function Add :TVkSwapch_; overload;
     end;

//const //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【定数】

//var //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【変数】

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【ルーチン】

implementation //############################################################### ■

uses LUX.GPU.Vulkan,
     vulkan.util;

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【レコード】

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【クラス】

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TVkFramer

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

/////////////////////////////////////////////////////////////////////// アクセス

function TVkFramer<TVkFramers_>.GetImage :VkImage;
begin
     Result := _Inform.image;
end;

/////////////////////////////////////////////////////////////////////// メソッド

procedure TVkFramer<TVkFramers_>.CreateHandle;
begin
     Assert( vkCreateImageView( TVkFramers( _Viewers ).Swapch.Device.Handle, @_Inform, nil, @_Handle ) = VK_SUCCESS );
end;

procedure TVkFramer<TVkFramers_>.DestroHandle;
begin
     vkDestroyImageView( TVkFramers( _Viewers ).Swapch.Device.Handle, _Handle, nil );
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TVkFramer<TVkFramers_>.Create( const Viewers_:TVkFramers_; const Image_:VkImage );
begin
     inherited Create;

     _Viewers  := Viewers_;

     TVkFramers( _Viewers ).Add( TVkFramer( Self ) );

     with _Inform do
     begin
          sType    := VK_STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO;
          pNext    := nil;
          flags    := 0;
          image    := Image_;
          viewType := VK_IMAGE_VIEW_TYPE_2D;
          format   := TVkFramers( _Viewers ).Swapch.Device.Format;

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

destructor TVkFramer<TVkFramers_>.Destroy;
begin
     DestroHandle;

     inherited;
end;

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TVkFramers

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

function TVkFramers<TVkSwapch_>.GetViewer :TVkFramer_;
begin
     Result := Items[ _ViewerI ];
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

procedure TVkFramers<TVkSwapch_>.FindImages;
var
   VsN, I :UInt32;
   Vs :TArray<VkImage>;
begin
     Assert( vkGetSwapchainImagesKHR( TVkSwapch( _Swapch ).Device.Handle, TVkSwapch( _Swapch ).Handle, @VsN, nil ) = VK_SUCCESS );

     Assert( VsN > 0 );

     SetLength( Vs, VsN );

     Assert( vkGetSwapchainImagesKHR( TVkSwapch( _Swapch ).Device.Handle, TVkSwapch( _Swapch ).Handle, @VsN, @Vs[0] ) = VK_SUCCESS );

     for I := 0 to VsN-1 do TVkFramer.Create( TVkFramers( Self ), Vs[I] );

     _ViewerI := 0;
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TVkFramers<TVkSwapch_>.Create( const Swapch_:TVkSwapch_ );
begin
     inherited Create;

     _Swapch := Swapch_;

     TVkSwapch( _Swapch )._Viewers := TVkFramers( Self );

     FindImages;
end;

destructor TVkFramers<TVkSwapch_>.Destroy;
begin

     inherited;
end;

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TVkSwapch

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

procedure TVkSwapch<TVkDevice_>.CreateHandle;
var
   surfCapabilities               :VkSurfaceCapabilitiesKHR;
   presentModeCount               :UInt32;
   presentModes                   :TArray<VkPresentModeKHR>;
   swapchainExtent                :VkExtent2D;
   swapchainPresentMode           :VkPresentModeKHR;
   desiredNumberOfSwapChainImages :UInt32;
   preTransform                   :VkSurfaceTransformFlagBitsKHR;
   compositeAlpha                 :VkCompositeAlphaFlagBitsKHR;
   compositeAlphaFlags            :array [ 0..4-1 ] of VkCompositeAlphaFlagBitsKHR;
   I                              :UInt32;
   queueFamilyIndices             :array [ 0..2-1 ] of UInt32;
begin
     (* DEPENDS on info.cmd and info.queue initialized *)

     Assert( vkGetPhysicalDeviceSurfaceCapabilitiesKHR( TVkDevice( _Device ).Physic, TVkDevice( _Device ).Instan.Surfacs[0].Handle, @surfCapabilities ) = VK_SUCCESS );

     Assert( vkGetPhysicalDeviceSurfacePresentModesKHR( TVkDevice( _Device ).Physic, TVkDevice( _Device ).Instan.Surfacs[0].Handle, @presentModeCount, nil ) = VK_SUCCESS );

     Assert( presentModeCount > 0 );

     SetLength( presentModes, presentModeCount );

     Assert( vkGetPhysicalDeviceSurfacePresentModesKHR( TVkDevice( _Device ).Physic, TVkDevice( _Device ).Instan.Surfacs[0].Handle, @presentModeCount, @presentModes[0] ) = VK_SUCCESS );

     // width and height are either both 0xFFFFFFFF, or both not 0xFFFFFFFF.
     if surfCapabilities.currentExtent.width = $FFFFFFFF then
     begin
          // If the surface size is undefined, the size is set to
          // the size of the images requested.
          swapchainExtent.width  := TVkDevice( _Device ).Instan.Surfacs[0].PxSizeX;
          swapchainExtent.height := TVkDevice( _Device ).Instan.Surfacs[0].PxSizeY;
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

     for I := 0 to Length( compositeAlphaFlags )-1 do
     begin
          if ( surfCapabilities.supportedCompositeAlpha and Ord( compositeAlphaFlags[I] ) ) <> 0 then
          begin
               compositeAlpha := compositeAlphaFlags[I];
               Break;
          end;
     end;

     queueFamilyIndices[0] := TVkDevice( _Device ).FamilyG;
     queueFamilyIndices[1] := TVkDevice( _Device ).FamilyP;

     with _Inform do
     begin
          sType                 := VK_STRUCTURE_TYPE_SWAPCHAIN_CREATE_INFO_KHR;
          pNext                 := nil;
          flags                 := 0;
          surface               := TVkDevice( _Device ).Instan.Surfacs[0].Handle;
          minImageCount         := desiredNumberOfSwapChainImages;
          imageFormat           := TVkDevice( _Device ).Format;
          imageColorSpace       := VK_COLOR_SPACE_SRGB_NONLINEAR_KHR;
          imageExtent.width     := swapchainExtent.width;
          imageExtent.height    := swapchainExtent.height;
          imageArrayLayers      := 1;
          imageUsage            := Ord( VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT ) or Ord( VK_IMAGE_USAGE_TRANSFER_SRC_BIT );
          imageSharingMode      := VK_SHARING_MODE_EXCLUSIVE;
          queueFamilyIndexCount := 0;
          pQueueFamilyIndices   := nil;
          preTransform          := preTransform;
          compositeAlpha        := compositeAlpha;
          presentMode           := swapchainPresentMode;
          clipped               := 1;
          oldSwapchain          := VK_NULL_HANDLE;

          if TVkDevice( _Device ).FamilyG <> TVkDevice( _Device ).FamilyP then
          begin
               // If the graphics and present queues are from different queue families,
               // we either have to explicitly transfer ownership of images between the
               // queues, or we have to create the swapchain with imageSharingMode
               // as VK_SHARING_MODE_CONCURRENT
               imageSharingMode      := VK_SHARING_MODE_CONCURRENT;
               queueFamilyIndexCount := 2;
               pQueueFamilyIndices   := @queueFamilyIndices[0];
          end;
     end;

     Assert( vkCreateSwapchainKHR( TVkDevice( _Device ).Handle, @_Inform, nil, @_Handle ) = VK_SUCCESS );
end;

procedure TVkSwapch<TVkDevice_>.DestroHandle;
begin
     vkDestroySwapchainKHR( TVkDevice( Device ).Handle, _Handle, nil );
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TVkSwapch<TVkDevice_>.Create( const Device_:TVkDevice_ );
begin
     inherited Create;

     _Device := Device_;

     TVkDevice( _Device ).Swapchs.Add( TVkSwapch( Self ) );

     CreateHandle;

     _Viewers := TVkFramers_.Create( Self );
end;

destructor TVkSwapch<TVkDevice_>.Destroy;
begin
     _Viewers.Free;

     DestroHandle;

     inherited;
end;

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TVkSwapchs

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TVkSwapchs<TVkDevice_>.Create( const Device_:TVkDevice_ );
begin
     inherited Create;

     _Device := Device_;
end;

destructor TVkSwapchs<TVkDevice_>.Destroy;
begin

     inherited;
end;

/////////////////////////////////////////////////////////////////////// メソッド

function TVkSwapchs<TVkDevice_>.Add :TVkSwapch_;
begin
     Result := TVkSwapch_.Create( _Device );
end;

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【ルーチン】

//############################################################################## □

initialization //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ 初期化

finalization //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ 最終化

end. //######################################################################### ■
