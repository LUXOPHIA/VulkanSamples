unit LUX.GPU.Vulkan.Swapch;

interface //#################################################################### ■

uses System.Generics.Collections,
     vulkan_core,
     LUX.GPU.Vulkan.Framer;

type //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【型】

     TVkSwapchs<TVkDevice_:class>  = class;
       TVkSwapch<TVkDevice_:class> = class;

     //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【レコード】

     //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【クラス】

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TVkSwapch

     TVkSwapch<TVkDevice_:class> = class
     private
       type TVkSwapch_  = TVkSwapch<TVkDevice_>;
            TVkFramers_ = TVkFramers<TVkSwapch_>;
     protected
       _Device  :TVkDevice_;
       _Inform  :VkSwapchainCreateInfoKHR;
       _Handle  :VkSwapchainKHR;
       _Framers :TVkFramers_;
       ///// アクセス
       function GetHandle :VkSwapchainKHR;
       procedure SetHandle( const Handle_:VkSwapchainKHR );
       function GetHandleP :P_VkSwapchainKHR;
       ///// メソッド
       procedure CreateHandle;
       procedure DestroHandle;
     public
       constructor Create( const Device_:TVkDevice_ );
       destructor Destroy; override;
       ///// プロパティ
       property Device  :TVkDevice_               read   _Device ;
       property Inform  :VkSwapchainCreateInfoKHR read   _Inform ;
       property Handle  :VkSwapchainKHR           read GetHandle  write SetHandle;
       property HandleP :P_VkSwapchainKHR         read GetHandleP;
       property Framers :TVkFramers_              read   _Framers;
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

uses LUX.GPU.Vulkan;

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【レコード】

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【クラス】

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TVkSwapch

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

/////////////////////////////////////////////////////////////////////// アクセス

function TVkSwapch<TVkDevice_>.GetHandle :VkSwapchainKHR;
begin
     if _Handle = 0 then CreateHandle;

     Result := _Handle;
end;

procedure TVkSwapch<TVkDevice_>.SetHandle( const Handle_:VkSwapchainKHR );
begin
     if _Handle <> 0 then DestroHandle;

     _Handle := Handle_;
end;

function TVkSwapch<TVkDevice_>.GetHandleP :P_VkSwapchainKHR;
begin
     if _Handle = 0 then CreateHandle;

     Result := @_Handle;
end;

/////////////////////////////////////////////////////////////////////// メソッド

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

     _Handle := 0;

     _Device := Device_;

     TVkDevice( _Device ).Swapchs.Add( TVkSwapch( Self ) );

     _Framers := TVkFramers_.Create( Self );
end;

destructor TVkSwapch<TVkDevice_>.Destroy;
begin
     _Framers.Free;

      Handle := 0;

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
