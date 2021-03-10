unit LUX.GPU.Vulkan.Framer;

interface //#################################################################### ■

uses System.Generics.Collections,
     vulkan_core;

type //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【型】

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

//const //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【定数】

//var //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【変数】

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【ルーチン】

implementation //############################################################### ■

uses LUX.GPU.Vulkan;

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

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【ルーチン】

//############################################################################## □

initialization //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ 初期化

finalization //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ 最終化

end. //######################################################################### ■
