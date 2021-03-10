unit LUX.GPU.Vulkan.Framer;

interface //#################################################################### ■

uses System.Generics.Collections,
     vulkan_core;

type //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【型】

     TVkFramers<TVkSwapch_:class>   = class;
       TVkFramer<TVkSwapch_:class> = class;

     //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【レコード】

     //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【クラス】

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TVkFramer

     TVkFramer<TVkSwapch_:class> = class
     private
       type TVkFramers_ = TVkFramers<TVkSwapch_>;
     protected
       _Framers :TVkFramers_;
       _Inform  :VkImageViewCreateInfo;
       _Handle  :VkImageView;
       ///// アクセス
       function GetSwapch :TVkSwapch_;
       function GetImage :VkImage;
       function GetHandle :VkImageView;
       procedure SetHandle( const Handle_:VkImageView );
       ///// メソッド
       procedure CreateHandle;
       procedure DestroHandle;
     public
       constructor Create( const Framers_:TVkFramers_; const Image_:VkImage );
       destructor Destroy; override;
       ///// プロパティ
       property Swapch  :TVkSwapch_            read GetSwapch ;
       property Framers :TVkFramers_           read   _Framers;
       property Inform  :VkImageViewCreateInfo read   _Inform ;
       property Image   :VkImage               read GetImage  ;
       property Handle  :VkImageView           read GetHandle  write SetHandle;
     end;

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TVkFramers

     TVkFramers<TVkSwapch_:class> = class( TObjectList<TVkFramer<TVkSwapch_>> )
     private
       type TVkFramer_ = TVkFramer<TVkSwapch_>;
     protected
       _Swapch  :TVkSwapch_;
       _FramerI :UInt32;
       ///// アクセス
       function GetFramer :TVkFramer_;
       ///// メソッド
       procedure FindImages;
     public
       constructor Create( const Swapch_:TVkSwapch_ );
       destructor Destroy; override;
       ///// プロパティ
       property Swapch  :TVkSwapch_ read   _Swapch                ;
       property FramerI :UInt32     read   _FramerI write _FramerI;
       property Framer  :TVkFramer_ read GetFramer                ;
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

function TVkFramer<TVkSwapch_>.GetSwapch :TVkSwapch_;
begin
     Result := _Framers.Swapch;
end;

//------------------------------------------------------------------------------

function TVkFramer<TVkSwapch_>.GetImage :VkImage;
begin
     Result := _Inform.image;
end;

//------------------------------------------------------------------------------

function TVkFramer<TVkSwapch_>.GetHandle :VkImageView;
begin
     if _Handle = 0 then CreateHandle;

     Result := _Handle;
end;

procedure TVkFramer<TVkSwapch_>.SetHandle( const Handle_:VkImageView );
begin
     if _Handle <> 0 then DestroHandle;

     _Handle := Handle_;
end;

/////////////////////////////////////////////////////////////////////// メソッド

procedure TVkFramer<TVkSwapch_>.CreateHandle;
begin
     Assert( vkCreateImageView( TVkSwapch( Swapch ).Device.Handle, @_Inform, nil, @_Handle ) = VK_SUCCESS );
end;

procedure TVkFramer<TVkSwapch_>.DestroHandle;
begin
     vkDestroyImageView( TVkSwapch( Swapch ).Device.Handle, _Handle, nil );
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TVkFramer<TVkSwapch_>.Create( const Framers_:TVkFramers_; const Image_:VkImage );
begin
     inherited Create;

     _Handle := 0;

     _Framers := Framers_;

     _Framers.Add( Self );

     with _Inform do
     begin
          sType    := VK_STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO;
          pNext    := nil;
          flags    := 0;
          image    := Image_;
          viewType := VK_IMAGE_VIEW_TYPE_2D;
          format   := TVkFramers( _Framers ).Swapch.Device.Format;

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
end;

destructor TVkFramer<TVkSwapch_>.Destroy;
begin
      Handle := 0;

     inherited;
end;

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TVkFramers

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

function TVkFramers<TVkSwapch_>.GetFramer :TVkFramer_;
begin
     Result := Items[ _FramerI ];
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

     _FramerI := 0;
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TVkFramers<TVkSwapch_>.Create( const Swapch_:TVkSwapch_ );
begin
     inherited Create;

     _Swapch := Swapch_;

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
