unit LUX.GPU.Vulkan.Textur;

interface //#################################################################### ■

uses System.Generics.Collections,
     vulkan_core,
     LUX.GPU.Vulkan.Imager;

type //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【型】

     TVkTexturs<TVkDevice_:class>    = class;
       TVkTextur<TVkDevice_:class>   = class;
         TVkTexIma<TVkDevice_:class> = class;
         TVkSamplr<TVkDevice_:class> = class;

     //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【レコード】

     //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【クラス】

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TVkSamplr

     TVkSamplr<TVkDevice_:class> = class
     private
       type TVkTextur_ = TVkTextur<TVkDevice_>;
     protected
       _Textur :TVkTextur_;
       _Inform :VkSamplerCreateInfo;
       _Handle :VkSampler;
       ///// アクセス
       function GetDevice :TVkDevice_;
       function GetHandle :VkSampler;
       procedure SetHandle( const Handle_:VkSampler );
       ///// メソッド
       procedure CreateHandle;
       procedure DestroHandle;
     public
       constructor Create; overload;
       constructor Create( const Textur_:TVkTextur_ ); overload;
       destructor Destroy; override;
       ///// プロパティ
       property Device :TVkDevice_          read GetDevice;
       property Textur :TVkTextur_          read   _Textur;
       property Inform :VkSamplerCreateInfo read   _Inform;
       property Handle :VkSampler           read GetHandle write SetHandle;
     end;

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TVkTexIma

     TVkTexIma<TVkDevice_:class> = class( TVkImager<TVkDevice_,TVkTextur<TVkDevice_>> )
     private
     protected
       ///// アクセス
       function GetDevice :TVkDevice_; override;
     public
     end;

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TVkTextur

     TVkTextur<TVkDevice_:class> = class
     private
       type TVkTexturs_ = TVkTexturs<TVkDevice_>;
            TVkTexIma_  = TVkTexIma <TVkDevice_>;
            TVkSamplr_  = TVkSamplr <TVkDevice_>;
     protected
       _Texturs :TVkTexturs_;
       _Imager  :TVkTexIma_;
       _Samplr  :TVkSamplr_;
       _Descri  :VkDescriptorImageInfo;
       _Handle  :P_VkDescriptorImageInfo;
       ///// アクセス
       function GetDevice :TVkDevice_;
       function GetHandle :P_VkDescriptorImageInfo;
       procedure SetHandle( const Handle_:P_VkDescriptorImageInfo );
       ///// メソッド
       procedure CreateHandle;
       procedure DestroHandle;
     public
       constructor Create; overload;
       constructor Create( const Texturs_:TVkTexturs_ ); overload;
       constructor Create( const Device_:TVkDevice_ ); overload;
       destructor Destroy; override;
       ///// プロパティ
       property Device  :TVkDevice_              read GetDevice ;
       property Texturs :TVkTexturs_             read   _Texturs;
       property Imager  :TVkTexIma_              read   _Imager ;
       property Samplr  :TVkSamplr_              read   _Samplr ;
       property Descri  :VkDescriptorImageInfo   read   _Descri ;
       property Handle  :P_VkDescriptorImageInfo read GetHandle write SetHandle;
       ///// メソッド
       procedure LoadFromFile( const FileName_:String );
     end;

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TVkTexturs

     TVkTexturs<TVkDevice_:class> = class( TObjectList<TVkTextur<TVkDevice_>> )
     private
       type TVkTextur_ = TVkTextur<TVkDevice_>;
     protected
       _Device :TVkDevice_;
     public
       constructor Create( const Device_:TVkDevice_ );
       destructor Destroy; override;
       ///// プロパティ
       property Device :TVkDevice_ read _Device;
       ///// メソッド
       function Add :TVkTextur_; overload;
     end;

//const //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【定数】

//var //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【変数】

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【ルーチン】

implementation //############################################################### ■

uses LUX.GPU.Vulkan;

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【レコード】

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【クラス】

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TVkSamplr

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

/////////////////////////////////////////////////////////////////////// アクセス

function TVkSamplr<TVkDevice_>.GetDevice :TVkDevice_;
begin
     Result := _Textur.Device;
end;

//------------------------------------------------------------------------------

function TVkSamplr<TVkDevice_>.GetHandle :VkSampler;
begin
     if _Handle = VK_NULL_HANDLE then CreateHandle;

     Result := _Handle;
end;

procedure TVkSamplr<TVkDevice_>.SetHandle( const Handle_:VkSampler );
begin
     if _Handle <> VK_NULL_HANDLE then DestroHandle;

     _Handle := Handle_;
end;

/////////////////////////////////////////////////////////////////////// メソッド

procedure TVkSamplr<TVkDevice_>.CreateHandle;
begin
     Assert( vkCreateSampler( TVkDevice( Device ).Handle, @_Inform, nil, @_Handle ) = VK_SUCCESS );
end;

procedure TVkSamplr<TVkDevice_>.DestroHandle;
begin
     vkDestroySampler( TVkDevice( Device ).Handle, _Handle, nil );
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TVkSamplr<TVkDevice_>.Create;
begin
     inherited;

     _Handle := VK_NULL_HANDLE;

     with _Inform do
     begin
          sType                   := VK_STRUCTURE_TYPE_SAMPLER_CREATE_INFO;
          pNext                   := nil;
          flags                   := 0;
          magFilter               := VK_FILTER_NEAREST;
          minFilter               := VK_FILTER_NEAREST;
          mipmapMode              := VK_SAMPLER_MIPMAP_MODE_NEAREST;
          addressModeU            := VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE;
          addressModeV            := VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE;
          addressModeW            := VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE;
          mipLodBias              := 0.0;
          anisotropyEnable        := VK_FALSE;
          maxAnisotropy           := 1;
          compareEnable           := VK_FALSE;
          compareOp               := VK_COMPARE_OP_NEVER;
          minLod                  := 0.0;
          maxLod                  := 0.0;
          borderColor             := VK_BORDER_COLOR_FLOAT_OPAQUE_WHITE;
          unnormalizedCoordinates := 0;
     end;
end;

constructor TVkSamplr<TVkDevice_>.Create( const Textur_:TVkTextur_ );
begin
     Create;

     _Textur := Textur_;
end;

destructor TVkSamplr<TVkDevice_>.Destroy;
begin
      Handle := VK_NULL_HANDLE;

     inherited;
end;

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TVkTexIma

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

/////////////////////////////////////////////////////////////////////// アクセス

function TVkTexIma<TVkDevice_>.GetDevice :TVkDevice_;
begin
     Result := _Parent.Device;
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TVkTextur

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

/////////////////////////////////////////////////////////////////////// アクセス

function TVkTextur<TVkDevice_>.GetDevice :TVkDevice_;
begin
     Result := _Texturs.Device;
end;

//------------------------------------------------------------------------------

function TVkTextur<TVkDevice_>.GetHandle :P_VkDescriptorImageInfo;
begin
     if not Assigned( _Handle ) then CreateHandle;

     Result := _Handle;
end;

procedure TVkTextur<TVkDevice_>.SetHandle( const Handle_:P_VkDescriptorImageInfo );
begin
     if Assigned( _Handle ) then DestroHandle;

     _Handle := Handle_;
end;

/////////////////////////////////////////////////////////////////////// メソッド

procedure TVkTextur<TVkDevice_>.CreateHandle;
begin
     with _Descri do
     begin
          imageView   := _Imager.Viewer.Handle;
          sampler     := _Samplr       .Handle;
          imageLayout := VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL;
     end;

     _Handle := @_Descri;
end;

procedure TVkTextur<TVkDevice_>.DestroHandle;
begin
     _Imager.Viewer.Handle := VK_NULL_HANDLE;
     _Samplr       .Handle := VK_NULL_HANDLE;
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TVkTextur<TVkDevice_>.Create;
begin
     inherited;

     _Handle := nil;

     _Imager := TVkTexIma_.Create( Self );
     _Samplr := TVkSamplr_.Create( Self );
end;

constructor TVkTextur<TVkDevice_>.Create( const Texturs_:TVkTexturs_ );
begin
     Create;

     _Texturs := Texturs_;

     _Texturs.Add( Self );
end;

constructor TVkTextur<TVkDevice_>.Create( const Device_:TVkDevice_ );
begin
     Create( TVkTexturs_( TVkDevice( Device_ ).Texturs ) );
end;

destructor TVkTextur<TVkDevice_>.Destroy;
begin
     _Samplr.Free;
     _Imager.Free;

      Handle := nil;

     inherited;
end;

/////////////////////////////////////////////////////////////////////// メソッド

procedure TVkTextur<TVkDevice_>.LoadFromFile( const FileName_:String );
begin
     _Imager.LoadFromFile( FileName_ );
end;

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TVkTexturs

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TVkTexturs<TVkDevice_>.Create( const Device_:TVkDevice_ );
begin
     inherited Create;

     _Device := Device_;
end;

destructor TVkTexturs<TVkDevice_>.Destroy;
begin

     inherited;
end;

/////////////////////////////////////////////////////////////////////// メソッド

function TVkTexturs<TVkDevice_>.Add :TVkTextur_;
begin
     Result := TVkTextur_.Create( Self );
end;

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【ルーチン】

//############################################################################## □

initialization //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ 初期化

finalization //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ 最終化

end. //######################################################################### ■