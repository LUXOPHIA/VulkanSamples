unit LUX.GPU.Vulkan.Textur;

interface //#################################################################### ■

uses System.Generics.Collections,
     vulkan_core;

type //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【型】

     TVkTexturs<TVkDevice_:class>    = class;
       TVkTextur<TVkDevice_:class>   = class;
         TVkImager<TVkDevice_:class> = class;
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

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TVkSamplr

     TVkImager<TVkDevice_:class> = class
     private
       type TVkTextur_ = TVkTextur<TVkDevice_>;
     protected
       _Textur :TVkTextur_;
       _Inform :VkImageCreateInfo;
       _Handle :VkImage;
       ///// アクセス
       function GetDevice :TVkDevice_;
       function GetHandle :VkImage;
       procedure SetHandle( const Handle_:VkImage );
       ///// メソッド
       procedure CreateHandle;
       procedure DestroHandle;
     public
       constructor Create; overload;
       constructor Create( const Textur_:TVkTextur_ ); overload;
       destructor Destroy; override;
       ///// プロパティ
       property Device :TVkDevice_        read GetDevice;
       property Textur :TVkTextur_        read   _Textur;
       property Inform :VkImageCreateInfo read   _Inform;
       property Handle :VkImage           read GetHandle write SetHandle;
     end;

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TVkTextur

     TVkTextur<TVkDevice_:class> = class
     private
       type TVkTexturs_ = TVkTexturs<TVkDevice_>;
            TVkImager_  = TVkImager<TVkDevice_>;
            TVkSamplr_  = TVkSamplr<TVkDevice_>;
     protected
       _Texturs :TVkTexturs_;
       _Imager  :TVkImager_;
       _Samplr  :TVkSamplr_;
       ///// アクセス
       function GetDevice :TVkDevice_;
       ///// メソッド
       procedure CreateHandle;
       procedure DestroHandle;
     public
       constructor Create; overload;
       constructor Create( const Texturs_:TVkTexturs_ ); overload;
       destructor Destroy; override;
       ///// プロパティ
       property Device  :TVkDevice_  read GetDevice ;
       property Texturs :TVkTexturs_ read   _Texturs;
       property Imager  :TVkImager_  read   _Imager ;
       property Samplr  :TVkSamplr_  read   _Samplr ;
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

uses vulkan.util_init,
     LUX.GPU.Vulkan;

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
     if _Handle = 0 then CreateHandle;

     Result := _Handle;
end;

procedure TVkSamplr<TVkDevice_>.SetHandle( const Handle_:VkSampler );
begin
     if _Handle <> 0 then DestroHandle;

     _Handle := Handle_;
end;

/////////////////////////////////////////////////////////////////////// メソッド

procedure TVkSamplr<TVkDevice_>.CreateHandle;
begin

end;

procedure TVkSamplr<TVkDevice_>.DestroHandle;
begin

end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TVkSamplr<TVkDevice_>.Create;
begin
     inherited Create;

     _Handle := 0;
end;

constructor TVkSamplr<TVkDevice_>.Create( const Textur_:TVkTextur_ );
begin
     inherited Create;

     _Textur := Textur_;
end;

destructor TVkSamplr<TVkDevice_>.Destroy;
begin
      Handle := 0;

     inherited;
end;

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TVkImager

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

/////////////////////////////////////////////////////////////////////// アクセス

function TVkImager<TVkDevice_>.GetDevice :TVkDevice_;
begin
     Result := _Textur.Device;
end;

//------------------------------------------------------------------------------

function TVkImager<TVkDevice_>.GetHandle :VkImage;
begin
     if _Handle = 0 then CreateHandle;

     Result := _Handle;
end;

procedure TVkImager<TVkDevice_>.SetHandle( const Handle_:VkImage );
begin
     if _Handle <> 0 then DestroHandle;

     _Handle := Handle_;
end;

/////////////////////////////////////////////////////////////////////// メソッド

procedure TVkImager<TVkDevice_>.CreateHandle;
begin

end;

procedure TVkImager<TVkDevice_>.DestroHandle;
begin

end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TVkImager<TVkDevice_>.Create;
begin
     inherited Create;

     _Handle := 0;
end;

constructor TVkImager<TVkDevice_>.Create( const Textur_:TVkTextur_ );
begin
     inherited Create;

     _Textur := Textur_;
end;

destructor TVkImager<TVkDevice_>.Destroy;
begin
      Handle := 0;

     inherited;
end;

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TVkTextur

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

/////////////////////////////////////////////////////////////////////// アクセス

function TVkTextur<TVkDevice_>.GetDevice :TVkDevice_;
begin
     Result := _Texturs.Device;
end;

/////////////////////////////////////////////////////////////////////// メソッド

procedure TVkTextur<TVkDevice_>.CreateHandle;
begin

end;

procedure TVkTextur<TVkDevice_>.DestroHandle;
begin

end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TVkTextur<TVkDevice_>.Create;
begin
     inherited;

     _Imager := TVkImager_.Create( Self );
     _Samplr := TVkSamplr_.Create( Self );
end;

constructor TVkTextur<TVkDevice_>.Create( const Texturs_:TVkTexturs_ );
begin
     Create;

     _Texturs := Texturs_;

     TVkTexturs( _Texturs ).Add( TVkTextur( Self ) );
end;

destructor TVkTextur<TVkDevice_>.Destroy;
begin
     _Imager.Free;
     _Samplr.Free;

     inherited;
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