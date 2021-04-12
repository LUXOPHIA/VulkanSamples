unit LUX.GPU.Vulkan.Layere;

interface //#################################################################### ■

uses LUX.Data.List,
     vulkan_core;

type //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【型】

     TVkLayeres<TVulkan_:class>    = class;
       TVkLayere<TVulkan_:class>   = class;
         TVkExtenss                = TArray<VkExtensionProperties>;
     TVkDevLays<TVkDevice_:class>  = class;
       TVkDevLay<TVkDevice_:class> = class;

     //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【レコード】

     //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【クラス】

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TVkLayere

     TVkLayere<TVulkan_:class> = class( TListChildr<TVulkan_,TVkLayeres<TVulkan_>> )
     private
       type TVkLayeres_ = TVkLayeres<TVulkan_>;
     protected
       _Inform   :VkLayerProperties;
       _ExtenssN :Integer;
       _Extenss  :TVkExtenss;
       ///// メソッド
       function FindExtenss :VkResult;
     public
       constructor Create( const Layeres_:TVkLayeres_; const Inform_:VkLayerProperties ); overload; virtual;
       destructor Destroy; override;
       ///// プロパティ
       property Vulkan   :TVulkan_          read GetOwnere  ;
       property Layeres  :TVkLayeres_       read GetParent  ;
       property Inform   :VkLayerProperties read   _Inform  ;
       property ExtenssN :Integer           read   _ExtenssN;
       property Extenss  :TVkExtenss        read   _Extenss ;
     end;

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TVkLayeres

     TVkLayeres<TVulkan_:class> = class( TListParent<TVulkan_,TVkLayere<TVulkan_>> )
     private
       type TVkLayere_ = TVkLayere<TVulkan_>;
     protected
       ///// メソッド
       function FindLayeres :VkResult;
     public
       constructor Create( const Vulkan_:TVulkan_ ); override;
       destructor Destroy; override;
       ///// プロパティ
       property Vulkan :TVulkan_ read GetOwnere;
       ///// メソッド
       function Add( const Inform_:VkLayerProperties ) :TVkLayere_; overload;
     end;

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TVkDevLay

     TVkDevLay<TVkDevice_:class> = class( TListChildr<TVkDevice_,TVkDevLays<TVkDevice_>> )
     private
       type TVkDevLays_ = TVkDevLays<TVkDevice_>;
     protected
       _LayereI  :Integer;
       _ExtenssN :Integer;
       _Extenss  :TVkExtenss;
       ///// メソッド
       function FindExtenss :VkResult;
     public
       constructor Create( const DevLays_:TVkDevLays_; const LayereI_:Integer ); overload; virtual;
       destructor Destroy; override;
       ///// プロパティ
       property Device   :TVkDevice_  read GetOwnere  ;
       property DevLays  :TVkDevLays_ read GetParent  ;
       property ExtenssN :Integer     read   _ExtenssN;
       property Extenss  :TVkExtenss  read   _Extenss ;
     end;

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TVkDevLays

     TVkDevLays<TVkDevice_:class> = class( TListParent<TVkDevice_,TVkDevLay<TVkDevice_>> )
     private
       type TVkDevLay_ = TVkDevLay<TVkDevice_>;
     protected
       ///// メソッド
       procedure FindDevLays;
     public
       ///// プロパティ
       property Device :TVkDevice_ read GetOwnere;
       ///// メソッド
       function Add( const LayereI_:Integer ) :TVkDevLay_; overload;
     end;

//const //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【定数】

//var //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【変数】

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【ルーチン】

implementation //############################################################### ■

uses LUX.GPU.Vulkan;

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【レコード】

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【クラス】

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TVkLayere

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

/////////////////////////////////////////////////////////////////////// メソッド

function TVkLayere<TVulkan_>.FindExtenss :VkResult;
begin
     repeat
           Result := vkEnumerateInstanceExtensionProperties( _Inform.layerName, @_ExtenssN, nil );
           if Result <> VK_SUCCESS then Exit;

           if _ExtenssN = 0 then Exit( VK_SUCCESS );

           SetLength( _Extenss, _ExtenssN );
           Result := vkEnumerateInstanceExtensionProperties( _Inform.layerName, @_ExtenssN, @_Extenss[0] );

     until Result <> VK_INCOMPLETE;
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TVkLayere<TVulkan_>.Create( const Layeres_:TVkLayeres_; const Inform_:VkLayerProperties );
begin
     inherited Create( Layeres_ );

     _Inform  := Inform_ ;

     FindExtenss;
end;

destructor TVkLayere<TVulkan_>.Destroy;
begin

     inherited;
end;

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TVkLayeres

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

/////////////////////////////////////////////////////////////////////// メソッド

function TVkLayeres<TVulkan_>.FindLayeres :VkResult;
var
   LsN, I :UInt32;
   Ls :TArray<VkLayerProperties>;
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
     for I := 0 to LsN-1 do Add( Ls[I] );
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TVkLayeres<TVulkan_>.Create( const Vulkan_:TVulkan_ );
begin
     inherited Create;

     FindLayeres;
end;

destructor TVkLayeres<TVulkan_>.Destroy;
begin

     inherited;
end;

/////////////////////////////////////////////////////////////////////// メソッド

function TVkLayeres<TVulkan_>.Add( const Inform_:VkLayerProperties ) :TVkLayere_;
begin
     Result := TVkLayere_.Create( Self, Inform_ );
end;

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TVkDevLay

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

/////////////////////////////////////////////////////////////////////// メソッド

function TVkDevLay<TVkDevice_>.FindExtenss :VkResult;
var
   D :TVkDevice;
   C :PAnsiChar;
begin
     D := TVkDevice( Device );
     C := D.Instan.Vulkan.Layeres[ _LayereI ].Inform.layerName;

     repeat
           Result := vkEnumerateDeviceExtensionProperties( D.Physic, C, @_ExtenssN, nil );
           if Result <> VK_SUCCESS then Exit;

           if _ExtenssN = 0 then Exit( VK_SUCCESS );

           SetLength( _Extenss, _ExtenssN );
           Result := vkEnumerateDeviceExtensionProperties( D.Physic, C, @_ExtenssN, @_Extenss[0] );

     until Result <> VK_INCOMPLETE;
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TVkDevLay<TVkDevice_>.Create( const DevLays_:TVkDevLays_; const LayereI_:Integer );
begin
     inherited Create( DevLays_ );

     _LayereI := LayereI_;

     FindExtenss;
end;

destructor TVkDevLay<TVkDevice_>.Destroy;
begin

     inherited;
end;

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TVkDevLays

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

/////////////////////////////////////////////////////////////////////// メソッド

procedure TVkDevLays<TVkDevice_>.FindDevLays;
var
   Ls :TVkLayeres;
   I :Integer;
begin
     Ls := TVkDevice( Device ).Instan.Vulkan.Layeres;

     (* query device extensions for enabled layers *)
     for I := 0 to Ls.ChildrsN-1 do Add( I );
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

/////////////////////////////////////////////////////////////////////// メソッド

function TVkDevLays<TVkDevice_>.Add( const LayereI_:Integer ) :TVkDevLay_;
begin
     Result := TVkDevLay_.Create( Self, LayereI_ );
end;

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【ルーチン】

//############################################################################## □

initialization //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ 初期化

finalization //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ 最終化

end. //######################################################################### ■