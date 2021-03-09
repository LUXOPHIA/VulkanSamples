unit LUX.GPU.Vulkan.Pooler;

interface //#################################################################### ■

uses System.Generics.Collections,
     vulkan_core,
     LUX.GPU.Vulkan.Comman;

type //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【型】

     TVkPoolers<TVkDevice_:class>  = class;
       TVkPooler<TVkDevice_:class> = class;

     //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【レコード】

     //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【クラス】

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TVkPooler

     TVkPooler<TVkDevice_:class> = class
     private
       type TVkPooler_  = TVkPooler<TVkDevice_>;
            TVkCommans_ = TVkCommans<TVkPooler_>;
     protected
       _Device  :TVkDevice_;
       _Inform  :VkCommandPoolCreateInfo;
       _Handle  :VkCommandPool;
       _Commans :TVkCommans_;
       ///// アクセス
       function GetHandle :VkCommandPool;
       procedure SetHandle( const Handle_:VkCommandPool );
       ///// メソッド
       procedure CreateHandle;
       procedure DestroHandle;
     public
       constructor Create; overload;
       constructor Create( const Device_:TVkDevice_ ); overload;
       destructor Destroy; override;
       ///// プロパティ
       property Device  :TVkDevice_              read   _Device                ;
       property Inform  :VkCommandPoolCreateInfo read   _Inform                ;
       property Handle  :VkCommandPool           read GetHandle write SetHandle;
       property Commans :TVkCommans_             read   _Commans               ;
     end;

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TVkPoolers

     TVkPoolers<TVkDevice_:class> = class( TObjectList<TVkPooler<TVkDevice_>> )
     private
       type TVkPooler_ = TVkPooler<TVkDevice_>;
     protected
       _Device :TVkDevice_;
     public
       constructor Create( const Device_:TVkDevice_ );
       destructor Destroy; override;
       ///// プロパティ
       property Device :TVkDevice_ read _Device;
       ///// メソッド
       function Add :TVkPooler_; overload;
     end;

//const //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【定数】

//var //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【変数】

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【ルーチン】

implementation //############################################################### ■

uses LUX.GPU.Vulkan;

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【レコード】

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【クラス】

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TVkPooler

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

/////////////////////////////////////////////////////////////////////// アクセス

function TVkPooler<TVkDevice_>.GetHandle :VkCommandPool;
begin
     if _Handle = 0 then CreateHandle;

     Result := _Handle;
end;

procedure TVkPooler<TVkDevice_>.SetHandle( const Handle_:VkCommandPool );
begin
     if _Handle <> 0 then DestroHandle;

     _Handle := Handle_;
end;

/////////////////////////////////////////////////////////////////////// メソッド

procedure TVkPooler<TVkDevice_>.CreateHandle;
begin
     Assert( vkCreateCommandPool( TVkDevice( _Device ).Handle, @_Inform, nil, @_Handle ) = VK_SUCCESS );
end;

procedure TVkPooler<TVkDevice_>.DestroHandle;
begin
     vkDestroyCommandPool( TVkDevice( _Device ).Handle, _Handle, nil );
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TVkPooler<TVkDevice_>.Create;
begin
     inherited Create;

     _Handle := 0;
end;

constructor TVkPooler<TVkDevice_>.Create( const Device_:TVkDevice_ );
begin
     Create;

     _Device := Device_;

     TVkDevice( _Device ).Poolers.Add( TVkPooler( Self ) );

     with _Inform do
     begin
          sType            := VK_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO;
          pNext            := nil;
          queueFamilyIndex := TVkDevice( _Device ).FamilyG;
          flags            := Ord( VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT );
     end;

     _Commans := TVkCommans_.Create( Self );
end;

destructor TVkPooler<TVkDevice_>.Destroy;
begin
     _Commans.Free;

      Handle := 0;

     inherited;
end;

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TVkPoolers

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TVkPoolers<TVkDevice_>.Create( const Device_:TVkDevice_ );
begin
     inherited Create;

     _Device := Device_;
end;

destructor TVkPoolers<TVkDevice_>.Destroy;
begin

     inherited;
end;

/////////////////////////////////////////////////////////////////////// メソッド

function TVkPoolers<TVkDevice_>.Add :TVkPooler_;
begin
     Result := TVkPooler_.Create( _Device );
end;

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【ルーチン】

//############################################################################## □

initialization //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ 初期化

finalization //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ 最終化

end. //######################################################################### ■