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
       _Handle  :VkCommandPool;
       _Commans :TVkCommans_;
       /////
       procedure CreateHandle;
       procedure DestroHandle;
     public
       constructor Create( const Device_:TVkDevice_ );
       destructor Destroy; override;
       ///// プロパティ
       property Device  :TVkDevice_    read _Device ;
       property Handle  :VkCommandPool read _Handle ;
       property Commans :TVkCommans_   read _Commans;
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

procedure TVkPooler<TVkDevice_>.CreateHandle;
var
   P :VkCommandPoolCreateInfo;
begin
     (* DEPENDS on init_swapchain_extension() *)

     with P do
     begin
          sType            := VK_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO;
          pNext            := nil;
          queueFamilyIndex := TVkDevice( _Device ).FamilyG;
          flags            := Ord( VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT );
     end;

     Assert( vkCreateCommandPool( TVkDevice( _Device ).Handle, @P, nil, @Handle ) = VK_SUCCESS );
end;

procedure TVkPooler<TVkDevice_>.DestroHandle;
begin
     vkDestroyCommandPool( TVkDevice( _Device ).Handle, Handle, nil );
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TVkPooler<TVkDevice_>.Create( const Device_:TVkDevice_ );
begin
     inherited Create;

     _Device := Device_;

     TVkDevice( _Device ).Poolers.Add( TVkPooler( Self ) );

     CreateHandle;

     _Commans := TVkCommans_.Create( Self );
end;

destructor TVkPooler<TVkDevice_>.Destroy;
begin
     _Commans.Free;

     DestroHandle;

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