unit LUX.GPU.Vulkan.Comman;

interface //#################################################################### ■

uses System.Generics.Collections,
     vulkan_core;

type //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【型】

     TVkCommans<TVkPooler_:class>  = class;
       TVkComman<TVkPooler_:class> = class;

     //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【レコード】

     //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【クラス】

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TVkComman

     TVkComman<TVkPooler_:class> = class
     private
     protected
       _Pooler :TVkPooler_;
       _Handle :VkCommandBuffer;
       ///// メソッド
       procedure CreateHandle;
       procedure DestroHandle;
     public
       constructor Create( const Pooler_:TVkPooler_ );
       destructor Destroy; override;
       ///// プロパティ
       property Pooler :TVkPooler_      read _Pooler;
       property Handle :VkCommandBuffer read _Handle;
       ///// メソッド
       procedure BeginRecord;
       procedure EndRecord;
     end;

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TVkCommans

     TVkCommans<TVkPooler_:class> = class( TObjectList<TVkComman<TVkPooler_>> )
     private
       type TVkComman_ = TVkComman<TVkPooler_>;
     protected
       _Pooler :TVkPooler_;
     public
       constructor Create( const Pooler_:TVkPooler_ );
       destructor Destroy; override;
       ///// プロパティ
       property Pooler :TVkPooler_ read _Pooler;
       ///// メソッド
       function Add :TVkComman_; overload;
     end;

//const //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【定数】

//var //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【変数】

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【ルーチン】

implementation //############################################################### ■

uses LUX.GPU.Vulkan;

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【レコード】

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【クラス】

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TVkComman

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

/////////////////////////////////////////////////////////////////////// メソッド

procedure TVkComman<TVkPooler_>.CreateHandle;
var
   B :VkCommandBufferAllocateInfo;
begin
     (* DEPENDS on init_swapchain_extension() and init_command_pool() *)

     with B do
     begin
          sType              := VK_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO;
          pNext              := nil;
          commandPool        := TVkPooler( _Pooler ).Handle;
          level              := VK_COMMAND_BUFFER_LEVEL_PRIMARY;
          commandBufferCount := 1;
     end;

     Assert( vkAllocateCommandBuffers( TVkPooler( _Pooler ).Device.Handle, @B, @_Handle ) = VK_SUCCESS );
end;

procedure TVkComman<TVkPooler_>.DestroHandle;
var
   Bs :array [ 0..1-1 ] of VkCommandBuffer;
begin
     Bs[0] := _Handle;
     vkFreeCommandBuffers( TVkPooler( _Pooler ).Device.Handle, TVkPooler( _Pooler ).Handle, 1, @Bs[0] );
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TVkComman<TVkPooler_>.Create( const Pooler_:TVkPooler_ );
begin
     inherited Create;

     _Pooler := Pooler_;

     TVkPooler( _Pooler ).Commans.Add( TVkComman( Self ) );

     CreateHandle;
end;

destructor TVkComman<TVkPooler_>.Destroy;
begin
     DestroHandle;

     inherited;
end;

/////////////////////////////////////////////////////////////////////// メソッド

procedure TVkComman<TVkPooler_>.BeginRecord;
var
   B :VkCommandBufferBeginInfo;
begin
     (* DEPENDS on init_command_buffer() *)

     with B do
     begin
          sType            := VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO;
          pNext            := nil;
          flags            := 0;
          pInheritanceInfo := nil;
     end;

     Assert( vkBeginCommandBuffer( _Handle, @B ) = VK_SUCCESS );
end;

procedure TVkComman<TVkPooler_>.EndRecord;
begin
     Assert( vkEndCommandBuffer( _Handle ) = VK_SUCCESS );
end;

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TVkCommans

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TVkCommans<TVkPooler_>.Create( const Pooler_:TVkPooler_ );
begin
     inherited Create;

     _Pooler := Pooler_;
end;

destructor TVkCommans<TVkPooler_>.Destroy;
begin

     inherited;
end;

/////////////////////////////////////////////////////////////////////// メソッド

function TVkCommans<TVkPooler_>.Add :TVkComman_;
begin
     Result := TVkComman_.Create( _Pooler );
end;
//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【ルーチン】

//############################################################################## □

initialization //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ 初期化

finalization //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ 最終化

end. //######################################################################### ■