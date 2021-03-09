unit LUX.GPU.Vulkan.Comman;

interface //#################################################################### ■

uses vulkan_core, vulkan_win32;

type //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【型】

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

     TVkPooler( _Pooler ).Comman := TVkComman( Self );

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

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【ルーチン】

//############################################################################## □

initialization //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ 初期化

finalization //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ 最終化

end. //######################################################################### ■