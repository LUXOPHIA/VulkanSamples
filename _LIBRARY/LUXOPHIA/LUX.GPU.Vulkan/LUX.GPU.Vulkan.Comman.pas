unit LUX.GPU.Vulkan.Comman;

interface //#################################################################### ■

uses vulkan_core, vulkan_win32;

type //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【型】

     TVkPooler<TVkDevice_:class> = class;
     TVkComman<TVkPooler_:class> = class;

     //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【レコード】

     //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【クラス】

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TVkPooler

     TVkPooler<TVkDevice_:class> = class
     private
       type TVkPooler_ = TVkPooler<TVkDevice_>;
            TVkComman_ = TVkComman<TVkPooler_>;
     protected
       _Device :TVkDevice_;
       _Handle :VkCommandPool;
       _Comman :TVkComman_;
       /////
       procedure CreateHandle;
       procedure DestroHandle;
     public
       constructor Create( const Device_:TVkDevice_ );
       destructor Destroy; override;
       ///// プロパティ
       property Device :TVkDevice_    read _Device              ;
       property Handle :VkCommandPool read _Handle              ;
       property Comman :TVkComman_    read _Comman write _Comman;
     end;

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

     TVkDevice( _Device ).Pooler := TVkPooler( Self );

     CreateHandle;
end;

destructor TVkPooler<TVkDevice_>.Destroy;
begin
     DestroHandle;

     inherited;
end;

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