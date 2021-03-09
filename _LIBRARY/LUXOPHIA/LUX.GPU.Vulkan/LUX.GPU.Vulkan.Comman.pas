unit LUX.GPU.Vulkan.Comman;

interface //#################################################################### ■

uses vulkan_core, vulkan_win32;

type //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【型】

     TVkCommandPool<TDevice_:class>          = class;
     TVkCommandBuffer<TVkCommandPool_:class> = class;

     //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【レコード】

     //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【クラス】

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TVkCommandPool

     TVkCommandPool<TDevice_:class> = class
     private
       type TVkCommandPool_   = TVkCommandPool<TDevice_>;
            TVkCommandBuffer_ = TVkCommandBuffer<TVkCommandPool_>;
     protected
       _Device  :TDevice_;
       _Handle  :VkCommandPool;
       _ComBufs :TVkCommandBuffer_;
       /////
       procedure CreateHandle;
       procedure DestroHandle;
     public
       constructor Create( const Device_:TDevice_ );
       procedure AfterConstruction; override;
       destructor Destroy; override;
       ///// プロパティ
       property Device  :TDevice_          read _Device                ;
       property Handle  :VkCommandPool     read _Handle                ;
       property ComBufs :TVkCommandBuffer_ read _ComBufs write _ComBufs;
     end;

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TVkCommandBuffer

     TVkCommandBuffer<TVkCommandPool_:class> = class
     private
     protected
       _ComPool :TVkCommandPool_;
       _Handle  :VkCommandBuffer;
       ///// メソッド
       procedure CreateHandle;
       procedure DestroHandle;
     public
       constructor Create( const ComPool_:TVkCommandPool_ );
       procedure AfterConstruction; override;
       destructor Destroy; override;
       ///// プロパティ
       property Pool   :TVkCommandPool_ read _ComPool;
       property Handle :VkCommandBuffer read _Handle ;
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

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TVkCommandPool

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

procedure TVkCommandPool<TDevice_>.CreateHandle;
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

procedure TVkCommandPool<TDevice_>.DestroHandle;
begin
     vkDestroyCommandPool( TVkDevice( _Device ).Handle, Handle, nil );
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TVkCommandPool<TDevice_>.Create( const Device_:TDevice_ );
begin
     inherited Create;

     _Device := Device_;

     TVkDevice( _Device ).Pooler := TVkCommandPool( Self );

     CreateHandle;
end;

procedure TVkCommandPool<TDevice_>.AfterConstruction;
begin
     inherited;

end;

destructor TVkCommandPool<TDevice_>.Destroy;
begin
     DestroHandle;

     inherited;
end;

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TVkCommandBuffer

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

/////////////////////////////////////////////////////////////////////// メソッド

procedure TVkCommandBuffer<TVkCommandPool_>.CreateHandle;
var
   B :VkCommandBufferAllocateInfo;
begin
     (* DEPENDS on init_swapchain_extension() and init_command_pool() *)

     with B do
     begin
          sType              := VK_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO;
          pNext              := nil;
          commandPool        := TVkCommandPool( _ComPool ).Handle;
          level              := VK_COMMAND_BUFFER_LEVEL_PRIMARY;
          commandBufferCount := 1;
     end;

     Assert( vkAllocateCommandBuffers( TVkCommandPool( _ComPool ).Device.Handle, @B, @_Handle ) = VK_SUCCESS );
end;

procedure TVkCommandBuffer<TVkCommandPool_>.DestroHandle;
var
   Bs :array [ 0..1-1 ] of VkCommandBuffer;
begin
     Bs[0] := _Handle;
     vkFreeCommandBuffers( TVkCommandPool( _ComPool ).Device.Handle, TVkCommandPool( _ComPool ).Handle, 1, @Bs[0] );
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TVkCommandBuffer<TVkCommandPool_>.Create( const ComPool_:TVkCommandPool_ );
begin
     inherited Create;

     _ComPool := ComPool_;

     TVkCommandPool( _ComPool ).ComBufs := TVkCommandBuffer( Self );

     CreateHandle;
end;

procedure TVkCommandBuffer<TVkCommandPool_>.AfterConstruction;
begin
     inherited;

end;

destructor TVkCommandBuffer<TVkCommandPool_>.Destroy;
begin
     DestroHandle;

     inherited;
end;

/////////////////////////////////////////////////////////////////////// メソッド

procedure TVkCommandBuffer<TVkCommandPool_>.BeginRecord;
var
   cmd_buf_info :VkCommandBufferBeginInfo;
begin
     (* DEPENDS on init_command_buffer() *)

     cmd_buf_info                  := Default( VkCommandBufferBeginInfo );
     cmd_buf_info.sType            := VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO;
     cmd_buf_info.pNext            := nil;
     cmd_buf_info.flags            := 0;
     cmd_buf_info.pInheritanceInfo := nil;

     Assert( vkBeginCommandBuffer( _Handle, @cmd_buf_info ) = VK_SUCCESS );
end;

procedure TVkCommandBuffer<TVkCommandPool_>.EndRecord;
begin
     Assert( vkEndCommandBuffer( _Handle ) = VK_SUCCESS );
end;

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【ルーチン】

//############################################################################## □

initialization //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ 初期化

finalization //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ 最終化

end. //######################################################################### ■