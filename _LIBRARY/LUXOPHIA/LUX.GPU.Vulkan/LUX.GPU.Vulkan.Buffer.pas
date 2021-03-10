unit LUX.GPU.Vulkan.Buffer;

interface //#################################################################### ■

uses System.Generics.Collections,
     vulkan_core, vulkan_win32;

type //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【型】

     //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【レコード】

     //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【クラス】

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TVkBuffer

     TVkBuffer<TDevice_:class> = class
     private
     protected
       _Device :TDevice_;
       _Handle :VkBuffer;
       _Memory :VkDeviceMemory;
       _Descri :VkDescriptorBufferInfo;
       /////
       procedure CreateHandle;
       procedure DestroHandle;
     public
       constructor Create( const Device_:TDevice_ );
       procedure AfterConstruction; override;
       destructor Destroy; override;
       ///// プロパティ
       property Device :TDevice_               read _Device;
       property Handle :VkBuffer               read _Handle;
       property Memory :VkDeviceMemory         read _Memory;
       property Descri :VkDescriptorBufferInfo read _Descri;
     end;

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TVkBuffers

     TVkBuffers<TDevice_:class> = class( TObjectList<TVkBuffer<TDevice_>> )
     private
       type TVkBuffer_ = TVkBuffer<TDevice_>;
     protected
       _Device :TDevice_;
     public
       constructor Create( const Device_:TDevice_ );
       destructor Destroy; override;
       ///// プロパティ
       property Pooler :TDevice_ read _Device;
       ///// メソッド
       function Add :TVkBuffer_; overload;
     end;

//const //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【定数】

//var //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【変数】

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【ルーチン】

implementation //############################################################### ■

uses System.Math,
     LUX, LUX.D1, LUX.D2, LUX.D3, LUX.D4, LUX.D4x4,
     LUX.GPU.Vulkan;

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【レコード】

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【クラス】

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TVkBuffer

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

procedure TVkBuffer<TDevice_>.CreateHandle;
var
   Projection :TSingleM4;
   View       :TSingleM4;
   Model      :TSingleM4;
   Clip       :TSingleM4;
   MVP        :TSingleM4;

   res        :VkResult;
   pass       :Boolean;
   fov        :Single;
   buf_info   :VkBufferCreateInfo;
   mem_reqs   :VkMemoryRequirements;
   alloc_info :VkMemoryAllocateInfo;
   pData      :PByte;
begin
     fov := DegToRad( 45 );
     Projection := TSingleM4.ProjPersH( fov, 1, 0.1, 100 );
     View := TSingleM4.LookAt( TSingle3D.Create( -5, +3, -10 ),    // Camera is at (-5,3,-10), in World Space
                               TSingle3D.Create(  0,  0,   0 ),    // and looks at the origin
                               TSingle3D.Create(  0, -1,   0 ) );  // Head is up (set to 0,-1,0 to look upside-down)

     Model := TSingleM4.Identity;
     // Vulkan clip space has inverted Y and half Z.
     Clip := TSingleM4.Create( +1.0,  0.0,  0.0,  0.0,
                                0.0, -1.0,  0.0,  0.0,
                                0.0,  0.0, +0.5, +0.5,
                                0.0,  0.0,  0.0, +1.0 );

     MVP := Clip * Projection *View * Model;

     (* VULKAN_KEY_START *)
     buf_info                       := Default( VkBufferCreateInfo );
     buf_info.sType                 := VK_STRUCTURE_TYPE_BUFFER_CREATE_INFO;
     buf_info.pNext                 := nil;
     buf_info.usage                 := Ord( VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT );
     buf_info.size                  := SizeOf( MVP );
     buf_info.queueFamilyIndexCount := 0;
     buf_info.pQueueFamilyIndices   := nil;
     buf_info.sharingMode           := VK_SHARING_MODE_EXCLUSIVE;
     buf_info.flags                 := 0;
     res := vkCreateBuffer( TVkDevice( _Device ).Handle, @buf_info, nil, @_Handle );
     Assert( res = VK_SUCCESS );

     vkGetBufferMemoryRequirements( TVkDevice( _Device ).Handle, _Handle, @mem_reqs );

     alloc_info                 := Default( VkMemoryAllocateInfo );
     alloc_info.sType           := VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO;
     alloc_info.pNext           := nil;
     alloc_info.memoryTypeIndex := 0;
     alloc_info.allocationSize := mem_reqs.size;
     pass := TVkDevice( _Device ).memory_type_from_properties( mem_reqs.memoryTypeBits,
                                          Ord( VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT ) or Ord( VK_MEMORY_PROPERTY_HOST_COHERENT_BIT ),
                                          alloc_info.memoryTypeIndex );
     Assert( pass, 'No mappable, coherent memory' );

     res := vkAllocateMemory( TVkDevice( _Device ).Handle, @alloc_info, nil, @_Memory );
     Assert( res = VK_SUCCESS );

     res := vkMapMemory( TVkDevice( _Device ).Handle, _Memory, 0, mem_reqs.size, 0, @pData );
     Assert( res = VK_SUCCESS );

     Move( MVP, pData^, SizeOf( MVP ) );

     vkUnmapMemory( TVkDevice( _Device ).Handle, _Memory );

     res := vkBindBufferMemory( TVkDevice( _Device ).Handle, _Handle, _Memory, 0 );
     Assert( res = VK_SUCCESS );

     _Descri.buffer := _Handle;
     _Descri.offset := 0;
     _Descri.range  := SizeOf( MVP );
end;

procedure TVkBuffer<TDevice_>.DestroHandle;
begin
     vkFreeMemory   ( TVkDevice( _Device ).Handle, _Memory, nil );
     vkDestroyBuffer( TVkDevice( _Device ).Handle, _Handle, nil );
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TVkBuffer<TDevice_>.Create( const Device_:TDevice_ );
begin
     inherited Create;

     _Device := Device_;

     TVkDevice( _Device ).Buffers.Add( TVkBuffer( Self ) );

     CreateHandle;
end;

procedure TVkBuffer<TDevice_>.AfterConstruction;
begin
     inherited;

end;

destructor TVkBuffer<TDevice_>.Destroy;
begin
     DestroHandle;

     inherited;
end;

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TVkBuffers

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TVkBuffers<TDevice_>.Create( const Device_:TDevice_ );
begin
     inherited Create;

     _Device := Device_;
end;

destructor TVkBuffers<TDevice_>.Destroy;
begin

     inherited;
end;

/////////////////////////////////////////////////////////////////////// メソッド

function TVkBuffers<TDevice_>.Add :TVkBuffer_;
begin
     Result := TVkBuffer_.Create( _Device );
end;

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【ルーチン】

//############################################################################## □

initialization //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ 初期化

finalization //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ 最終化

end. //######################################################################### ■