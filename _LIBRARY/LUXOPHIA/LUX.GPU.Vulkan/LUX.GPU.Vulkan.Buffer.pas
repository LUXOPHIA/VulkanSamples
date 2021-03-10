unit LUX.GPU.Vulkan.Buffer;

interface //#################################################################### ■

uses System.Generics.Collections,
     vulkan_core, vulkan_win32;

type //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【型】

     TVkBuffers<TDevice_:class>      = class;
       TVkBuffer<TDevice_:class>     = class;
         TVkMemory<TVkDevice_:class> = class;

     //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【レコード】

     //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【クラス】

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TVkMemory

     TVkMemory<TVkDevice_:class> = class
     private
       type TVkBuffer_ = TVkBuffer<TVkDevice_>;
     protected
       _Buffer :TVkBuffer_;
       _Requir :VkMemoryRequirements;
       _Inform :VkMemoryAllocateInfo;
       _Handle :VkDeviceMemory;
       ///// アクセス
       function GetDevice :TVkDevice_;
       ///// メソッド
       procedure CreateHandle;
       procedure DestroHandle;
     public
       constructor Create( const Buffer_:TVkBuffer_ );
       destructor Destroy; override;
       ///// プロパティ
       property Device :TVkDevice_           read GetDevice;
       property Buffer :TVkBuffer_           read   _Buffer;
       property Requir :VkMemoryRequirements read   _Requir;
       property Inform :VkMemoryAllocateInfo read   _Inform;
       property Handle :VkDeviceMemory       read   _Handle;
       ///// メソッド
       function Map( var Pointer_:PByte ) :Boolean;
       procedure Unmap;
     end;

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TVkBuffer

     TVkBuffer<TDevice_:class> = class
     private
       type TVkMemory_ = TVkMemory<TDevice_>;
     protected
       _Device :TDevice_;
       _Inform :VkBufferCreateInfo;
       _Handle :VkBuffer;
       _Descri :VkDescriptorBufferInfo;
       _Memory :TVkMemory_;
       ///// メソッド
       procedure CreateHandle;
       procedure DestroHandle;
     public
       constructor Create( const Device_:TDevice_ );
       destructor Destroy; override;
       ///// プロパティ
       property Device :TDevice_               read   _Device;
       property Inform :VkBufferCreateInfo     read   _Inform;
       property Handle :VkBuffer               read   _Handle;
       property Descri :VkDescriptorBufferInfo read   _Descri;
       property Memory :TVkMemory_             read   _Memory;
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

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TVkMemory

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

/////////////////////////////////////////////////////////////////////// アクセス

function TVkMemory<TVkDevice_>.GetDevice :TVkDevice_;
begin
     Result := _Buffer.Device;
end;

/////////////////////////////////////////////////////////////////////// メソッド

procedure TVkMemory<TVkDevice_>.CreateHandle;
var
   pass :Boolean;
begin
     vkGetBufferMemoryRequirements( TVkDevice( Device ).Handle, TVkBuffer( _Buffer ).Handle, @_Requir );

     _Inform                 := Default( VkMemoryAllocateInfo );
     _Inform.sType           := VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO;
     _Inform.pNext           := nil;
     _Inform.memoryTypeIndex := 0;
     _Inform.allocationSize  := _Requir.size;

     pass := TVkDevice( Device ).memory_type_from_properties(
                  _Requir.memoryTypeBits,
                  Ord( VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT ) or Ord( VK_MEMORY_PROPERTY_HOST_COHERENT_BIT ),
                  _Inform.memoryTypeIndex );
     Assert( pass, 'No mappable, coherent memory' );

     Assert( vkAllocateMemory( TVkDevice( Device ).Handle, @_Inform, nil, @_Handle ) = VK_SUCCESS );

     Assert( vkBindBufferMemory( TVkDevice( Device ).Handle, TVkBuffer( _Buffer ).Handle, _Handle, 0 ) = VK_SUCCESS );
end;

procedure TVkMemory<TVkDevice_>.DestroHandle;
begin
     vkFreeMemory( TVkDevice( Device ).Handle, _Handle, nil );
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TVkMemory<TVkDevice_>.Create( const Buffer_:TVkBuffer_ );
begin
     inherited Create;

     _Buffer := Buffer_;

     CreateHandle;
end;

destructor TVkMemory<TVkDevice_>.Destroy;
begin
     DestroHandle;

     inherited;
end;

/////////////////////////////////////////////////////////////////////// メソッド

function TVkMemory<TVkDevice_>.Map( var Pointer_:PByte ) :Boolean;
begin
     Result := vkMapMemory( TVkDevice( Device ).Handle, Handle, 0, _Requir.size, 0, @Pointer_ ) = VK_SUCCESS;
end;

procedure TVkMemory<TVkDevice_>.Unmap;
begin
     vkUnmapMemory( TVkDevice( Device ).Handle, Handle );
end;

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TVkBuffer

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

/////////////////////////////////////////////////////////////////////// メソッド

procedure TVkBuffer<TDevice_>.CreateHandle;
var
   fov        :Single;
   Projection :TSingleM4;
   View       :TSingleM4;
   Model      :TSingleM4;
   Clip       :TSingleM4;
   MVP        :TSingleM4;
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

     _Inform                       := Default( VkBufferCreateInfo );
     _Inform.sType                 := VK_STRUCTURE_TYPE_BUFFER_CREATE_INFO;
     _Inform.pNext                 := nil;
     _Inform.usage                 := Ord( VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT );
     _Inform.size                  := SizeOf( MVP );
     _Inform.queueFamilyIndexCount := 0;
     _Inform.pQueueFamilyIndices   := nil;
     _Inform.sharingMode           := VK_SHARING_MODE_EXCLUSIVE;
     _Inform.flags                 := 0;

     Assert( vkCreateBuffer( TVkDevice( _Device ).Handle, @_Inform, nil, @_Handle ) = VK_SUCCESS );

     _Descri.buffer := _Handle;
     _Descri.offset := 0;
     _Descri.range  := SizeOf( MVP );

     _Memory := TVkMemory_.Create( Self );

     _Memory.Map( pData );

     Move( MVP, pData^, SizeOf( MVP ) );

     _Memory.Unmap;
end;

procedure TVkBuffer<TDevice_>.DestroHandle;
begin
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

destructor TVkBuffer<TDevice_>.Destroy;
begin
     _Memory.Free;

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