unit LUX.GPU.Vulkan.Buffer;

interface //#################################################################### ■

uses System.Generics.Collections,
     vulkan_core,
     LUX.GPU.Vulkan.root,
     LUX.GPU.Vulkan.Memory;

type //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【型】

     TVkBuffers<TVkDevice_:class>    = class;
       TVkBuffer<TVkDevice_:class>   = class;
         TVkBufMem<TVkDevice_:class> = class;

     //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【レコード】

     //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【クラス】

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TVkBufMem

     TVkBufMem<TVkDevice_:class> = class( TVkMemory<TVkDevice_,TVkBuffer<TVkDevice_>> )
     private
       type TVkBuffer_ = TVkBuffer<TVkDevice_>;
     protected
       ///// アクセス
       function GetDevice :TVkDevice_; override;
       function GetSize :VkDeviceSize; override;
       function GetTypeI :UInt32; override;
       ///// メソッド
       procedure CreateHandle; override;
     public
       ///// プロパティ
       property Buffer :TVkBuffer_   read GetParent;
       property Size   :VkDeviceSize read GetSize  ;
       property TypeI  :UInt32       read GetTypeI ;
     end;

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TVkBuffer

     TVkBuffer<TVkDevice_:class> = class
     private
       type TVkBufMem_ = TVkBufMem<TVkDevice_>;
     protected
       _Device :TVkDevice_;
       _Inform :VkBufferCreateInfo;
       _Handle :VkBuffer;
       _Memory :TVkBufMem_;
       ///// アクセス
       function GetSize :VkDeviceSize; virtual;
       procedure SetSize( const Size_:VkDeviceSize ); virtual;
       function GetHandle :VkBuffer; virtual;
       procedure SetHandle( const Handle_:VkBuffer ); virtual;
       function GetDescri :VkDescriptorBufferInfo; virtual;
       ///// メソッド
       procedure CreateHandle;
       procedure DestroHandle;
     public
       constructor Create; overload;
       constructor Create( const Device_:TVkDevice_ ); overload;
       destructor Destroy; override;
       ///// プロパティ
       property Device :TVkDevice_             read   _Device;
       property Inform :VkBufferCreateInfo     read   _Inform;
       property Size   :VkDeviceSize           read GetSize   write SetSize  ;
       property Handle :VkBuffer               read GetHandle write SetHandle;
       property Descri :VkDescriptorBufferInfo read GetDescri;
       property Memory :TVkBufMem_             read   _Memory;
       ///// メソッド
       function GetRequir :VkMemoryRequirements;
       function Bind( const Memory_:TVkBufMem_ ) :Boolean;
     end;

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TVkBuffer<TVkDevice_:class;TItemer_:>


     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TVkBuffers

     TVkBuffers<TVkDevice_:class> = class( TVkDeviceLister<TVkDevice_,TVkBuffer<TVkDevice_>> )
     private
       type TVkBuffer_ = TVkBuffer<TVkDevice_>;
     protected
     public
       ///// メソッド
       function Add :TVkBuffer_; overload;
       procedure FreeHandles;
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

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TVkBufMem

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

/////////////////////////////////////////////////////////////////////// アクセス

function TVkBufMem<TVkDevice_>.GetDevice :TVkDevice_;
begin
     Result := Buffer.Device;
end;

//------------------------------------------------------------------------------

function TVkBufMem<TVkDevice_>.GetSize :VkDeviceSize;
begin
     Result := Buffer.GetRequir.size;
end;

//------------------------------------------------------------------------------

function TVkBufMem<TVkDevice_>.GetTypeI :UInt32;
begin
     Assert( TVkDevice( Device ).memory_type_from_properties(
                  Buffer.GetRequir.memoryTypeBits,
                  Ord( VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT ) or Ord( VK_MEMORY_PROPERTY_HOST_COHERENT_BIT ),
                  Result ),
             'No mappable, coherent memory' );
end;

/////////////////////////////////////////////////////////////////////// メソッド

procedure TVkBufMem<TVkDevice_>.CreateHandle;
begin
     inherited;

     Buffer.Bind( Self );
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TVkBuffer

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

/////////////////////////////////////////////////////////////////////// アクセス

function TVkBuffer<TVkDevice_>.GetSize :VkDeviceSize;
begin
     Result := _Inform.size;
end;

procedure TVkBuffer<TVkDevice_>.SetSize( const Size_:VkDeviceSize );
begin
     _Inform.size := Size_;

     Handle := VK_NULL_HANDLE;
end;

//------------------------------------------------------------------------------

function TVkBuffer<TVkDevice_>.GetHandle :VkBuffer;
begin
     if _Handle = VK_NULL_HANDLE then CreateHandle;

     Result := _Handle;
end;

procedure TVkBuffer<TVkDevice_>.SetHandle( const Handle_:VkBuffer );
begin
     if _Handle <> VK_NULL_HANDLE then DestroHandle;

     _Handle := Handle_;
end;

//------------------------------------------------------------------------------

function TVkBuffer<TVkDevice_>.GetDescri :VkDescriptorBufferInfo;
begin
     with Result do
     begin
          buffer := Handle;
          offset := 0;
          range  := Size;
     end;
end;

/////////////////////////////////////////////////////////////////////// メソッド

procedure TVkBuffer<TVkDevice_>.CreateHandle;
begin
     Assert( vkCreateBuffer( TVkDevice( _Device ).Handle, @_Inform, nil, @_Handle ) = VK_SUCCESS );

     Assert( Bind( _Memory ) );
end;

procedure TVkBuffer<TVkDevice_>.DestroHandle;
begin
     _Memory.Handle := VK_NULL_HANDLE;

     vkDestroyBuffer( TVkDevice( _Device ).Handle, _Handle, nil );
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TVkBuffer<TVkDevice_>.Create;
begin
     inherited;

     _Handle := VK_NULL_HANDLE;

     _Memory := TVkBufMem_.Create( Self );

     with _Inform do
     begin
          sType                 := VK_STRUCTURE_TYPE_BUFFER_CREATE_INFO;
          pNext                 := nil;
          flags                 := 0;
       // size                  :
          usage                 := Ord( VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT );
          sharingMode           := VK_SHARING_MODE_EXCLUSIVE;
          queueFamilyIndexCount := 0;
          pQueueFamilyIndices   := nil;
     end;

     Size := SizeOf( TSingleM4 );
end;

constructor TVkBuffer<TVkDevice_>.Create( const Device_:TVkDevice_ );
var
   fov        :Single;
   Projection :TSingleM4;
   View       :TSingleM4;
   Model      :TSingleM4;
   Clip       :TSingleM4;
   MVP        :TSingleM4;
   pData      :PByte;
begin
     Create;

     _Device := Device_;

     TVkDevice( _Device ).Buffers.Add( TVkBuffer( Self ) );

     //////////////////////////////

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

     _Memory.Map( pData );

     Move( MVP, pData^, SizeOf( MVP ) );

     _Memory.Unmap;
end;

destructor TVkBuffer<TVkDevice_>.Destroy;
begin
     _Memory.Free;

      Handle := VK_NULL_HANDLE;

     inherited;
end;

/////////////////////////////////////////////////////////////////////// メソッド

function TVkBuffer<TVkDevice_>.GetRequir :VkMemoryRequirements;
begin
     vkGetBufferMemoryRequirements( TVkDevice( Device ).Handle, Handle, @Result );
end;

//------------------------------------------------------------------------------

function TVkBuffer<TVkDevice_>.Bind( const Memory_:TVkBufMem_ ) :Boolean;
begin
     Result := vkBindBufferMemory( TVkDevice( Device ).Handle, Handle, Memory_.Handle, 0 ) = VK_SUCCESS;
end;

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TVkBuffers

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

/////////////////////////////////////////////////////////////////////// メソッド

function TVkBuffers<TVkDevice_>.Add :TVkBuffer_;
begin
     Result := TVkBuffer_.Create( Device );
end;

//------------------------------------------------------------------------------

procedure TVkBuffers<TVkDevice_>.FreeHandles;
var
   B :TVkBuffer_;
begin
     for B in Self do B.Handle := VK_NULL_HANDLE;
end;

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【ルーチン】

end. //######################################################################### ■