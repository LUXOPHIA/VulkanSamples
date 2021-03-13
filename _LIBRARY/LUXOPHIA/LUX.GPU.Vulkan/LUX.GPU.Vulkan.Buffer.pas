unit LUX.GPU.Vulkan.Buffer;

interface //#################################################################### ■

uses System.Generics.Collections,
     vulkan_core,
     LUX.GPU.Vulkan.root;

type //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【型】

     TVkBuffers<TVkDevice_:class>    = class;
       TVkBuffer<TVkDevice_:class>   = class;
         TVkMemory<TVkDevice_:class> = class;

     //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【レコード】

     //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【クラス】

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TVkMemory

     TVkMemory<TVkDevice_:class> = class( TVkDeviceChildr<TVkDevice_,TVkBuffer<TVkDevice_>> )
     private
       type TVkBuffer_ = TVkBuffer<TVkDevice_>;
     protected
       _Inform :VkMemoryAllocateInfo;
       _Handle :VkDeviceMemory;
       ///// アクセス
       function GetDevice :TVkDevice_; override;
       function GetHandle :VkDeviceMemory;
       procedure SetHandle( const Handle_:VkDeviceMemory );
       ///// メソッド
       procedure CreateHandle;
       procedure DestroHandle;
     public
       constructor Create; override;
       destructor Destroy; override;
       ///// プロパティ
       property Device :TVkDevice_           read GetDevice;
       property Buffer :TVkBuffer_           read GetParent;
       property Inform :VkMemoryAllocateInfo read   _Inform;
       property Handle :VkDeviceMemory       read GetHandle write SetHandle;
       ///// メソッド
       function Bind :Boolean;
       function Map( var Pointer_:PByte ) :Boolean;
       procedure Unmap;
     end;

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TVkBuffer

     TVkBuffer<TVkDevice_:class> = class
     private
       type TVkMemory_ = TVkMemory<TVkDevice_>;
     protected
       _Device :TVkDevice_;
       _Inform :VkBufferCreateInfo;
       _Handle :VkBuffer;
       _Descri :VkDescriptorBufferInfo;
       _Memory :TVkMemory_;
       ///// アクセス
       function GetHandle :VkBuffer;
       procedure SetHandle( const Handle_:VkBuffer );
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
       property Handle :VkBuffer               read GetHandle write SetHandle;
       property Descri :VkDescriptorBufferInfo read   _Descri;
       property Memory :TVkMemory_             read   _Memory;
       ///// メソッド
       function GetRequir :VkMemoryRequirements;
     end;

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TVkBuffers

     TVkBuffers<TVkDevice_:class> = class( TObjectList<TVkBuffer<TVkDevice_>> )
     private
       type TVkBuffer_ = TVkBuffer<TVkDevice_>;
     protected
       _Device :TVkDevice_;
     public
       constructor Create( const Device_:TVkDevice_ );
       destructor Destroy; override;
       ///// プロパティ
       property Device :TVkDevice_ read _Device;
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
     Result := Buffer.Device;
end;

//------------------------------------------------------------------------------

function TVkMemory<TVkDevice_>.GetHandle :VkDeviceMemory;
begin
     if _Handle = 0 then
     begin
          CreateHandle;

          Assert( Bind );
     end;

     Result := _Handle;
end;

procedure TVkMemory<TVkDevice_>.SetHandle( const Handle_:VkDeviceMemory );
begin
     if _Handle <> 0 then DestroHandle;

     _Handle := Handle_;
end;

/////////////////////////////////////////////////////////////////////// メソッド

procedure TVkMemory<TVkDevice_>.CreateHandle;
var
   R :VkMemoryRequirements;
begin
     R := TVkBuffer( Buffer ).GetRequir;

     _Inform                 := Default( VkMemoryAllocateInfo );
     _Inform.sType           := VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO;
     _Inform.pNext           := nil;
     _Inform.memoryTypeIndex := 0;
     _Inform.allocationSize  := R.size;

     Assert( TVkDevice( Device ).memory_type_from_properties( R.memoryTypeBits,
                  Ord( VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT ) or Ord( VK_MEMORY_PROPERTY_HOST_COHERENT_BIT ),
                  _Inform.memoryTypeIndex ),
             'No mappable, coherent memory' );

     Assert( vkAllocateMemory( TVkDevice( Device ).Handle, @_Inform, nil, @_Handle ) = VK_SUCCESS );
end;

procedure TVkMemory<TVkDevice_>.DestroHandle;
begin
     vkFreeMemory( TVkDevice( Device ).Handle, _Handle, nil );
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TVkMemory<TVkDevice_>.Create;
begin
     inherited;

     _Handle := VK_NULL_HANDLE;
end;

destructor TVkMemory<TVkDevice_>.Destroy;
begin
      Handle := VK_NULL_HANDLE;

     inherited;
end;

/////////////////////////////////////////////////////////////////////// メソッド

function TVkMemory<TVkDevice_>.Bind :Boolean;
begin
     Result := vkBindBufferMemory( TVkDevice( Device ).Handle, TVkBuffer( Buffer ).Handle, Handle, 0 ) = VK_SUCCESS;
end;

//------------------------------------------------------------------------------

function TVkMemory<TVkDevice_>.Map( var Pointer_:PByte ) :Boolean;
begin
     Result := vkMapMemory( TVkDevice( Device ).Handle, Handle, 0, _Inform.allocationSize, 0, @Pointer_ ) = VK_SUCCESS;
end;

procedure TVkMemory<TVkDevice_>.Unmap;
begin
     vkUnmapMemory( TVkDevice( Device ).Handle, Handle );
end;

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TVkBuffer

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

/////////////////////////////////////////////////////////////////////// アクセス

function TVkBuffer<TVkDevice_>.GetHandle :VkBuffer;
begin
     if _Handle = 0 then
     begin
          CreateHandle;

          Assert( _Memory.Bind );
     end;

     Result := _Handle;
end;

procedure TVkBuffer<TVkDevice_>.SetHandle( const Handle_:VkBuffer );
begin
     if _Handle <> 0 then
     begin
          _Memory.Handle := 0;

          DestroHandle;
     end;

     _Handle := Handle_;
end;

/////////////////////////////////////////////////////////////////////// メソッド

procedure TVkBuffer<TVkDevice_>.CreateHandle;
begin
     Assert( vkCreateBuffer( TVkDevice( _Device ).Handle, @_Inform, nil, @_Handle ) = VK_SUCCESS );

     with _Descri do
     begin
          buffer := _Handle;
          offset := 0;
          range  := _Inform.size;
     end;
end;

procedure TVkBuffer<TVkDevice_>.DestroHandle;
begin
     vkDestroyBuffer( TVkDevice( _Device ).Handle, _Handle, nil );
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TVkBuffer<TVkDevice_>.Create;
begin
     inherited;

     _Handle := 0;

     _Memory := TVkMemory_.Create( Self );

     with _Inform do
     begin
          sType                 := VK_STRUCTURE_TYPE_BUFFER_CREATE_INFO;
          pNext                 := nil;
          flags                 := 0;
          size                  := SizeOf( TSingleM4 );
          usage                 := Ord( VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT );
          sharingMode           := VK_SHARING_MODE_EXCLUSIVE;
          queueFamilyIndexCount := 0;
          pQueueFamilyIndices   := nil;
     end;
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

      Handle := 0;

     inherited;
end;

/////////////////////////////////////////////////////////////////////// メソッド

function TVkBuffer<TVkDevice_>.GetRequir :VkMemoryRequirements;
begin
     vkGetBufferMemoryRequirements( TVkDevice( Device ).Handle, Handle, @Result );
end;

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TVkBuffers

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TVkBuffers<TVkDevice_>.Create( const Device_:TVkDevice_ );
begin
     inherited Create;

     _Device := Device_;
end;

destructor TVkBuffers<TVkDevice_>.Destroy;
begin

     inherited;
end;

/////////////////////////////////////////////////////////////////////// メソッド

function TVkBuffers<TVkDevice_>.Add :TVkBuffer_;
begin
     Result := TVkBuffer_.Create( _Device );
end;

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【ルーチン】

//############################################################################## □

initialization //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ 初期化

finalization //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ 最終化

end. //######################################################################### ■