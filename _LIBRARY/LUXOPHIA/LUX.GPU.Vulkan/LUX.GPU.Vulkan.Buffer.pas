unit LUX.GPU.Vulkan.Buffer;

interface //#################################################################### ■

uses vulkan_core,
     LUX.GPU.Vulkan.root,
     LUX.GPU.Vulkan.Memory;

type //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【型】

     TVkBuffers<TVkDevice_:class>               = class;
       TVkBuffer<TVkDevice_,TVkParent_:class>   = class;
         TVkBufMem<TVkDevice_,TVkParent_:class> = class;

       TVkBuffer<TVkDevice_:class>              = class;

     //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【レコード】

     //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【クラス】

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TVkBufMem<TVkDevice_,TVkParent_>

     TVkBufMem<TVkDevice_,TVkParent_:class> = class( TVkMemory<TVkDevice_,TVkBuffer<TVkDevice_,TVkParent_>> )
     private
       type TVkBuffer_ = TVkBuffer<TVkDevice_,TVkParent_>;
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

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TVkBuffer<TVkDevice_,TVkParent_>

     TVkBuffer<TVkDevice_,TVkParent_:class> = class( TVkDeviceObject<TVkDevice_,TVkParent_> )
     private
       type TVkBufMem_ = TVkBufMem <TVkDevice_,TVkParent_>;
     protected
       _Inform :VkBufferCreateInfo;
       _Handle :VkBuffer;
       _Memory :TVkBufMem_;
       ///// アクセス
       function GetSize :VkDeviceSize; virtual;
       procedure SetSize( const Size_:VkDeviceSize ); virtual;
       function GetUsage :VkBufferUsageFlags; virtual;
       procedure SetUsage( const Usage_:VkBufferUsageFlags ); virtual;
       function GetSharingMode :VkSharingMode; virtual;
       procedure SetSharingMode( const SharingMode_:VkSharingMode ); virtual;
       function GetHandle :VkBuffer; virtual;
       procedure SetHandle( const Handle_:VkBuffer ); virtual;
       function GetDescri :VkDescriptorBufferInfo; virtual;
       function GetRequir :VkMemoryRequirements; virtual;
       ///// メソッド
       procedure CreateHandle;
       procedure DestroHandle;
     public
       constructor Create; override;
       destructor Destroy; override;
       ///// プロパティ
       property Inform      :VkBufferCreateInfo     read   _Inform                          ;
       property Size        :VkDeviceSize           read GetSize        write SetSize       ;
       property Usage       :VkBufferUsageFlags     read GetUsage       write SetUsage      ;
       property SharingMode :VkSharingMode          read GetSharingMode write SetSharingMode;
       property Handle      :VkBuffer               read GetHandle      write SetHandle     ;
       property Descri      :VkDescriptorBufferInfo read GetDescri                          ;
       property Requir      :VkMemoryRequirements   read GetRequir                          ;
       property Memory      :TVkBufMem_             read   _Memory                          ;
       ///// メソッド
       function Bind( const Memory_:TVkBufMem_ ) :Boolean;
     end;

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TVkBuffer<TVkDevice_>

     TVkBuffer<TVkDevice_:class> = class( TVkBuffer<TVkDevice_,TVkBuffers<TVkDevice_>> )
     private
       type TVkBuffers_ = TVkBuffers<TVkDevice_>;
     protected
       ///// アクセス
       function GetDevice :TVkDevice_; override;
     public
       constructor Create( const Buffers_:TVkBuffers_ ); override;
       constructor Create( const Device_:TVkDevice_ ); overload; virtual;
       ///// プロパティ
       property Buffers :TVkBuffers_ read GetParent;
     end;

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TVkBuffers<TVkDevice_>

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

uses LUX.GPU.Vulkan;

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【レコード】

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【クラス】

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TVkBufMem<TVkDevice_,TVkParent_>

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

/////////////////////////////////////////////////////////////////////// アクセス

function TVkBufMem<TVkDevice_,TVkParent_>.GetDevice :TVkDevice_;
begin
     Result := Buffer.Device;
end;

//------------------------------------------------------------------------------

function TVkBufMem<TVkDevice_,TVkParent_>.GetSize :VkDeviceSize;
begin
     Result := Buffer.Requir.size;
end;

//------------------------------------------------------------------------------

function TVkBufMem<TVkDevice_,TVkParent_>.GetTypeI :UInt32;
begin
     Assert( TVkDevice( Device ).memory_type_from_properties(
                  Buffer.Requir.memoryTypeBits,
                  Ord( VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT ) or Ord( VK_MEMORY_PROPERTY_HOST_COHERENT_BIT ),
                  Result ),
             'No mappable, coherent memory' );
end;

/////////////////////////////////////////////////////////////////////// メソッド

procedure TVkBufMem<TVkDevice_,TVkParent_>.CreateHandle;
begin
     inherited;

     Buffer.Bind( Self );
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TVkBuffer<TVkDevice_,TVkParent_>

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

/////////////////////////////////////////////////////////////////////// アクセス

function TVkBuffer<TVkDevice_,TVkParent_>.GetSize :VkDeviceSize;
begin
     Result := _Inform.size;
end;

procedure TVkBuffer<TVkDevice_,TVkParent_>.SetSize( const Size_:VkDeviceSize );
begin
     _Inform.size := Size_;

     Handle := VK_NULL_HANDLE;
end;

//------------------------------------------------------------------------------

function TVkBuffer<TVkDevice_,TVkParent_>.GetUsage :VkBufferUsageFlags;
begin
     Result := _Inform.usage;
end;

procedure TVkBuffer<TVkDevice_,TVkParent_>.SetUsage( const Usage_:VkBufferUsageFlags );
begin
     _Inform.usage := Usage_;

     Handle := VK_NULL_HANDLE;
end;

//------------------------------------------------------------------------------

function TVkBuffer<TVkDevice_,TVkParent_>.GetSharingMode :VkSharingMode;
begin
     Result := _Inform.sharingMode;
end;

procedure TVkBuffer<TVkDevice_,TVkParent_>.SetSharingMode( const SharingMode_:VkSharingMode );
begin
     _Inform.sharingMode := SharingMode_;

     Handle := VK_NULL_HANDLE;
end;

//------------------------------------------------------------------------------

function TVkBuffer<TVkDevice_,TVkParent_>.GetHandle :VkBuffer;
begin
     if _Handle = VK_NULL_HANDLE then CreateHandle;

     Result := _Handle;
end;

procedure TVkBuffer<TVkDevice_,TVkParent_>.SetHandle( const Handle_:VkBuffer );
begin
     if _Handle <> VK_NULL_HANDLE then DestroHandle;

     _Handle := Handle_;
end;

//------------------------------------------------------------------------------

function TVkBuffer<TVkDevice_,TVkParent_>.GetDescri :VkDescriptorBufferInfo;
begin
     with Result do
     begin
          buffer := Handle;
          offset := 0;
          range  := Size;
     end;
end;

//------------------------------------------------------------------------------

function TVkBuffer<TVkDevice_,TVkParent_>.GetRequir :VkMemoryRequirements;
begin
     vkGetBufferMemoryRequirements( TVkDevice( Device ).Handle, Handle, @Result );
end;

/////////////////////////////////////////////////////////////////////// メソッド

procedure TVkBuffer<TVkDevice_,TVkParent_>.CreateHandle;
begin
     Assert( vkCreateBuffer( TVkDevice( Device ).Handle, @_Inform, nil, @_Handle ) = VK_SUCCESS );

     Assert( Bind( _Memory ) );
end;

procedure TVkBuffer<TVkDevice_,TVkParent_>.DestroHandle;
begin
     _Memory.Handle := VK_NULL_HANDLE;

     vkDestroyBuffer( TVkDevice( Device ).Handle, _Handle, nil );
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TVkBuffer<TVkDevice_,TVkParent_>.Create;
begin
     inherited;

     _Handle := VK_NULL_HANDLE;

     _Memory := TVkBufMem_.Create( Self );

     with _Inform do
     begin
          sType                 := VK_STRUCTURE_TYPE_BUFFER_CREATE_INFO;
          pNext                 := nil;
          flags                 := 0;
       // size                   = Size
       // usage                  = Usage
       // sharingMode            = SharingMode
          queueFamilyIndexCount := 0;
          pQueueFamilyIndices   := nil;
     end;

     Size        := 0;
     Usage       := 0;
     SharingMode := VK_SHARING_MODE_EXCLUSIVE;
end;

destructor TVkBuffer<TVkDevice_,TVkParent_>.Destroy;
begin
     _Memory.Free;

      Handle := VK_NULL_HANDLE;

     inherited;
end;

/////////////////////////////////////////////////////////////////////// メソッド

function TVkBuffer<TVkDevice_,TVkParent_>.Bind( const Memory_:TVkBufMem_ ) :Boolean;
begin
     Result := vkBindBufferMemory( TVkDevice( Device ).Handle, Handle, Memory_.Handle, 0 ) = VK_SUCCESS;
end;

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TVkBuffer<TVkDevice_>

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

/////////////////////////////////////////////////////////////////////// アクセス

function TVkBuffer<TVkDevice_>.GetDevice :TVkDevice_;
begin
     Result := Buffers.Device;
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TVkBuffer<TVkDevice_>.Create( const Buffers_:TVkBuffers_ );
begin
     inherited;

     Buffers.Add( Self );
end;

constructor TVkBuffer<TVkDevice_>.Create( const Device_:TVkDevice_ );
begin
     Create( TVkBuffers_( TVkDevice( Device_ ).Buffers ) );
end;

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TVkBuffers<TVkDevice_>

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

/////////////////////////////////////////////////////////////////////// メソッド

function TVkBuffers<TVkDevice_>.Add :TVkBuffer_;
begin
     Result := TVkBuffer_.Create( Self );
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