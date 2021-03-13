unit LUX.GPU.Vulkan.Buffer;

interface //#################################################################### ■

uses vulkan_core,
     LUX.GPU.Vulkan.root,
     LUX.GPU.Vulkan.Memory;

type //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【型】

     TVkBuffers<TVkDevice_:class>    = class;
       TVkBuffer<TVkDevice_:class>   = class;
         TVkBufMem<TVkDevice_:class> = class;

       TVkUniBuf<TVkDevice_:class>                = class;
       TVkBuffer<TVkDevice_:class;TValue_:record> = class;

     //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【レコード】

     //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【クラス】

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TVkBufMem<TVkDevice_>

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

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TVkBuffer<TVkDevice_>

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
       constructor Create; overload; virtual;
       constructor Create( const Device_:TVkDevice_ ); overload; virtual;
       destructor Destroy; override;
       ///// プロパティ
       property Device      :TVkDevice_             read   _Device                          ;
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

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TVkUniBuf<TVkDevice_>

     TVkUniBuf<TVkDevice_:class> = class( TVkBuffer<TVkDevice_> )
     private
     protected
     public
       constructor Create; override;
     end;

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TVkBuffer<TVkDevice_,TValue_>

     TVkBuffer<TVkDevice_:class;TValue_:record> = class( TVkUniBuf<TVkDevice_> )
     private
     protected
       _Value :TValue_;
       ///// アクセス
       function GetValue :TValue_; virtual;
       procedure SetValue( const Value_:TValue_ ); virtual;
       ///// プロパティ
       property Size :VkDeviceSize read GetSize write SetSize;
     public
       constructor Create; override;
       ///// プロパティ
       property Value :TValue_ read GetValue write SetValue;
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

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TVkBufMem<TVkDevice_>

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
     Result := Buffer.Requir.size;
end;

//------------------------------------------------------------------------------

function TVkBufMem<TVkDevice_>.GetTypeI :UInt32;
begin
     Assert( TVkDevice( Device ).memory_type_from_properties(
                  Buffer.Requir.memoryTypeBits,
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

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TVkBuffer<TVkDevice_>

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

function TVkBuffer<TVkDevice_>.GetUsage :VkBufferUsageFlags;
begin
     Result := _Inform.usage;
end;

procedure TVkBuffer<TVkDevice_>.SetUsage( const Usage_:VkBufferUsageFlags );
begin
     _Inform.usage := Usage_;

     Handle := VK_NULL_HANDLE;
end;

//------------------------------------------------------------------------------

function TVkBuffer<TVkDevice_>.GetSharingMode :VkSharingMode;
begin
     Result := _Inform.sharingMode;
end;

procedure TVkBuffer<TVkDevice_>.SetSharingMode( const SharingMode_:VkSharingMode );
begin
     _Inform.sharingMode := SharingMode_;

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

//------------------------------------------------------------------------------

function TVkBuffer<TVkDevice_>.GetRequir :VkMemoryRequirements;
begin
     vkGetBufferMemoryRequirements( TVkDevice( Device ).Handle, Handle, @Result );
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

constructor TVkBuffer<TVkDevice_>.Create( const Device_:TVkDevice_ );
begin
     Create;

     _Device := Device_;

     TVkDevice( _Device ).Buffers.Add( TVkBuffer( Self ) );
end;

destructor TVkBuffer<TVkDevice_>.Destroy;
begin
     _Memory.Free;

      Handle := VK_NULL_HANDLE;

     inherited;
end;

/////////////////////////////////////////////////////////////////////// メソッド

function TVkBuffer<TVkDevice_>.Bind( const Memory_:TVkBufMem_ ) :Boolean;
begin
     Result := vkBindBufferMemory( TVkDevice( Device ).Handle, Handle, Memory_.Handle, 0 ) = VK_SUCCESS;
end;

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TVkUniBuf<TVkDevice_>

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TVkUniBuf<TVkDevice_>.Create;
begin
     inherited;

     Usage := Ord( VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT );
end;

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TVkBuffer<TVkDevice_,TValue_>

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

/////////////////////////////////////////////////////////////////////// アクセス

function TVkBuffer<TVkDevice_,TValue_>.GetValue :TValue_;
begin
     Result := _Value;
end;

procedure TVkBuffer<TVkDevice_,TValue_>.SetValue( const Value_:TValue_ );
var
   P :PByte;
begin
     _Value := Value_;

     _Memory.Map( P );

     Move( _Value, P^, Size );

     _Memory.Unmap;
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TVkBuffer<TVkDevice_,TValue_>.Create;
begin
     inherited;

     Size := SizeOf( TValue_ );
end;

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TVkBuffers<TVkDevice_>

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