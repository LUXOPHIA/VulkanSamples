unit LUX.GPU.Vulkan.Memory;

interface //#################################################################### ■

uses vulkan_core,
     LUX.GPU.Vulkan.root;

type //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【型】

     TVkMemory<TVkDevice_,TVkParent_:class> = class;

     //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【レコード】

     //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【クラス】

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TVkMemory

     TVkMemory<TVkDevice_,TVkParent_:class> = class( TVkDeviceObject<TVkDevice_,TVkParent_> )
     private
     protected
       _Inform :VkMemoryAllocateInfo;
       _Handle :VkDeviceMemory;
       ///// アクセス
       function GetInform :VkMemoryAllocateInfo; virtual;
       function GetSize :VkDeviceSize; virtual;
       procedure SetSize( const Size_:VkDeviceSize ); virtual;
       function GetTypeI :UInt32; virtual;
       procedure SetTypeI( const TypeI_:UInt32 ); virtual;
       function GetHandle :VkDeviceMemory; virtual;
       procedure SetHandle( const Handle_:VkDeviceMemory ); virtual;
       ///// メソッド
       procedure CreateHandle; virtual;
       procedure DestroHandle; virtual;
     public
       constructor Create; override;
       destructor Destroy; override;
       ///// プロパティ
       property Inform :VkMemoryAllocateInfo read GetInform                ;
       property Size   :VkDeviceSize         read GetSize   write SetSize  ;
       property TypeI  :UInt32               read GetTypeI  write SetTypeI ;
       property Handle :VkDeviceMemory       read GetHandle write SetHandle;
       ///// メソッド
       function Map( var Pointer_:PByte ) :Boolean;
       procedure Unmap;
     end;

//const //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【定数】

//var //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【変数】

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【ルーチン】

implementation //############################################################### ■

uses LUX.GPU.Vulkan;

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【レコード】

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【クラス】

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TVkMemory

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

/////////////////////////////////////////////////////////////////////// アクセス

function TVkMemory<TVkDevice_,TVkParent_>.GetInform :VkMemoryAllocateInfo;
begin
     Result := _Inform;
end;

//------------------------------------------------------------------------------

function TVkMemory<TVkDevice_,TVkParent_>.GetSize :VkDeviceSize;
begin
     Result := _Inform.allocationSize;
end;

procedure TVkMemory<TVkDevice_,TVkParent_>.SetSize( const Size_:VkDeviceSize );
begin
     _Inform.allocationSize := Size_;

     Handle := VK_NULL_HANDLE;
end;

//------------------------------------------------------------------------------

function TVkMemory<TVkDevice_,TVkParent_>.GetTypeI :UInt32;
begin
     Result := _Inform.memoryTypeIndex;
end;

procedure TVkMemory<TVkDevice_,TVkParent_>.SetTypeI( const TypeI_:UInt32 );
begin
     _Inform.memoryTypeIndex := TypeI_;

     Handle := VK_NULL_HANDLE;
end;

//------------------------------------------------------------------------------

function TVkMemory<TVkDevice_,TVkParent_>.GetHandle :VkDeviceMemory;
begin
     if _Handle = VK_NULL_HANDLE then CreateHandle;

     Result := _Handle;
end;

procedure TVkMemory<TVkDevice_,TVkParent_>.SetHandle( const Handle_:VkDeviceMemory );
begin
     if _Handle <> VK_NULL_HANDLE then DestroHandle;

     _Handle := Handle_;
end;

/////////////////////////////////////////////////////////////////////// メソッド

procedure TVkMemory<TVkDevice_,TVkParent_>.CreateHandle;
begin
     Assert( vkAllocateMemory( TVkDevice( Device ).Handle, @_Inform, nil, @_Handle ) = VK_SUCCESS );
end;

procedure TVkMemory<TVkDevice_,TVkParent_>.DestroHandle;
begin
     vkFreeMemory( TVkDevice( Device ).Handle, _Handle, nil );
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TVkMemory<TVkDevice_,TVkParent_>.Create;
begin
     inherited;

     _Handle := VK_NULL_HANDLE;

     with _Inform do
     begin
          sType           := VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO;
          pNext           := nil;
       // allocationSize  :
       // memoryTypeIndex :
     end;

     Size  := 0;
     TypeI := 0;
end;

destructor TVkMemory<TVkDevice_,TVkParent_>.Destroy;
begin
      Handle := VK_NULL_HANDLE;

     inherited;
end;

/////////////////////////////////////////////////////////////////////// メソッド

function TVkMemory<TVkDevice_,TVkParent_>.Map( var Pointer_:PByte ) :Boolean;
begin
     Result := vkMapMemory( TVkDevice( Device ).Handle, Handle, 0, Size, 0, @Pointer_ ) = VK_SUCCESS;
end;

procedure TVkMemory<TVkDevice_,TVkParent_>.Unmap;
begin
     vkUnmapMemory( TVkDevice( Device ).Handle, Handle );
end;

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【ルーチン】

end. //######################################################################### ■