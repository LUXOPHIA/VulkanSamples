unit LUX.GPU.Vulkan.root;

interface //#################################################################### ■

uses SYstem.Generics.Collections;

type //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【型】

     TVkDeviceObject<TVkDevice_,TVkParent_:class>            = class;

     TVkDeviceLister<TVkDevice_,TVkParent_,TVkChildr_:class> = class;
     TVkDeviceLister<TVkDevice_,TVkChildr_:class>            = class;

     //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【レコード】

     //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【クラス】

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TVkDeviceChildr<TVkDevice_,TVkParent_>

     TVkDeviceObject<TVkDevice_,TVkParent_:class> = class
     private
     protected
       _Parent :TVkParent_;
       ///// アクセス
       function GetDevice :TVkDevice_; virtual; abstract;
       function GetParent :TVkParent_;                                          { TODO : virtual; にすると「F2084 内部エラー: URW1302」が生じる。 }
     public
       constructor Create; overload; virtual;
       constructor Create( const Parent_:TVkParent_ ); overload; virtual;
       ///// プロパティ
       property Device :TVkDevice_ read GetDevice;
       property Parent :TVkParent_ read GetParent;
     end;

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TVkDeviceLister<TVkDevice_,TVkParent_,TVkChildr_>

     TVkDeviceLister<TVkDevice_,TVkParent_,TVkChildr_:class> = class( TObjectList<TVkChildr_> )
     private
     protected
       _Parent :TVkParent_;
       ///// アクセス
       function GetDevice :TVkDevice_; virtual; abstract;
       function GetParent :TVkParent_; virtual;
     public
       constructor Create; overload; virtual;
       constructor Create( const Parent_:TVkParent_ ); overload; virtual;
       ///// プロパティ
       property Device :TVkDevice_ read GetDevice;
       property Parent :TVkParent_ read GetParent;
     end;

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TVkDeviceLister<TVkDevice_,TVkChildr_>

     TVkDeviceLister<TVkDevice_,TVkChildr_:class> = class( TVkDeviceLister<TVkDevice_,TVkDevice_,TVkChildr_> )
     private
     protected
       ///// アクセス
       function GetDevice :TVkDevice_; override;
     public
     end;

//const //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【定数】

//var //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【変数】

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【ルーチン】

implementation //############################################################### ■

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【レコード】

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【クラス】

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TVkDeviceChildr<TVkDevice_,TVkParent_>

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

function TVkDeviceObject<TVkDevice_,TVkParent_>.GetParent :TVkParent_;
begin
     Result := _Parent;
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TVkDeviceObject<TVkDevice_,TVkParent_>.Create;
begin
     inherited;

end;

constructor TVkDeviceObject<TVkDevice_,TVkParent_>.Create( const Parent_:TVkParent_ );
begin
     Create;

     _Parent := Parent_;
end;

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TVkDeviceLister<TVkDevice_,TVkParent_,TVkChildr_>

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

function TVkDeviceLister<TVkDevice_,TVkParent_,TVkChildr_>.GetParent :TVkParent_;
begin
     Result := _Parent;
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TVkDeviceLister<TVkDevice_,TVkParent_,TVkChildr_>.Create;
begin
     inherited;

end;

constructor TVkDeviceLister<TVkDevice_,TVkParent_,TVkChildr_>.Create( const Parent_:TVkParent_ );
begin
     Create;

     _Parent := Parent_;
end;

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TVkDeviceLister<TVkDevice_,TVkChildr_>

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

/////////////////////////////////////////////////////////////////////// アクセス

function TVkDeviceLister<TVkDevice_,TVkChildr_>.GetDevice :TVkDevice_;
begin
     Result := _Parent;
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【ルーチン】

end. //######################################################################### ■