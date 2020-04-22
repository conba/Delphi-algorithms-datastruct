unit TDMyNdeMgr;

{$I TDDefine.inc}

interface
uses
  SysUtils,
  {$IFDEF Delphi1}
  WinTypes, WinProcs,
  {$ENDIF}
  {$IFDEF Delphi2Plus}
  Windows,
  {$ENDIF}
  {$IFDEF Kylix1Plus}
  Types, Libc,
  {$ENDIF}
  TDBasics;

type
  TMyNodeManager = class
  private
    FNodeSize: Cardinal; // 节点大小
    FFreeList: Pointer;  // 节点的空闲列表
    FNodesPerPage: Cardinal; // 每一页的节点个数
    FPageHead: Pointer; // 页头
    FPageSize: Cardinal;  // 页的字节数
    FName: TtdNameString;
  protected
    // 申请新的页面
    procedure nmAllocNewPage;
    // 判断节点是否在所有的页内
    procedure nmValidateNode(aNode : pointer);
    procedure nmError(aErrorCode  : integer; const aMethodName : TtdNameString);
  public
    constructor Create(aNodeSize: Cardinal);
    destructor Destroy; override;
    function AllocNode: Pointer;
    // 将节点添加到空闲列表
    procedure FreeNode(aNode : pointer);
    property Name: TtdNameString read FName write FName;
  end;

implementation

const
  PageSize = 1024;

type
  PGenericNode = ^TGenericNode;
  TGenericNode = packed record
    gnnext: PGenericNode;
    gnData: record end;
  end;

{ TNodeManager }

function TMyNodeManager.AllocNode: Pointer;
begin
  Result := nil;
  GetMem(Result, FNodeSize);
  if FFreeList = nil then
    nmAllocNewPage;
  Result := FFreeList;
  FFreeList := PGenericNode(FFreeList)^.gnnext
end;

constructor TMyNodeManager.Create(aNodeSize: Cardinal);
begin
  FNodeSize := ((aNodeSize + 3) shl 2) shr 2;
  FNodesPerPage := (PageSize - SizeOf(Pointer)) div FNodeSize;
  if FNodesPerPage > 1 then
    FPageSize := PageSize
  else
  begin
    FPageSize := SizeOf(Pointer) + FNodeSize;
    FNodesPerPage := 1;
  end;
end;

destructor TMyNodeManager.Destroy;
var
  Temp: Pointer;
begin
  while FPageHead <> nil do
  begin
    Temp := PGenericNode(FPageHead)^.gnnext;
    FreeMem(FPageHead, FNodeSize);
    FPageHead := Temp;
  end;
  inherited;
end;

procedure TMyNodeManager.FreeNode(aNode: pointer);
begin
  if Assigned(aNode) then
  begin
    nmValidateNode(aNode);
    // 如果节点非空，则加到空闲列表顶部
    PGenericNode(aNode)^.gnnext := FFreeList;
    FFreeList := aNode;
  end;
end;

procedure TMyNodeManager.nmAllocNewPage;
var
  PNewPage: PAnsiChar;
  i: Integer;
begin
  GetMem(PNewPage, FPageSize);
  if Assigned(PNewPage) then
  begin
    PGenericNode(PNewPage)^.gnnext := FPageHead;
    FPageHead := PNewPage;
  end;
  Inc(PNewPage, SizeOf(Pointer));
  for I := FNodesPerPage - 1 downto 0 do
  begin
    FreeNode(PNewPage);
    Inc(PNewPage, FNodeSize);
  end;
end;

procedure TMyNodeManager.nmError(aErrorCode: integer;
  const aMethodName: TtdNameString);
begin
  if (Name = '') then
    Name := '-unnamed-';
  raise EtdNodeMgrException.Create(
     FmtLoadStr(aErrorCode,
                [UnitName, ClassName, aMethodName, Name]));
end;

procedure TMyNodeManager.nmValidateNode(aNode: pointer);
var
  PPageHead: PAnsiChar;
begin
  PPageHead := FPageHead;
  while Assigned(PPageHead) do
  begin
    if (PPageHead < aNode) and (aNode < PPageHead + FPageSize) then
      Exit;
    PPageHead := PAnsiChar(PGenericNode(PPageHead)^.gnnext);
  end;
end;

end.
