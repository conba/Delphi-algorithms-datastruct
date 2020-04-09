unit TTDNdeMgr;

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
  TNodeManager = class
  private
    FNodeSize: Cardinal; // 节点大小
    FFreeList: Pointer;  // 节点的空闲列表
    FNodesPerPage: Cardinal; // 每一页的节点个数
    FPageHead: Pointer; // 页头
    FPageSize: Cardinal;  // 页的字节数
    FName: TtdNameString;
  protected
      procedure nmAllocNewPage;
  public
    constructor Create(aNodeSize: Cardinal);
    destructor Destroy; override;
    function AllocNode: Pointer;
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

function TNodeManager.AllocNode: Pointer;
begin
  GetMem(Result, FNodeSize);
  if FFreeList = nil then

end;

constructor TNodeManager.Create(aNodeSize: Cardinal);
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

destructor TNodeManager.Destroy;
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

procedure TNodeManager.nmAllocNewPage;
begin

end;

end.
