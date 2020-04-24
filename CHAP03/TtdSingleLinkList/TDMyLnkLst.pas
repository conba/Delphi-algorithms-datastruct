unit TDMyLnkLst;

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
  PslNode = ^TslNode;      {a node with one link}
  TslNode = record
    slnNext: PslNode;
    slnData: Pointer;
  end;

  PdlNode = ^TdlNode;
  TdlNode = record
    dlPrior: PdlNode;
    dlNext: PdlNode;
    dlData: Pointer;
  end;

type
  TtdMySingleLinkList = class
  private
    FHead: PslNode;
    FParent: PslNode;
    FCount: Integer;
    FCursor: PslNode;
    FCursorIx: Integer;
    FDispose : TtdDisposeProc;
    FName: TtdNameString;
    FIsSorted: boolean;
  protected
    // 获得节点管理器
    procedure dllGetNodeManager;
    // 将游标移动到某个索引位置
    procedure sllPositionAtNth(aIndex : longint);
    // 获得某个索引的数据
    function sllGetItem(aIndex : longint) : pointer;

  public
    constructor Create(aDispose: TtdDisposeProc);
    destructor Destroy; override;

    function Add(aItem: Pointer): Integer;
    procedure InsertAtCursor(aItem: Pointer);
    procedure Insert(aIndex: Integer; aItem: Pointer);
    procedure Delete(aIndex: Integer);
    procedure DeleteAtCursor;
    procedure Remove(aItem : pointer);
    // 删除所有节点,不包括头结点
    procedure Clear;
    function IndexOf(aItem: Pointer): Integer;
    // 获得第一个节点的数据
    function First: Pointer;
    // 获得最后一个数据
    function Last: Pointer;
    // 游标移动到头结点
    procedure MoveBeforeFirst;
    // 游标移动到下一个节点
    procedure MoveNext;
    // 返回当前游标数据内容
    function Examine: pointer;
    // 判断游标是否在最后节点的后面
    function IsAfterLast: Boolean;
    // 判断游标是否在头结点
    function IsBeforeFirst: Boolean;
    // 判断链表是否为空
    function IsEmpty: Boolean;
  end;

  TtdMyDoubleLinkList = class
  private
    FCount: longint;
    FCursor: PdlNode;
    FCursorIx: longint;
    FDispose: TtdDisposeProc;
    FHead: PdlNode;
    FIsSorted: boolean;
    FName: TtdNameString;
    FTail: PdlNode;
  protected
    // 获得某个索引的数据
    function dllGetItem(aIndex: longint): pointer;

    procedure dllSetItem(aIndex: longint; aItem: pointer);

    procedure dllError(aErrorCode: integer; const aMethodName: TtdNameString);
    // 获得节点管理器类
    class procedure dllGetNodeManager;

    function dllMerge(aCompare: TtdCompareFunc; aPriorNode1: PdlNode;
      aCount1: longint; aPriorNode2: PdlNode; aCount2: longint): PdlNode;

    function dllMergesort(aCompare: TtdCompareFunc; aPriorNode: PdlNode; aCount: longint): PdlNode;

    procedure dllPositionAtNth(aIndex: longint);
  public
    constructor Create(aDispose: TtdDisposeProc);
    destructor Destroy; override;
    function Add(aItem: pointer): longint;
    // 清空所有节点
    procedure Clear;
    // 删除某个索引节点
    procedure Delete(aIndex: longint);
    // 删除当前游标节点
    procedure DeleteAtCursor;
    // 返回当前游标内容
    function Examine: pointer;
    // 移动到第一个节点
    function First: pointer;
    function IndexOf(aItem: pointer): longint;
    procedure Insert(aIndex: longint; aItem: pointer);
    procedure InsertAtCursor(aItem: pointer);
    procedure InsertSorted(aItem: pointer; aCompare: TtdCompareFunc);
    function IsAfterLast: boolean;
    function IsBeforeFirst: boolean;
    function IsEmpty: boolean;
    function Last: pointer;
    function Locate(aItem: pointer; aCompare: TtdCompareFunc): longint;
    procedure MoveAfterLast;
    // 移动到头结点
    procedure MoveBeforeFirst;
    procedure MoveNext;
    procedure MovePrior;
    procedure Remove(aItem: pointer);
    procedure InsertionSort(aCompare: TtdCompareFunc);
    procedure Sort(aCompare: TtdCompareFunc);
    property Count: longint read FCount;
    property IsSorted: boolean read FIsSorted;
    property Items[aIndex: longint]: pointer read dllGetItem write dllSetItem; default;
    property Name: TtdNameString read FName write FName;
  end;

implementation
uses
  TDMyNdeMgr;

var
  SLNodeManager: TMyNodeManager; {nodemanager for singlylinked lists}
  DlNodeManager: TMyNodeManager; {nodemanager for doublylinked lists}


{ TtdSingleLinkList }

function TtdMySingleLinkList.Add(aItem: Pointer): Integer;
var
  slNode: PslNode;
begin
  slNode := SLNodeManager.AllocNode;
  slNode^.slnData := aItem;
  sllPositionAtNth(FCount);
  FCursor^.slnNext := slNode;
  FCursor := slNode;
  FCursorIx := FCursorIx + 1;
end;

procedure TtdMySingleLinkList.Clear;
var
  TempNode: PslNode;
begin
  TempNode := FHead^.slnNext;
  while Assigned(TempNode) do
  begin
    FHead^.slnNext := TempNode^.slnNext;
    if Assigned(FDispose) then
      FDispose(TempNode^.slnData);
    SLNodeManager.FreeNode(TempNode);
    TempNode := FHead^.slnNext;
  end;
  FCount := 0;
  MoveBeforeFirst;
  FIsSorted := True;
end;

constructor TtdMySingleLinkList.Create(aDispose: TtdDisposeProc);
begin
  inherited Create;
  FDispose := aDispose;
  dllGetNodeManager;
  FHead := SLNodeManager.AllocNode;
  FHead^.slnNext := nil;
  FHead^.slnData := nil;
  MoveBeforeFirst;
  FIsSorted := True;
end;

procedure TtdMySingleLinkList.Delete(aIndex: Integer);
begin
  sllPositionAtNth(aIndex);
  DeleteAtCursor;
end;

procedure TtdMySingleLinkList.DeleteAtCursor;
var
  TempNode: PslNode;
begin
  // 如果游标在链尾或者在头结点
  if (not Assigned(FCursor)) or (FCursor = FHead) then
    Exit; // 或者提示错误
  TempNode := FCursor;
  if Assigned(FDispose) then
    FDispose(TempNode^.slnData);
  FParent^.slnNext := TempNode^.slnNext;
  SLNodeManager.FreeNode(TempNode);
  FCursor := FParent^.slnNext;
  Dec(FCount);
end;

destructor TtdMySingleLinkList.Destroy;
begin
  Clear;
  // 删除头结点
  SLNodeManager.FreeNode(FHead);
  inherited;
end;

procedure TtdMySingleLinkList.dllGetNodeManager;
begin
  if not Assigned(SLNodeManager) then
    SLNodeManager := TMyNodeManager.Create(SizeOf(TslNode));
end;

function TtdMySingleLinkList.Examine: pointer;
begin
  if (not Assigned(FCursor)) and (FCursor = FHead)then
    Exit; // 或者提示错误
  Result := FCursor^.slnData;
end;

function TtdMySingleLinkList.First: Pointer;
begin
  sllPositionAtNth(0);
  Result := FCursor^.slnData;
end;

function TtdMySingleLinkList.IndexOf(aItem: Pointer): Integer;
var
  TempParent, TempNode: PslNode;
  TempIndex: Integer;
begin
  TempParent := FHead;
  TempNode := FHead^.slnNext;
  TempIndex := 0;
  while Assigned(TempNode) do
  begin
    if TempNode^.slnData = aItem then
    begin
      FParent := TempParent;
      FCursor := TempNode;
      FCursorIx := TempIndex;
      Exit;
    end
    else
    begin
      TempParent := TempNode;
      TempNode := TempNode^.slnNext;
      TempIndex := TempIndex + 1;
    end;
  end;
  Result := -1;
end;

procedure TtdMySingleLinkList.Insert(aIndex: Integer; aItem: Pointer);
var
  TempNode: PslNode;
begin
  sllPositionAtNth(aIndex);
  InsertAtCursor(aItem);
end;

procedure TtdMySingleLinkList.InsertAtCursor(aItem: Pointer);
var
  TempNode: PslNode;
begin
  if FCursor = FHead then
    MoveNext;
  TempNode := SLNodeManager.AllocNode;
  TempNode^.slnData := aItem;
  FParent^.slnNext := TempNode;
  TempNode^.slnNext := FCursor;
  FCursor := TempNode;
  Inc(FCount);
end;

function TtdMySingleLinkList.IsAfterLast: Boolean;
begin
  Result := FCursor = nil;
end;

function TtdMySingleLinkList.IsBeforeFirst: Boolean;
begin
  Result := FCursor = FHead;
end;

function TtdMySingleLinkList.IsEmpty: Boolean;
begin
  Result := FCount = 0;
end;

function TtdMySingleLinkList.Last: Pointer;
begin
  sllPositionAtNth(FCount);
  Result := FCursor^.slnData;
end;

procedure TtdMySingleLinkList.MoveBeforeFirst;
begin
  FCursor := FHead;
  FParent := nil;
  FCursorIx := -1;
end;

procedure TtdMySingleLinkList.MoveNext;
begin
  if Assigned(FCursor) then
  begin
    FParent := FCursor;
    FCursor := FCursor^.slnNext;
    Inc(FCursorIx);
  end;
end;

procedure TtdMySingleLinkList.Remove(aItem: pointer);
var
  iIndex: Integer;
begin
  iIndex := IndexOf(aItem);
  if iIndex <> -1 then
    DeleteAtCursor;
end;

function TtdMySingleLinkList.sllGetItem(aIndex: Integer): pointer;
begin
  sllPositionAtNth(aIndex);
  Result := FCursor^.slnData;
end;

procedure TtdMySingleLinkList.sllPositionAtNth(aIndex: Integer);
var
  TempNode: PslNode;
  TempIndex: Integer;
begin
  if (aIndex < 0) or (aIndex > FCount - 1) then
    Exit;  // 或者提示错误
  if aIndex = FCursorIx then
    Exit;
  if aIndex > FCursorIx then //如果索引大于当前游标索引
  begin
    TempIndex := FCursorIx;
    TempNode := FCursor;
  end
  else
  begin
    TempNode := FHead;
    TempIndex := -1;
  end;
  while TempIndex < aIndex - 1 do
  begin
    TempNode := TempNode^.slnNext;
    TempIndex := TempIndex + 1;
  end;
  FParent := TempNode;
  FCursor := TempNode^.slnNext;
  FCursorIx := TempIndex;
end;

{ TtdMyDoubleLinkList }

function TtdMyDoubleLinkList.Add(aItem: pointer): longint;
begin
  if FCursor = FHead then
    MoveNext;
  dllPositionAtNth(0);
  InsertAtCursor(aItem);
end;

procedure TtdMyDoubleLinkList.Clear;
var
  TempNode: PdlNode;
begin
//  MoveBeforeFirst;
  TempNode := FHead^.dlNext;
  while TempNode <> FTail do
  begin
    if Assigned(FDispose) then
    begin
      FDispose(TempNode^.dlData);
      FHead^.dlNext := TempNode^.dlNext;
      DlNodeManager.FreeNode(TempNode);
    end;
  end;
  FTail^.dlPrior := FHead;
  FCursor := FHead;
  FCursorIx := -1;
  FCount := 0;
end;

constructor TtdMyDoubleLinkList.Create(aDispose: TtdDisposeProc);
begin
  FDispose := aDispose;
  FHead := DlNodeManager.AllocNode;
  FTail := DlNodeManager.AllocNode;
  FHead^.dlPrior := nil;
  FHead^.dlNext := FTail;
  FHead^.dlData := nil;
  FTail^.dlPrior := FHead;
  FTail^.dlNext := nil;
  FTail^.dlData := nil;
  FCount := 0;
  FIsSorted := True;
  FCursor := FHead;
  FCursorIx := -1;
end;

procedure TtdMyDoubleLinkList.Delete(aIndex: Integer);
begin
  dllPositionAtNth(aIndex);;
  DeleteAtCursor;
end;

procedure TtdMyDoubleLinkList.DeleteAtCursor;
var
  TempNode: PdlNode;
begin
  TempNode := FCursor;
  if (TempNode = FHead) or (TempNode = FTail)  then
    dllError(tdeListCannotDelete, 'Delete');
  if Assigned(FDispose) then
    FDispose(TempNode^.dlData);
  TempNode^.dlNext.dlPrior := TempNode^.dlPrior;
  TempNode^.dlPrior.dlNext := TempNode^.dlNext;
  FCursor := TempNode^.dlNext;
  FCount := FCount - 1;
  DlNodeManager.FreeNode(TempNode);
end;

destructor TtdMyDoubleLinkList.Destroy;
begin
  Clear;
  // 删除头结点和尾节点
  FHead^.dlNext := nil;
  FTail^.dlPrior := nil;
  DlNodeManager.FreeNode(FHead);
  DlNodeManager.FreeNode(FTail);
  inherited;
end;

procedure TtdMyDoubleLinkList.dllError(aErrorCode: integer;
  const aMethodName: TtdNameString);
begin

end;

function TtdMyDoubleLinkList.dllGetItem(aIndex: Integer): pointer;
begin
  dllPositionAtNth(aIndex);
  Result := FCursor^.dlData;
end;

class procedure TtdMyDoubleLinkList.dllGetNodeManager;
begin
  if not Assigned(DlNodeManager) then
    DlNodeManager := TMyNodeManager.Create(SizeOf(TdlNode));
end;

function TtdMyDoubleLinkList.dllMerge(aCompare: TtdCompareFunc;
  aPriorNode1: PdlNode; aCount1: Integer; aPriorNode2: PdlNode;
  aCount2: Integer): PdlNode;
begin

end;

function TtdMyDoubleLinkList.dllMergesort(aCompare: TtdCompareFunc;
  aPriorNode: PdlNode; aCount: Integer): PdlNode;
begin

end;

procedure TtdMyDoubleLinkList.dllPositionAtNth(aIndex: Integer);
var
  TempNode: PdlNode;
  TempCursorIx: Integer;
begin
  if (aIndex < 0) or (aIndex >Count) then
    dllError(tdeListInvalidIndex, 'dllPositionAtNth');

  TempNode := FCursor;
  TempCursorIx := FCursorIx;
  if aIndex < FCursorIx then
  begin
    if aIndex < (FCursorIx - aIndex) then
    begin
      TempNode := FHead^.dlNext;
      TempCursorIx := 0;
    end;
  end
  else
  begin
    if (FCount - aIndex) < (FCursorIx - aIndex) then
    begin
      TempNode := FTail;
      TempCursorIx := FCount;
    end;
  end;

  while TempCursorIx < aIndex do
  begin
    TempNode := TempNode^.dlNext;
    Inc(TempCursorIx);
  end;

  while TempNode > aIndex do
  begin
    TempNode := TempNode^.dlPrior;
    Dec(TempCursorIx);
  end;

  FCursor := TempNode;
  FCursorIx := TempCursorIx;
end;

procedure TtdMyDoubleLinkList.dllSetItem(aIndex: Integer; aItem: pointer);
begin
  dllPositionAtNth(aIndex);
  FCursor^.dlData := aItem;
end;

function TtdMyDoubleLinkList.Examine: pointer;
begin
  if (FCursor = FHead) and (FCursor = FTail) then
    dllError(tdeListCannotExamine, 'Examine');
  Result := FCursor^.dlData;
end;

function TtdMyDoubleLinkList.First: pointer;
begin
  dllPositionAtNth(0);
  Result := FCursor^.dlData;
end;

function TtdMyDoubleLinkList.IndexOf(aItem: pointer): longint;
var
  TempNode: PdlNode;
  TempIx: Integer;
begin
  TempNode := FHead^.dlNext;
  TempIx := 0;
  while TempNode <> FTail do
  begin
    if TempNode^.dlData = aItem then
    begin
      Result := TempIx;
      FCursor := TempNode;
      FCursorIx := TempIx;
      Exit;
    end;
    Inc(TempIx);
    TempNode := TempNode^.dlNext;
  end;
  Result := -1;
end;

procedure TtdMyDoubleLinkList.Insert(aIndex: Integer; aItem: pointer);
begin
  dllPositionAtNth(aIndex);
  InsertAtCursor(aItem);
end;

procedure TtdMyDoubleLinkList.InsertAtCursor(aItem: pointer);
var
  TempNode: PdlNode;
begin
  if FCursor = FHead then
    MoveNext;
  TempNode := DlNodeManager.AllocNode;
  TempNode^.dlData := aItem;
  TempNode^.dlPrior := FCursor^.dlPrior;
  FCursor^.dlPrior^.dlNext := TempNode;
  FCursor^.dlPrior := TempNode;
  TempNode^.dlNext := FCursor;
  FCursor = TempNode;
  Inc(FCount);
end;

procedure TtdMyDoubleLinkList.InsertionSort(aCompare: TtdCompareFunc);
begin

end;

procedure TtdMyDoubleLinkList.InsertSorted(aItem: pointer;
  aCompare: TtdCompareFunc);
begin

end;

function TtdMyDoubleLinkList.IsAfterLast: boolean;
begin
  Result := FCursor = FTail;
end;

function TtdMyDoubleLinkList.IsBeforeFirst: boolean;
begin
  Result := FCursor = FHead;
end;

function TtdMyDoubleLinkList.IsEmpty: boolean;
begin
  Result := FCount = 0;
end;

function TtdMyDoubleLinkList.Last: pointer;
begin
  FCursor := FTail^.dlPrior;
end;

function TtdMyDoubleLinkList.Locate(aItem: pointer;
  aCompare: TtdCompareFunc): longint;
begin

end;

procedure TtdMyDoubleLinkList.MoveAfterLast;
begin
  FCursor := FTail;
  FCursorIx := Count;
end;

procedure TtdMyDoubleLinkList.MoveBeforeFirst;
begin
  FCursor := FHead;
  FCursorIx := -1;
end;

procedure TtdMyDoubleLinkList.MoveNext;
begin
  if FCursor <> FTail then
  begin
    FCursor := FCursor^.dlNext;
    Inc(FCursor);
  end;
end;

procedure TtdMyDoubleLinkList.MovePrior;
begin
  if FCursor <> FHead then
  begin
    FCursor := FCursor^.dlPrior;
    Dec(FCursorIx);
  end;
end;

procedure TtdMyDoubleLinkList.Remove(aItem: pointer);
begin
  if IndexOf(aItem) <> -1 then
    DeleteAtCursor;
end;

procedure TtdMyDoubleLinkList.Sort(aCompare: TtdCompareFunc);
begin

end;

end.
