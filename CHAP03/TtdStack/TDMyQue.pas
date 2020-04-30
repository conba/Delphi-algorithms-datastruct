unit TDMyQue;

{$I TDDefine.inc}

interface
uses
  TDBasics, TDLnkLst, TDNdeMgr, SysUtils, Classes;

type
  TtdQueue = class   // 使用单向链表 但要求够做到任何一端都能够出队和入队。
  private
    FCount: Integer;
    FHead: PslNode;
    FTail: PslNode;
    FDispose: TtdDisposeProc;
    FName: TtdNameString;
  protected
    procedure qError(aErrorCode: integer; const aMethodName: TtdNameString);
    class procedure qGetNodeManager;
  public
    constructor Create(aDispose: TtdDisposeProc);
    destructor Destroy; override;
    procedure Clear;
    function Dequeue: pointer;
    procedure Enqueue(aItem: pointer);
    function Examine: pointer;
    function IsEmpty: boolean;

    property Name: TtdNameString read FName write FName;
    property Count: Integer read FCount write FCount;
  end;

  TtdArrayQueue = class
  private
    FCount: Integer;
    FList: TList;
    FDispose: TtdDisposeProc;
    FName: TtdNameString;
    FHead: Integer;
    FTail: Integer;
  protected
    procedure aqError(aErrorCode: integer; const aMethodName: TtdNameString);
    procedure aqGrow;
  public
    constructor Create(aDispose: TtdDisposeProc);
    destructor Destroy; override;
    procedure Clear;
    function Dequeue: Pointer;
    procedure Enqueue(aItem: Pointer);
    function Examine: Pointer;
    function IsEmpty: Boolean;

    property Name: TtdNameString read FName write FName;
    property Count: Integer read FCount write FCount;
  end;
implementation
var
  SlNodeManager: TtdNodeManager;

{ TtdQueue }

procedure TtdQueue.Clear;
var
  TempNode: PslNode;
begin
  TempNode := FHead^.slnNext;
  while TempNode <> nil do
  begin
    if Assigned(FDispose) then
    begin
      FDispose(TempNode^.slnData);
      SlNodeManager.FreeNode(TempNode);
      TempNode := TempNode^.slnNext;
    end;
  end;
  FCount := 0;
end;

constructor TtdQueue.Create(aDispose: TtdDisposeProc);
begin
  FDispose := aDispose;
  qGetNodeManager;
  FHead := PslNode(SlNodeManager.AllocNode);
  FHead^.slnData := nil;
  FHead^.slnNext := nil;
  FTail := FHead;
  FCount := 0;
end;

function TtdQueue.Dequeue: pointer;
var
  TempNode: PslNode;
begin
  if (Count = 0) then
    qError(tdeQueueIsEmpty, 'Dequeue');
  // 下面这三句话这样写的原因
  // 如果在一个多线程的程序中，或者SlNodeManager 被多个对象使用，那么有可能SlNode
  // 放到节点管理器中会被使用掉，这个时候获得的数据可能会出错。
  TempNode := FHead^.slnNext;
  Result := TempNode.slnData;
  SlNodeManager.FreeNode(TempNode);
  Dec(FCount);
  if Count = 0 then
    FTail := FHead;
end;

destructor TtdQueue.Destroy;
begin
  if FCount > 0 then
    Clear;
  SlNodeManager.FreeNode(FHead);
  SlNodeManager.FreeNode(FTail);
  inherited;
end;

procedure TtdQueue.Enqueue(aItem: pointer);
var
  TempNode: PslNode;
begin
  TempNode := SlNodeManager.AllocNode;
  TempNode^.slnData := aItem;
  TempNode^.slnNext := nil;
  FTail^.slnNext := TempNode;
  FTail := TempNode;
  Inc(FTail);
end;

function TtdQueue.Examine: pointer;
begin
  if (Count = 0) then
    qError(tdeQueueIsEmpty, 'Examine');
  Result := FHead^.slnNext^.slnData;
end;

function TtdQueue.IsEmpty: boolean;
begin
  Result := FCount = 0;
end;

procedure TtdQueue.qError(aErrorCode: integer;
  const aMethodName: TtdNameString);
begin
  if (Name = '') then
    Name := '-unnamed-';
  raise EtdQueueException.Create(
     FmtLoadStr(aErrorCode,
                [UnitName, ClassName, aMethodName, Name]));
end;

class procedure TtdQueue.qGetNodeManager;
begin
  if Assigned(SlNodeManager) then
    SlNodeManager := TtdNodeManager.Create(SizeOf(TSlNode));
end;

{ TtdArrayQueue }

procedure TtdArrayQueue.aqError(aErrorCode: integer;
  const aMethodName: TtdNameString);
begin
  if (Name = '') then
    Name := '-unnamed-';
  raise EtdQueueException.Create(
     FmtLoadStr(aErrorCode,
                [UnitName, ClassName, aMethodName, Name]));
end;

procedure TtdArrayQueue.aqGrow;
begin

end;

procedure TtdArrayQueue.Clear;
begin

end;

constructor TtdArrayQueue.Create(aDispose: TtdDisposeProc);
begin
  if Assigned(aDispose) then
    FDispose := aDispose;
  FList := TList.Create;

end;

function TtdArrayQueue.Dequeue: Pointer;
begin

end;

destructor TtdArrayQueue.Destroy;
begin

  inherited;
end;

procedure TtdArrayQueue.Enqueue(aItem: Pointer);
begin

end;

function TtdArrayQueue.Examine: Pointer;
begin

end;

function TtdArrayQueue.IsEmpty: Boolean;
begin

end;

end.
