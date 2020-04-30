unit TDMyStack;

{$I TDDefine.inc}

interface
uses
  TDBasics, TDLnkLst, SysUtils, Classes;
type
  TtdStack = class
  private
    FCount: longint;
    FDispose: TtdDisposeProc;
    FHead: PslNode;
    FName: TtdNameString;
  protected
    procedure sError(aErrorCode: integer; const aMethodName: TtdNameString);
    class procedure sGetNodeManager;
  public
    constructor Create(aDispose: TtdDisposeProc);
    destructor Destroy; override;
    procedure Clear;
    function Examine: pointer;
    function IsEmpty: boolean;
    function Pop: pointer;
    procedure Push(aItem: pointer);
    property Count: longint read FCount;
    property Name: TtdNameString read FName write FName;
  end;

  TtdArrayStack = class
  private
    FCount: Integer;
    FDispose: TtdDisposeProc;
    FList: Tlist;
    FName: TtdNameString;
  protected
    procedure asError(aErrorCode: integer; const aMethodName: TtdNameString);
    procedure asGrow;
  public
    constructor Create(aDispose: TtdDisposeProc; aCapacity: integer);
    destructor Destroy; override;

    procedure Clear;
    function Examine: Pointer;
    function IsEmpty: Boolean;
    function Pop: Pointer;
    procedure Push(aItem: Pointer);

    property Count: LongInt read FCount;
    property Name: TtdNameString read FName write FName;
  end;

implementation
uses
  TDNdeMgr;

var
  SLNodeManager : TtdNodeManager; {nodemanager for stacks and queues}
{ TtdStack }

procedure TtdStack.Clear;
var
  TempNode: PslNode;
begin
  TempNode := FHead^.slnNext;
  while Assigned(TempNode) do
  begin
    FHead^.slnNext := TempNode^.slnNext;
    if Assigned(FDispose) then
    begin
      FDispose(TempNode^.slnData);
      SLNodeManager.FreeNode(TempNode);
      TempNode := FHead^.slnNext;
    end;
  end;
  FCount := 0;
end;

constructor TtdStack.Create(aDispose: TtdDisposeProc);
begin
  FDispose := aDispose;
  sGetNodeManager;
  FHead := PslNode(SLNodeManager.AllocNode);
  FHead^.slnNext := nil;
  FHead^.slnData := nil;
  FCount := 0;
end;

destructor TtdStack.Destroy;
begin
  if Count > 0 then
    Clear;
  SLNodeManager.FreeNode(FHead);
  inherited;
end;

function TtdStack.Examine: pointer;
begin
  if Count > 0 then
    Result := FHead^.slnNext.slnData;
end;

function TtdStack.IsEmpty: boolean;
begin
  Result := FHead^.slnNext = nil;
end;

function TtdStack.Pop: pointer;
begin
  if FCount = 0 then
    sError(tdeStackIsEmpty, 'Pop');
  Result := FHead^.slnNext^.slnData;
  FHead^.slnNext := FHead^.slnNext.slnNext;
  SLNodeManager.FreeNode(Result);
  Dec(FCount);
end;

procedure TtdStack.Push(aItem: pointer);
var
  TempNode: PslNode;
begin
  TempNode := SLNodeManager.AllocNode;
  TempNode^.slnData := aItem;
  TempNode^.slnNext := FHead^.slnNext;
  FHead^.slnNext := TempNode;
  Inc(FCount);
end;

procedure TtdStack.sError(aErrorCode: integer;
  const aMethodName: TtdNameString);
begin
  if (Name = '') then
    Name := '-unnamed-';
  raise EtdStackException.Create(
     FmtLoadStr(aErrorCode,
                [UnitName, ClassName, aMethodName, Name]));
end;

class procedure TtdStack.sGetNodeManager;
begin
  if (SLNodeManager = nil) then
    SLNodeManager := TtdNodeManager.Create(sizeof(TslNode));
end;

{ TtdArrayStack }

procedure TtdArrayStack.asError(aErrorCode: integer;
  const aMethodName: TtdNameString);
begin
  if (Name = '') then
    Name := '-unnamed-';
  raise EtdStackException.Create(
     FmtLoadStr(aErrorCode,
                [UnitName, ClassName, aMethodName, Name]));
end;

procedure TtdArrayStack.asGrow;
begin
  FList.Count := (FList.Count * 3) div 2;
end;

procedure TtdArrayStack.Clear;
var
  I: Integer;
begin
  if FCount > 0 then
  begin
    if Assigned(FDispose) then
      for I := 0 to FCount - 1 do
        FDispose(FList.Items[i]);
    FCount := 0;
  end;
end;

constructor TtdArrayStack.Create(aDispose: TtdDisposeProc; aCapacity: integer);
begin
  FDispose := aDispose;
  FList := TList.Create;
  if aCapacity <= 1 then
    aCapacity := 16;
  FList.Capacity := aCapacity;
  FCount := 0;
end;

destructor TtdArrayStack.Destroy;
begin
  Clear;
  FList.Free;
  inherited;
end;

function TtdArrayStack.Examine: Pointer;
begin
  if (Count = 0) then
    asError(tdeStackIsEmpty, 'Examine');
  Result := FList[pred(Count)];
end;

function TtdArrayStack.IsEmpty: Boolean;
begin
  Result := FCount = 0;
end;

function TtdArrayStack.Pop: Pointer;
begin
  if FCount = 0 then
    asError(tdeStackIsEmpty, 'Pop');
  Result := FList[FCount - 1];
  Dec(FCount);
end;

procedure TtdArrayStack.Push(aItem: Pointer);
begin
  if FCount = FList.Count then
    asGrow;
  FList[FCount] := aItem;
  Inc(FCount);
end;

end.
