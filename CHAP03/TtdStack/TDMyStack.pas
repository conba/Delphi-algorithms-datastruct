unit TDMyStack;

{$I TDDefine.inc}

interface
uses
  TDBasics, TDLnkLst, SysUtils;
type
  TtdStack = class
    private
      FCount   : longint;
      FDispose : TtdDisposeProc;
      FHead    : PslNode;
      FName    : TtdNameString;
    protected
      procedure sError(aErrorCode  : integer;
                 const aMethodName : TtdNameString);
      class procedure sGetNodeManager;
    public
      constructor Create(aDispose : TtdDisposeProc);
      destructor Destroy; override;

      procedure Clear;
      function Examine : pointer;
      function IsEmpty : boolean;
      function Pop : pointer;
      procedure Push(aItem : pointer);

      property Count : longint
         read FCount;
      property Name : TtdNameString
         read FName write FName;
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
  FHead := SLNodeManager.AllocNode;
  FHead^.slnNext := nil;
  FHead^.slnData := nil;
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

end.
