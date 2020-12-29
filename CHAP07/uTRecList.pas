unit uTRecList;

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
  TRecList = class
  private
    FActElemSize: Integer;
    FArrayChar: PAnsiChar;     // 字符串的起始指针，也是起始地址。
    FCount: Integer;
    FCapacity: Integer;
    FElementSize: Integer;
    FIsSorted: Boolean;
    FMaxElemCount: Integer;
    FName: TtdNameString;
  protected
    procedure rlError(aErrorCode: integer; const aMethodName: TtdNameString; aIndex: integer);
  public
    constructor Create(AElementSize: Integer);
    destructor Destroy; override;
    // 返回总记录个数
    function Add(AItem: Pointer): Integer;
    procedure Insert(AIndex: Integer; AItem: Pointer);
    property Capacity: Integer read FCapacity write FCapacity;
    property Count: Integer read FCount;
    property Name: TtdNameString read FName write FName;
  end;


implementation

{ TRecList }

function TRecList.Add(AItem: Pointer): Integer;
begin
  Result := Count;
  Insert(Count, AItem);
end;

constructor TRecList.Create(AElementSize: Integer);
begin
  inherited Create();
  FActElemSize := AElementSize;
  FElementSize := ((FActElemSize + 3) shr 2) shl 2;
  {$IFDEF Delphi1}
  FMaxElemCount := 65535 div FElementSize;
  {$ELSE}
  FMaxElemCount := MaxInt div Integer(FElementSize);
  {$ENDIF}
  FIsSorted := True;
end;

destructor TRecList.Destroy;
begin
  Capacity := 0;
  inherited;
end;

procedure TRecList.Insert(AIndex: Integer; AItem: Pointer);
begin

end;

procedure TRecList.rlError(aErrorCode: integer;
  const aMethodName: TtdNameString; aIndex: integer);
begin

end;

end.
