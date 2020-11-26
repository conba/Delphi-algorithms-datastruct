(********************************************************************)
(* Tomes of Delphi: Algorithms and Data Structures                  *)
(* Source code copyright (c) Julian M Bucknall, 1999-2001           *)
(* All rights reserved                                              *)
(*------------------------------------------------------------------*)
(* TDRecLst                                                         *)
(* Record list class                                                *)
(********************************************************************)

unit TDRecLst;

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
  TtdRecordList = class
  private
    FActElemSize: integer;
    FArray: PAnsiChar;   // �ַ�������ʼ��ַ
    FCount: integer;
    FCapacity: integer;
    FElementSize: integer;
    FIsSorted: boolean;
    FMaxElemCount: integer;
    FName: TtdNameString;
  protected
    function rlGetItem(aIndex: integer): pointer;
    procedure rlSetCapacity(aCapacity: integer);
    procedure rlSetCount(aCount: integer);
    function rlBinarySearch(aItem: pointer; aCompare: TtdCompareFunc;
      var aInx: integer): boolean;
    procedure rlError(aErrorCode: integer; const aMethodName: TtdNameString;
      aIndex: integer);
    procedure rlExpand;
  public
    constructor Create(aElementSize: integer);
    destructor Destroy; override;
    function Add(aItem: pointer): integer;
    procedure Clear;
    procedure Delete(aIndex: integer);
    procedure Exchange(aIndex1, aIndex2: integer);
    function First: pointer;
    function IndexOf(aItem: pointer; aCompare: TtdCompareFunc): integer;
    procedure Insert(aIndex: integer; aItem: pointer);
    function InsertSorted(aItem: pointer; aCompare: TtdCompareFunc): integer;
    function Last: pointer;
    procedure Move(aCurIndex, aNewIndex: integer);
    function Remove(aItem: pointer; aCompare: TtdCompareFunc): integer;
    procedure Sort(aCompare: TtdCompareFunc);
    property Capacity: integer read FCapacity write rlSetCapacity;
    property Count: integer read FCount write rlSetCount;
    property ElementSize: integer read FActElemSize;
    property IsSorted: boolean read FIsSorted;
    property Items[aIndex: integer]: pointer read rlGetItem; default;
    property MaxCount: integer read FMaxElemCount;
    property Name: TtdNameString read FName write FName;
  end;

implementation

{ TtdRecordList }

function TtdRecordList.Add(aItem: pointer): integer;
begin
  Result := Count;
  Insert(Count, aItem);
end;

procedure TtdRecordList.Clear;
begin

end;

constructor TtdRecordList.Create(aElementSize: integer);
begin
  // ����Ԫ��ʵ�ʵĴ�С
  FActElemSize := aElementSize;
  // ����Ԫ���������д洢�Ĵ�С����4�ֽڵ�λ��
  FElementSize := ((aElementSize + 3) shr 2) shl 2;
  // ����Ԫ�����ĸ���
  {$IFDEF  Delphi1}
  FMaxElemCount := 65535 div FElementSize;
  {$ELSE}
  FMaxElemCount := MaxInt div FElementSize;
  {$ENDIF}
end;

procedure TtdRecordList.Delete(aIndex: integer);
begin
  if (aIndex < 0) or (aIndex >= Count) then
    rlError(tdeIndexOutOfBounds, 'Delete', aIndex);
  Dec(FCount);
  if (aIndex < Count) then
    System.Move((FArray + (succ(aIndex) * FElementSize))^,
      (FArray + System.Succ(aIndex * FElementSize))^,
      (Count - aIndex) * FElementSize);
end;

destructor TtdRecordList.Destroy;
begin
  Capacity := 0;
  inherited;
end;

procedure TtdRecordList.Exchange(aIndex1, aIndex2: integer);
begin

end;

function TtdRecordList.First: pointer;
begin

end;

function TtdRecordList.IndexOf(aItem: pointer;
  aCompare: TtdCompareFunc): integer;
var
  ElementPtr: PAnsiChar;
  i: Integer;
begin
  ElementPtr := FArray;
  for I := 0 to System.Pred(Count) do
  begin
    if (aCompare(aItem, ElementPtr) = 0) then
    begin
      Result := i;
      Exit;
    end;
    System.Inc(ElementPtr, FElementSize);
  end;
  Result := -1;
end;

procedure TtdRecordList.Insert(aIndex: integer; aItem: pointer);
begin
  if aItem = nil then
    rlError(tdeNilItem, 'Insert', aIndex);
  if (aIndex < 0) or (aIndex > Count) then
    rlError(tdeIndexOutOfBounds, 'Insert', aIndex);
  if Count = Capacity then
    rlExpand;
//  if aIndex < Count then
//    System.Move((FArray + (aIndex * FElementSize))^,
//      (FArray + (succ(aIndex) * FElementSize))^,
//      (Count - aIndex) * FElementSize);
  if aIndex < Count then
    System.Move((FArray + (aIndex * FElementSize))^,
      (FArray + (System.Succ(aIndex) * FElementSize))^,
      (Count - aIndex) * FElementSize);
  System.Move(aItem^, (FArray + (aIndex * FElementSize))^,
    FActElemSize);
end;

function TtdRecordList.InsertSorted(aItem: pointer;
  aCompare: TtdCompareFunc): integer;
begin

end;

function TtdRecordList.Last: pointer;
begin

end;

procedure TtdRecordList.Move(aCurIndex, aNewIndex: integer);
begin

end;

function TtdRecordList.Remove(aItem: pointer;
  aCompare: TtdCompareFunc): integer;
begin
  Result := IndexOf(aItem, aCompare);
  if (Result <> -1) then
    Delete(Result);
end;

function TtdRecordList.rlBinarySearch(aItem: pointer; aCompare: TtdCompareFunc;
  var aInx: integer): boolean;
begin

end;

procedure TtdRecordList.rlError(aErrorCode: integer;
  const aMethodName: TtdNameString; aIndex: integer);
begin

end;

procedure TtdRecordList.rlExpand;
var
  NewCapacity: Integer;
begin
  if Capacity = 0 then
    NewCapacity := 4
  {�����ǰ����С��64����ʹ�������ڵ�ǰ����������16��Ԫ��}
  else if (Capacity < 64) then
    NewCapacity := Capacity + 16
  {�����ǰ�������ڵ���64����ʹ�����������ӵ�ǰ������1/4}
  else
    NewCapacity := Capacity + (Capacity div 4);
  {ȷ�������ڳ������������}
  if (NewCapacity > FMaxElemCount) then
    NewCapacity := FMaxElemCount;
  if (NewCapacity = Capacity) then
    rlError(tdeAtMaxCapacity, 'rlExpand', 0);
  Capacity := NewCapacity;
end;

function TtdRecordList.rlGetItem(aIndex: integer): pointer;
begin
  if (aIndex < 0) or (aIndex >= Count) then
    rlError(tdeIndexOutOfBounds, 'rlGetItem', aIndex);
  Result := Pointer(FArray + (aIndex * FElementSize));
end;

procedure TtdRecordList.rlSetCapacity(aCapacity: integer);
begin
  if aCapacity <> FCapacity then
  begin
    {��Ҫ�������Ԫ�ظ���}
    if aCapacity > FMaxElemCount then
      rlError(tdeCapacityTooLarge, 'rlSetCapacity', 0);
    {���·������飬��������������0ʱ�����ͷ�}
    {$IFDEF Delphi1}
    if aCapacity = 0 then
    begin
      FreeMem(FArray, word(FCapacity) * FElementSize);
      FArray := nil;
    end
    else
    begin
      if FCapacity = 0 then
        GetMem(FArray, Word(aCapacity) * FElementSize)
      else
        FArray := ReallocMen(FArray, Word(FCapacity) * FElementSize,
          Word(aCapacity) * FElementSize);
    end;
    {$ELSE}
    ReallocMem(FArray, aCapacity * FElementSize);
    {$ENDIF}
    {����������С�����������������Ӧ��Ŀ}
    if aCapacity < FCapaCity then
    begin
      if Count > aCapacity then
        Count := aCapacity;
    end;
    FCapacity := aCapacity;
  end;
end;

procedure TtdRecordList.rlSetCount(aCount: integer);
begin
  if aCount <> FCount then
  begin
    {����µĸ���������������������չ}
    if aCount > FCapacity then
      Capacity := aCount;
    {����µĸ�������ԭ�и�����������Ԫ��Ϊ������0}
    if aCount > FCount then
      FillChar((FArray + (FCount * FElementSize))^,
      (aCount - FCount) * FElementSize, 0);
    {�����µ�Ԫ�ظ���}
    FCount := aCount;
  end;
end;

procedure TtdRecordList.Sort(aCompare: TtdCompareFunc);
begin

end;

end.
