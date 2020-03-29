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
    FArray: PAnsiChar;   // 字符串的起始地址
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
  inherited;
  // 保留元素实际的大小
  FActElemSize := aElementSize;
  // 保留元素在数组中存储的大小（以4字节单位）
  FElementSize := ((aElementSize + 3) shr 2) shl 2;
  // 保留元素最大的个数
  FMaxElemCount := MaxInt div FElementSize;
end;

procedure TtdRecordList.Delete(aIndex: integer);
begin

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
begin

end;

procedure TtdRecordList.Insert(aIndex: integer; aItem: pointer);
begin
  if aItem = nil then
    rlError(tdeNilItem, 'Insert', aIndex);
  if (aIndex < 0) or (aIndex > Count) then
    rlError(tdeIndexOutOfBounds, 'Insert', aIndex);
  if Count = Capacity then
    rlExpand;
  if aIndex < Count then
    System.Move((FArray + (aIndex * FElementSize))^,
      (FArray + (succ(aIndex) * FElementSize))^,
      (Count - aIndex) * FElementSize);
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
begin

end;

function TtdRecordList.rlGetItem(aIndex: integer): pointer;
begin

end;

procedure TtdRecordList.rlSetCapacity(aCapacity: integer);
begin

end;

procedure TtdRecordList.rlSetCount(aCount: integer);
begin

end;

procedure TtdRecordList.Sort(aCompare: TtdCompareFunc);
begin

end;

end.
