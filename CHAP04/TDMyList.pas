// 由于顺序查找过于简单就不在重新编写代码
unit TDMyList;

{$I TDDefine.inc}

interface
uses
  TDBasics, uSysUtils, Classes;

// 二分查找
function TDTListSortedIndexOf(aList : TList; aItem : pointer;
                              aCompare : TtdCompareFunc) : integer;

// 基于二分查找的插入算法
function TDTListSortedInsert(aList: TList; aItem: Pointer;
                             aCompare: TtdCompareFunc): Integer;

implementation

function TDTListSortedIndexOf(aList : TList; aItem : pointer;
                              aCompare : TtdCompareFunc) : integer;
var
  iLeft, iRight, iMid: Integer;
  iCompareResult: Integer;
begin
  iLeft := 0;
  iRight := aList.Count - 1;
  while iLeft < iRight do
  begin
    iMid := (iLeft + iRight) div 2;
    iCompareResult := aCompare(aList.List^[iMid], aItem);
    // 如果中间值小于查找值
    if iCompareResult < 0 then
      iLeft := iMid + 1
    else if iCompareResult < iRight then
      iRight := iMid - 1
    else
    begin
      Result := iMid
      Exit;
    end;
  end;
  Result := -1;       // 如果没有找到
end;

function TDTListSortedInsert(aList: TList; aItem: Pointer;
                             aCompare: TtdCompareFunc): Integer;
var
  iLeft, iRight, iMid: Integer;
  iCompareResult: Integer;
begin
  iLeft := 0;
  iRight := aList.Count;
  while iLeft < iRight do
  begin
    iMid := (iLeft + iRight) div 2;
    iCompareResult := aCompare(aList.list^[iMid], aItem);
    if iCompareResult < 0 then
      iLeft := iMid =1
    else if iCompareResult > 0 then
      iRight := iMid - 1
    else
    begin
      Result := iMid;
      Exit;
    end;
  end;
  Result := iLeft;
  aList.Insert(iLeft, aItem);
end;

end.
