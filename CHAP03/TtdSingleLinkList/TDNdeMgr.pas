(********************************************************************)
(* Tomes of Delphi: Algorithms and Data Structures                  *)
(* Source code copyright (c) Julian M Bucknall, 2000                *)
(* All rights reserved                                              *)
(*------------------------------------------------------------------*)
(* TDNdeMgr                                                         *)
(* Node Memory Manager                                              *)
(********************************************************************)

unit TDNdeMgr;

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

{The following compiler define tells the compiler whether to have the
 node manager class use the heap for all allocations or not; for test
 purposes only}
{.$DEFINE UseHeap}

type
  TtdNodeManager = class
    private
      FNodeSize     : cardinal;
      {$IFNDEF UseHeap}
      FFreeList     : pointer;
      FNodesPerPage : cardinal;
      FPageHead     : pointer;
      FPageSize     : cardinal;
      {$ENDIF}
      FName    : TtdNameString;
    protected
      {$IFNDEF UseHeap}
      procedure nmAllocNewPage;
      {$ENDIF}
      {$IFDEF DebugMode}
      procedure nmError(aErrorCode  : integer;
                  const aMethodName : TtdNameString);
      procedure nmValidateNode(aNode : pointer);
      {$ENDIF}
    public
      constructor Create(aNodeSize : cardinal);
      destructor Destroy; override;

      function AllocNode : pointer;
      function AllocNodeClear : pointer;
      procedure FreeNode(aNode : pointer);

      property Name : TtdNameString
         read FName write FName;
  end;

implementation

const
  UnitName = 'TDNdeMgr';

const
  PageSize = 1024;

type
  PGenericNode = ^TGenericNode;
  TGenericNode = packed record
    gnNext : PGenericNode;
    gnData : record end;
  end;


constructor TtdNodeManager.Create(aNodeSize : cardinal);
begin
  inherited Create;
  {���ֽڴ�Сȡ��Ϊ��ӽ���4�ֽڱ���������}
  if (aNodeSize <= sizeof(pointer)) then
    aNodeSize := sizeof(pointer)
  else
    aNodeSize := ((aNodeSize + 3) shr 2) shl 2;
  FNodeSize := aNodeSize;
  {$IFNDEF UseHeap}
  {calculate the page size (default 1024 bytes) and the number of
   nodes per page; if the default page size is not large enough for
   two or more nodes, force a single node per page}
  {����ҳ���С(Ĭ��Ϊ1024�ֽ�)�Լ�ÿҳ�ϵĽڵ����� ���ÿ��ҳ�治��������2������
   ����ڵ㣬��ÿ��ҳ��ֻ���һ���ڵ���ҳ���С���ڽڵ��С+ָ���С}
  FNodesPerPage := (PageSize - sizeof(pointer)) div aNodeSize;
  if (FNodesPerPage > 1) then
    FPageSize := 1024
  else begin
    FNodesPerPage := 1;
    FPagesize := aNodeSize + sizeof(pointer);
  end;
  {$ENDIF}
end;

destructor TtdNodeManager.Destroy;
{$IFNDEF UseHeap}
var
  Temp : pointer;
{$ENDIF}
begin
  {$IFNDEF UseHeap}
  {dispose of all the pages, if there are any}
  {�������ҳ�棬��ȫ���ͷ�}
  while (FPageHead <> nil) do
  begin
    Temp := PGenericNode(FPageHead)^.gnNext;
    FreeMem(FPageHead, FPageSize);
    FPageHead := Temp;
  end;
  {$ENDIF}
  inherited Destroy;
end;

function TtdNodeManager.AllocNode : pointer;
begin
  {$IFDEF UseHeap}
  GetMem(Result, FNodeSize);
  {$ELSE}
  {if the free list is empty, allocate a new page; this'll fill the
   free list}
  {��������б�Ϊ�գ�����һ���µ�ҳ�棻������������б�}
  if (FFreeList = nil) then
    nmAllocNewPage;
  {return the top of the free list}
  {���ؿ����б�Ķ����ڵ�}
  Result := FFreeList;
  FFreeList := PGenericNode(FFreeList)^.gnNext;
  {$ENDIF}
end;

function TtdNodeManager.AllocNodeClear : pointer;
begin
  {$IFDEF UseHeap}
  GetMem(Result, FNodeSize);
  {$ELSE}
  {if the free list is empty, allocate a new page; this'll fill the
   free list}
  if (FFreeList = nil) then
    nmAllocNewPage;
  {return the top of the free list}
  Result := FFreeList;
  FFreeList := PGenericNode(FFreeList)^.gnNext;
  {$ENDIF}
  FillChar(Result^, FNodeSize, 0);
end;

procedure TtdNodeManager.FreeNode(aNode : pointer);
begin
  {$IFDEF UseHeap}
  FreeMem(aNode, FNodeSize);
  {$ELSE}
  {add the node (if non-nil) to the top of the free list}
  {���ڵ�(����ǿ�)�ӵ������б���}
  if (aNode <> nil) then
  begin
    {$IFDEF DebugMode}
    nmValidateNode(aNode);
    {$ENDIF}
    PGenericNode(aNode)^.gnNext := FFreeList;
    FFreeList := aNode;
  end;
  {$ENDIF}
end;

{$IFNDEF UseHeap}
procedure TtdNodeManager.nmAllocNewPage;
var
  NewPage : PAnsiChar;
  i       : integer;
begin
  {allocate a new page and add it to the front of the page list}
  {����һ���µ�ҳ�棬�������ӵ�ҳ���б���ǰ��}
  GetMem(NewPage, FPageSize);
  PGenericNode(NewPage)^.gnNext := FPageHead;
  FPageHead := NewPage;
  {now split up the new page into nodes and push them all onto the
   free list; note that the first 4 bytes of the page is a pointer to
   the next page, so remember to skip over them}
  {���ڽ���ҳ�滮��Ϊ�ڵ㣬�������нڵ��ѹ�뵽�����б��У�ע��ҳ���ǰ�ĸ��ֽ�
   Ϊָ����һ��ҳ���ָ�룬���һ��Ҫ������4���ֽ�}
  inc(NewPage, sizeof(pointer));
  for i := pred(FNodesPerPage) downto 0 do
  begin
    FreeNode(NewPage);
    inc(NewPage, FNodeSize);
  end;
end;
{$ENDIF}

{$IFDEF DebugMode}
procedure TtdNodeManager.nmError(aErrorCode  : integer;
                           const aMethodName : TtdNameString);
begin
  if (Name = '') then
    Name := '-unnamed-';
  raise EtdNodeMgrException.Create(
     FmtLoadStr(aErrorCode,
                [UnitName, ClassName, aMethodName, Name]));
end;
{$ENDIF}

{$IFDEF DebugMode}
procedure TtdNodeManager.nmValidateNode(aNode : pointer);
var
  Page : PAnsiChar;
begin
  {validate aNode to be on a page}
  Page := FPageHead;
  while (Page <> nil) do
  begin
    if (Page < aNode) and (aNode < Page+FPageSize) then
      Exit;
    Page := PAnsiChar(PGenericNode(Page)^.gnNext);
  end;
  nmError(tdeInvalidNode, 'FreeNode');
end;
{$ENDIF}

end.

