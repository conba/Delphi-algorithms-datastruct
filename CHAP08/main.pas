unit main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, TDBinTre;

type
  TMyRec = record
    c: string;
  end;
  pMyRec = ^TMyRec;

  TForm1 = class(TForm)
    btn1: TButton;
    procedure btn1Click(Sender: TObject);
  private
    FBinTree: TtdBinaryTree;
    { Private declarations }
    procedure InitBinTree();
  public
    { Public declarations }
  end;

procedure Actions(aData: pointer; aExtraData: pointer; var aStopVisits: boolean);

procedure DisPoseData(aData: pointer);

var
  Form1: TForm1;

implementation

{$R *.dfm}
procedure Actions(aData: pointer; aExtraData: pointer; var aStopVisits: boolean);
begin
  ShowMessage(pMyRec(aData)^.c);
end;

procedure DisPoseData(aData: pointer);
begin
  Dispose(pMyRec(aData));
end;

procedure TForm1.btn1Click(Sender: TObject);
begin
  ShowMessage(IntToStr(i));
  InitBinTree;
//  FBinTree.Traverse(tmPreOrder, Actions, nil, False);
//  FBinTree.Traverse(tmInOrder, Actions, nil, False);
  FBinTree.btNoRecInOrder1(actions);
//  FBinTree.Traverse(tmPostOrder, Actions, nil, False);
end;

{ TForm1 }

procedure TForm1.InitBinTree;
var
  MyRec: pMyRec;
  I: Integer;
  BinNode1, BinNode2, BinNode3, BinNode4, BinNode5, BinNode6, BinNode7: PtdBinTreeNode;
begin
  FBinTree := TtdBinaryTree.Create(DisPoseData);
  New(MyRec);
  Myrec^.c := #100;
  BinNode1 := FBinTree.InsertAt(nil, ctLeft, Pointer(Myrec));
  New(MyRec);
  Myrec^.c := #98;
  BinNode2 := FBinTree.InsertAt(BinNode1, ctLeft, Pointer(Myrec));
  New(MyRec);
  Myrec^.c := #102;
  BinNode3 := FBinTree.InsertAt(BinNode1, ctRight, Pointer(Myrec));
  New(MyRec);
  Myrec^.c := #97;
  BinNode4 := FBinTree.InsertAt(BinNode2, ctLeft, Pointer(Myrec));
  New(MyRec);
  Myrec^.c := #99;
  BinNode5 := FBinTree.InsertAt(BinNode2, ctRight, Pointer(Myrec));
  New(MyRec);
  Myrec^.c := #101;
  BinNode6 := FBinTree.InsertAt(BinNode3, ctLeft, Pointer(Myrec));
  New(MyRec);
  Myrec^.c := #103;
  BinNode7 := FBinTree.InsertAt(BinNode3, ctRight, Pointer(Myrec));
end;

end.
