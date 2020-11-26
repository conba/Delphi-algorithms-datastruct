unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls;

type
  TMyRec = record
    dValue: Double;
    iValue: Integer;
  end;
  pMyRec = ^TMyRec;

  TForm1 = class(TForm)
    btn1: TButton;
    procedure btn1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation
uses
  TDRecLst;
{$R *.dfm}

procedure TForm1.btn1Click(Sender: TObject);
var
  a: TtdRecordList;
  Rec, Rec1: pMyRec;
begin
  a := TtdRecordList.Create(SizeOf(TMyRec));
  New(Rec);
  Rec^.dValue := 1.2;
  Rec^.iValue := 1;
  a.add(Pointer(Rec));
  Rec1 := a[0];
end;

end.
