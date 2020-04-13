unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, TDLnkLst;

type
  TForm13 = class(TForm)
    btn1: TButton;
    procedure btn1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

procedure MyDisposeProc(aData: pointer);

var
  Form13: TForm13;

implementation

{$R *.dfm}

procedure TForm13.btn1Click(Sender: TObject);
var
  MyList: TtdSingleLinkList;
  P: Pointer;
begin
  MyList := TtdSingleLinkList.Create(MyDisposeProc);
end;

procedure MyDisposeProc(aData: pointer);
begin

end;

end.
