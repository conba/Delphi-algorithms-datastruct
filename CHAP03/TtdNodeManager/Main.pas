unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, TDNdeMgr, StdCtrls;

type
  TForm13 = class(TForm)
    btn1: TButton;
    procedure btn1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

  PGenericNode = ^TGenericNode;
  TGenericNode = packed record
    gnNext : PGenericNode;
    gnData : record end;
  end;

var
  Form13: TForm13;

implementation

{$R *.dfm}

procedure TForm13.btn1Click(Sender: TObject);
var
  NodeManager: TtdNodeManager;
  i: Integer;
  P: Pointer;
begin
  i := SizeOf(TGenericNode);
  NodeManager := TtdNodeManager.Create(SizeOf(TGenericNode));
  P := NodeManager.AllocNode;
end;

end.
