unit main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
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
  TDStates;

{$R *.dfm}

procedure TForm1.btn1Click(Sender: TObject);
var
  Str: string;
  StrList: TStrings;
begin
  Str := 'He said, "State machines?"';
  StrList := TStringList.Create;
  try
    TDExtractWords(Str, StrList);
  finally
    StrList.Free;
  end;
end;

end.
