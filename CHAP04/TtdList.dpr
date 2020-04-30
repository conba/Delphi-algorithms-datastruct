program TtdList;

uses
  Forms,
  main in 'main.pas' {Form13},
  TDTList in 'TDTList.pas',
  TDBasics in 'TDBasics.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm13, Form13);
  Application.Run;
end.
