program TtdNodeManager;

uses
  ExceptionLog,
  Forms,
  Main in 'Main.pas' {Form13},
  TDNdeMgr in 'TDNdeMgr.pas',
  TDBasics in 'TDBasics.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm13, Form13);
  Application.Run;
end.
