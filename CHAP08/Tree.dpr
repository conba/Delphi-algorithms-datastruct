program Tree;

uses
  ExceptionLog,
  Forms,
  main in 'main.pas' {Form1},
  TDBasics in 'TDBasics.pas',
  TDBinTre in 'TDBinTre.pas',
  TDNdeMgr in 'TDNdeMgr.pas',
  TDStkQue in 'TDStkQue.pas',
  TDLnkLst in 'TDLnkLst.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
