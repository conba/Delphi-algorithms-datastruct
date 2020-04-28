program TtdStack;

uses
  Forms,
  main in 'main.pas' {Form13},
  TDStkQue in 'TDStkQue.pas',
  TDBasics in 'TDBasics.pas',
  TDLnkLst in 'TDLnkLst.pas',
  TDNdeMgr in 'TDNdeMgr.pas',
  TDMyStack in 'TDMyStack.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm13, Form13);
  Application.Run;
end.
