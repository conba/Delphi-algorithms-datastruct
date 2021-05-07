program Regex;

uses
  Forms,
  main in 'main.pas' {Form2},
  TDRegex in 'TDRegex.pas',
  TDBasics in 'TDBasics.pas',
  TDLnkLst in 'TDLnkLst.pas',
  TDNdeMgr in 'TDNdeMgr.pas',
  TDStates in 'TDStates.pas',
  TDStkQue in 'TDStkQue.pas',
  TDRecLst in 'TDRecLst.pas',
  TDIntDeq in 'TDIntDeq.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
