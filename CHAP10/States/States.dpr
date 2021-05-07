program States;

uses
  Forms,
  main in 'main.pas' {Form1},
  TDBasics in 'TDBasics.pas',
  TDStates in 'TDStates.pas',
  TDStkQue in 'TDStkQue.pas',
  TDLnkLst in 'TDLnkLst.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
