program TtdSingleLinkList;

uses
  Forms,
  Main in 'Main.pas' {Form13},
  TDBasics in 'TDBasics.pas',
  TDLnkLst in 'TDLnkLst.pas',
  TDNdeMgr in 'TDNdeMgr.pas',
  TDMyNdeMgr in 'TDMyNdeMgr.pas',
  TDMyLnkLst in 'TDMyLnkLst.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm13, Form13);
  Application.Run;
end.
