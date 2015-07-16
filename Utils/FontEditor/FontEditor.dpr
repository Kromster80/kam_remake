program FontEditor;
uses
  Forms,
  umain in 'umain.pas', {frmMain}
  Constants in 'Constants.pas';

var
  frmMain: TfrmMain;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
