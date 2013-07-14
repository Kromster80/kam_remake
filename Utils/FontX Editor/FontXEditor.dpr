program FontXEditor;
uses
  Forms,
  umain in 'umain.pas' {frmMain},
  KM_ResFonts in '..\..\KM_ResFonts.pas',
  KM_ResFontsEdit in '..\..\KM_ResFontsEdit.pas';

var
    frmMain: TfrmMain;


{$R *.res}


begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
