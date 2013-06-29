program FontEditor;
uses
  Forms,
  umain in 'umain.pas' {frmMain},
  KM_ResourceFontsEdit in '..\..\KM_ResourceFontsEdit.pas',
  KM_ResourceFonts in '..\..\KM_ResourceFonts.pas';

var
    frmMain: TfrmMain;


{$R *.res}


begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
