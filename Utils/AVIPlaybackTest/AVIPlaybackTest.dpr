program AVIPlaybackTest;

uses
  Forms,
  FormMain in 'FormMain.pas' {FrmMain},
  SAVIUnit in 'SAVIUnit.pas',
  OALHandler in 'OALHandler.pas',
  openal_main in 'openal_main.pas',
  openal_types in 'openal_types.pas',
  VFW in 'VFW.pas';

{$R *.res}

var
  FrmMain: TFrmMain;

begin
  Application.Initialize;
  Application.CreateForm(TFrmMain, FrmMain);
  Application.Run;
end.
