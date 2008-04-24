program KM_Editor;

uses
  Forms,
  sysutils,
  KM_Unit1 in 'KM_Unit1.pas' {Form1},
  KM_Defaults in 'KM_Defaults.pas',
  KM_Render in 'KM_Render.pas',
  KM_ReadGFX1 in 'KM_ReadGFX1.pas',
  KM_Form_NewMap in 'KM_Form_NewMap.pas' {FormNewMap},
  KM_Form_Loading in 'KM_Form_Loading.pas' {FormLoading},
  KM_LoadDAT in 'KM_LoadDAT.pas',
  KM_TPlayer in 'KM_TPlayer.pas',
  KM_MapSettings in 'KM_MapSettings.pas' {Form_MapSettings},
  KM_Terrain in 'KM_Terrain.pas',
  KM_Global_Data in 'KM_Global_Data.pas',
  KM_Viewport in 'KM_Viewport.pas',
  KM_Units in 'KM_Units.pas',
  KM_Users in 'KM_Users.pas',
  KM_Houses in 'KM_Houses.pas',
  KM_Log in 'KM_Log.pas',
  KM_DeliverQueue in 'KM_DeliverQueue.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'KaM Editor';
  Application.HelpFile := '';
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TFormLoading, FormLoading);
  Application.CreateForm(TFormNewMap, FormNewMap);
  Application.CreateForm(TForm_MapSettings, Form_MapSettings);
  Form1.HelpFile:=ExtractFilePath(Application.ExeName)+'KM_EDITOR.HLP';
  FormNewMap.HelpFile:=ExtractFilePath(Application.ExeName)+'KM_EDITOR.HLP';

  Application.Run;
end.
