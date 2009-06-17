program KaM_Remake;

uses
  Forms,
  KM_Unit1 in 'KM_Unit1.pas' {Form1},
  KM_Form_Loading in 'KM_Form_Loading.pas' {FormLoading},
  KM_Defaults in 'KM_Defaults.pas',
  KM_Render in 'KM_Render.pas',
  KM_ReadGFX1 in 'KM_ReadGFX1.pas',
  KM_Terrain in 'KM_Terrain.pas',
  KM_Viewport in 'KM_Viewport.pas',
  KM_Units in 'KM_Units.pas',
  KM_Users in 'KM_Users.pas',
  KM_Houses in 'KM_Houses.pas',
  KM_DeliverQueue in 'KM_DeliverQueue.pas',
  KM_GamePlayInterface in 'KM_GamePlayInterface.pas',
  KM_RenderUI in 'KM_RenderUI.pas',
  KM_Controls in 'KM_Controls.pas',
  ColorPicker in ' Common \ColorPicker.pas' {Form_ColorPicker},
  KM_LoadLib in 'KM_LoadLib.pas',
  KM_LoadSFX in 'KM_LoadSFX.pas',
  KM_Settings in 'KM_Settings.pas',
  KM_LoadDAT in 'KM_LoadDAT.pas',
  KM_Game in 'KM_Game.pas',
  KM_PathFinding in 'KM_PathFinding.pas',
  KM_Units_WorkPlan in 'KM_Units_WorkPlan.pas';

{$R *.RES}
{}

begin
  Application.Initialize;
  Application.Title := 'KaM Remake';
  Application.HelpFile := '';
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TFormLoading, FormLoading);
  Application.CreateForm(TForm_ColorPicker, Form_ColorPicker);
  Form1.OnCreate(nil);

  Application.Run;

end.
