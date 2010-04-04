program KaM_Remake;
{$IFDEF FPC}
  {$Mode Delphi}
{$ENDIF}

uses
  //FastMM4, //Can be used only in Delphi, not Lazarus
  Forms,
  {$IFDEF FPC} Interfaces, {$ENDIF}
  KM_Unit1 in 'KM_Unit1.pas' {Form1},
  KM_Form_Loading in 'KM_Form_Loading.pas' {FormLoading},
  ColorPicker in ' Common \ColorPicker.pas' {Form_ColorPicker},
  KM_Defaults in 'KM_Defaults.pas',
  KM_Render in 'KM_Render.pas',
  KM_ResourceGFX in 'KM_ResourceGFX.pas',
  KM_Terrain in 'KM_Terrain.pas',
  KM_Viewport in 'KM_Viewport.pas',
  KM_Units in 'KM_Units.pas',
  KM_PlayersCollection in 'KM_PlayersCollection.pas',
  KM_Houses in 'KM_Houses.pas',
  KM_DeliverQueue in 'KM_DeliverQueue.pas',
  KM_InterfaceGamePlay in 'KM_InterfaceGamePlay.pas',
  KM_InterfaceMainMenu in 'KM_InterfaceMainMenu.pas',
  KM_InterfaceMapEditor in 'KM_InterfaceMapEditor.pas',
  KM_RenderUI in 'KM_RenderUI.pas',
  KM_Controls in 'KM_Controls.pas',
  KM_LoadLib in 'KM_LoadLib.pas',
  KM_SoundFX in 'KM_SoundFX.pas',
  KM_Settings in 'KM_Settings.pas',
  KM_LoadDAT in 'KM_LoadDAT.pas',
  KM_Game in 'KM_Game.pas',
  KM_PathFinding in 'KM_PathFinding.pas',
  KM_Units_WorkPlan in 'KM_Units_WorkPlan.pas',
  KM_PlayerAI in 'KM_PlayerAI.pas',
  KM_Player in 'KM_Player.pas',
  KM_CommonTypes in 'KM_CommonTypes.pas',
  KM_Utils in 'KM_Utils.pas',
  KM_TGATexture in ' Common \KM_TGATexture.pas',
  KM_UnitActionAbandonWalk in 'KM_UnitActionAbandonWalk.pas',
  KM_UnitActionFight in 'KM_UnitActionFight.pas',
  KM_UnitActionGoInOut in 'KM_UnitActionGoInOut.pas',
  KM_UnitActionStay in 'KM_UnitActionStay.pas',
  KM_UnitActionWalkTo in 'KM_UnitActionWalkTo.pas',
  KM_UnitTaskDelivery in 'KM_UnitTaskDelivery.pas',
  KM_UnitTaskMining in 'KM_UnitTaskMining.pas',
  KM_UnitTaskAttackHouse in 'KM_UnitTaskAttackHouse.pas';


{$IFDEF VER140}
{$R *.RES}
{$ENDIF}


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
