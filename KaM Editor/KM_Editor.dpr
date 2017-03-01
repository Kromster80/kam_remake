program KM_Editor;
{$I ..\KaM_Remake.inc}
uses
  Forms,
  SysUtils,
  {$IFDEF FPC} Interfaces, {$ENDIF}
  KM_Unit1 in 'KM_Unit1.pas' {Form1},
  KM_Defaults in 'KM_Defaults.pas',
  KM_Render in 'KM_Render.pas',
  KM_ReadGFX1 in 'KM_ReadGFX1.pas',
  KM_Form_NewMap in 'KM_Form_NewMap.pas' {FormNewMap},
  KM_Form_Loading in 'KM_Form_Loading.pas' {FormLoading},
  KM_LoadDAT in 'KM_LoadDAT.pas',
  KM_TPlayer in 'KM_TPlayer.pas',
  KM_TGATexture in 'KM_TGATexture.pas';

{$IFDEF WDC}
  {$R *.RES}
{$ENDIF}

begin
  Application.Initialize;
  Application.Title := 'KaM Editor';
  Application.HelpFile := 'KM_EDITOR.HLP';
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TFormLoading, FormLoading);
  Application.CreateForm(TFormNewMap, FormNewMap);
  Form1.HelpFile:=ExtractFilePath(ParamStr(0)) + 'KM_EDITOR.HLP';
  FormNewMap.HelpFile:=ExtractFilePath(ParamStr(0)) + 'KM_EDITOR.HLP';

  Application.Run;
end.
