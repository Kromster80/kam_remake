program RXXPacker;
{$I ..\..\KaM_Remake.inc}
uses
  Forms,
  {$IFDEF FPC}Interfaces,{$ENDIF}
  RXXPackerForm in 'RXXPackerForm.pas' {RXXForm1},
  KM_PNG in '..\..\Common\KM_PNG.pas',
  KM_ResSprites in '..\..\KM_ResSprites.pas',
  KM_ResSpritesEdit in '..\..\KM_ResSpritesEdit.pas',
  KM_SoftShadows in '..\..\KM_SoftShadows.pas';

{$IFDEF WDC}
{$R *.res}
{$ENDIF}


begin
  Application.Initialize;
  Application.CreateForm(TRXXForm1, RXXForm1);
  Application.Run;
end.
