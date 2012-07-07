program RXXPacker;
{$I ..\..\KaM_Remake.inc}
uses
  //FastMM4,
  Forms,
  {$IFDEF FPC} Interfaces, {$ENDIF}
  RXXPackerForm in 'RXXPackerForm.pas' {RXXForm1},
  KM_ResourceSprites in '..\..\KM_ResourceSprites.pas',
  KM_ResourceSpritesEdit in '..\..\KM_ResourceSpritesEdit.pas',
  KM_SoftShadows in '..\..\KM_SoftShadows.pas';


{$IFDEF WDC}
{$R *.res}
{$ENDIF}


begin
  Application.Initialize;
  Application.CreateForm(TRXXForm1, RXXForm1);
  Application.Run;
end.
