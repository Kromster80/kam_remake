program RXXEditor;
{$I ..\..\KaM_Remake.inc}
uses
  Forms,
  RXXEditorForm in 'RXXEditorForm.pas' {RXXForm1},
  KM_ResourceSprites in '..\..\KM_ResourceSprites.pas',
  KM_ResourceSpritesEdit in '..\..\KM_ResourceSpritesEdit.pas';

{$IFDEF WDC}
{$R *.res}
{$ENDIF}


begin
  Application.Initialize;
  Application.CreateForm(TRXXForm1, RXXForm1);
  Application.Run;
end.
