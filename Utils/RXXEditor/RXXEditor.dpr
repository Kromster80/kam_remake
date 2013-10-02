program RXXEditor;
{$I ..\..\KaM_Remake.inc}
uses
  Forms,
  RXXEditorForm in 'RXXEditorForm.pas' {RXXForm1},
  KM_ResSprites in '..\..\src\res\KM_ResSprites.pas',
  KM_ResSpritesEdit in '..\..\src\res\KM_ResSpritesEdit.pas';

{$IFDEF WDC}
{$R *.res}
{$ENDIF}


begin
  Application.Initialize;
  Application.CreateForm(TRXXForm1, RXXForm1);
  Application.Run;
end.
