program TranslationManager;
{$I ..\..\KaM_Remake.inc}
uses
  {$IFDEF FPC} Interfaces, {$ENDIF}
  Forms,
  Unit1 in 'Unit1.pas' {Form1},
  Unit_Text in 'Unit_Text.pas';

{$IFDEF WDC}
{$R *.res}
{$ENDIF}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
