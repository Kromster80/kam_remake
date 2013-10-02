program FontXCollator;
{$I ..\..\KaM_Remake.inc}
uses
  Forms,
  {$IFDEF FPC} Interfaces, {$ENDIF}
  Form_Collator in 'Form_Collator.pas' {Form1},
  KM_ResFonts in '..\..\src\res\KM_ResFonts.pas',
  KM_ResFontsEdit in '..\..\src\res\KM_ResFontsEdit.pas',
  KM_FontCollator in 'KM_FontCollator.pas';

begin
  Application.Initialize;
  {$IFDEF FPC} Application.MainFormOnTaskbar := True; {$ENDIF}
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
