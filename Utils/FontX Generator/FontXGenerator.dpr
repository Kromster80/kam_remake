program FontXGenerator;
{$I ..\..\KaM_Remake.inc}
uses
  Forms,
  {$IFDEF FPC} Interfaces, {$ENDIF}
  Unit_Main in 'Unit_Main.pas' {Form1},
  KM_ResFonts in '..\..\KM_ResFonts.pas',
  KM_ResFontsEdit in '..\..\KM_ResFontsEdit.pas',
  KM_FontCollator in 'KM_FontCollator.pas';

begin
  Application.Initialize;
  {$IFDEF FPC} Application.MainFormOnTaskbar := True; {$ENDIF}
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
