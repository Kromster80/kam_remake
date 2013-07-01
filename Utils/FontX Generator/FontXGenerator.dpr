program FontXGenerator;
{$I ..\..\KaM_Remake.inc}
uses
  {$IFDEF FPC} Interfaces, {$ENDIF}
  Forms,
  Unit_Main in 'Unit_Main.pas' {Form1},
  KM_ResourceFonts in '..\..\KM_ResourceFonts.pas',
  KM_ResourceFontsEdit in '..\..\KM_ResourceFontsEdit.pas';

begin
  Application.Initialize;
  {$IFDEF FPC} Application.MainFormOnTaskbar := True; {$ENDIF}
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
