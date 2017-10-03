program Runner;
{$I KaM_Remake.inc}
uses
  Forms,
  {$IFDEF FPC} Interfaces, {$ENDIF}
  Unit1 in 'Unit1.pas' {Form2},
  Unit_Runner in 'Unit_Runner.pas',
  Runner_Game in 'Runner_Game.pas',
  KM_Defaults in '..\..\src\common\KM_Defaults.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
