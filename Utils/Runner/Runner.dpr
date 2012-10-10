program Runner;
{$I KaM_Remake.inc}
uses
  Forms,
  Unit1 in 'Unit1.pas' {Form2},
  Unit_Runner in 'Unit_Runner.pas',
  Runner_Game in 'Runner_Game.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
