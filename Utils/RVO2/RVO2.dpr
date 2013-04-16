program RVO2;

uses
  Forms,
  Unit_Form in 'Unit_Form.pas' {Form1},
  RVO2_Agent in 'RVO2_Agent.pas',
  KM_Terrain in 'KM_Terrain.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
