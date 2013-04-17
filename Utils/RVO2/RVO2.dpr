program RVO2;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

uses
{$IFNDEF FPC}
{$ELSE}
  Interfaces,
{$ENDIF}
  Forms,
  Unit_Form in 'Unit_Form.pas' {Form1},
  RVO2_Agent in 'RVO2_Agent.pas',
  RVO2_KdTree in 'RVO2_KdTree.pas',
  RVO2_Line in 'RVO2_Line.pas',
  RVO2_Math in 'RVO2_Math.pas',
  RVO2_Obstacle in 'RVO2_Obstacle.pas',
  RVO2_Simulator in 'RVO2_Simulator.pas',
  RVO2_Vector2 in 'RVO2_Vector2.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
