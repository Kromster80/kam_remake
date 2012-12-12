program PathFinder;

uses
  Forms,
  Unit_Form in 'Unit_Form.pas' {Form1},
  KM_PathFinding in '..\..\KM_PathFinding.pas',
  KM_PathFindingAStarNew in '..\..\KM_PathFindingAStarNew.pas',
  KM_PathFindingAStarOld in '..\..\KM_PathFindingAStarOld.pas',
  KM_PathFindingJPS in '..\..\KM_PathFindingJPS.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
