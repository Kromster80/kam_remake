program KaM_RemakeTestFunc;
{$IFDEF CONSOLE_TESTRUNNER}
{$APPTYPE CONSOLE}
{$ENDIF}

uses
  Forms,
  TestFramework,
  GUITestRunner,
  TextTestRunner,
  TestKM_AIFields in 'TestKM_AIFields.pas',
  TestKM_Scripting in 'TestKM_Scripting.pas',
  TestKM_Terrain in 'TestKM_Terrain.pas',
  TestKM_MissionScript in 'TestKM_MissionScript.pas';

{$R *.RES}

begin
  Application.Initialize;
  if IsConsole then
    with TextTestRunner.RunRegisteredTests do
      Free
  else
    GUITestRunner.RunRegisteredTests;
end.

