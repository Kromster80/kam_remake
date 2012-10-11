program KaM_RemakeTestFunc;
{$IFDEF CONSOLE_TESTRUNNER}
{$APPTYPE CONSOLE}
{$ENDIF}

//{$DEFINE DUNIT_TEST} Defined in ProjectOptions

uses
  //FastMM4,
  Forms,
  TestFramework,
  GUITestRunner,
  TextTestRunner,
  TestKM_AIFields in 'TestKM_AIFields.pas',
  TestKM_Terrain in 'TestKM_Terrain.pas';

{$R *.RES}

begin
  Application.Initialize;
  if IsConsole then
    with TextTestRunner.RunRegisteredTests do
      Free
  else
    GUITestRunner.RunRegisteredTests;
end.

