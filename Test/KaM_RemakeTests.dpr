program KaM_RemakeTests;
{$IFDEF CONSOLE_TESTRUNNER}
{$APPTYPE CONSOLE}
{$ENDIF}

//{$DEFINE DUNIT_TEST} Defined in ProjectOptions

uses
  Forms,
  TestFramework,
  GUITestRunner,
  TextTestRunner,
  TestKM_Campaigns in 'TestKM_Campaigns.pas',
  TestKM_CommonClasses in 'TestKM_CommonClasses.pas',
  TestKM_FogOfWar in 'TestKM_FogOfWar.pas',
  TestKM_Points in 'TestKM_Points.pas',
  TestKM_Utils in 'TestKM_Utils.pas',
  TestKM_UnitActionGoInOut in 'TestKM_UnitActionGoInOut.pas'{,
  TestKM_MessageStack in 'TestKM_MessageStack.pas'};

{$R *.RES}

begin
  Application.Initialize;
  if IsConsole then
    with TextTestRunner.RunRegisteredTests do
      Free
  else
    GUITestRunner.RunRegisteredTests;
end.

