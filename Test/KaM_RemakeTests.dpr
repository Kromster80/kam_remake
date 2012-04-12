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
  TestKM_CommonClasses in 'TestKM_CommonClasses.pas',
  TestKM_Campaigns in 'TestKM_Campaigns.pas',
  TestKM_Game in 'TestKM_Game.pas',
  TestKM_Points in 'TestKM_Points.pas';

{$R *.RES}

begin
  Application.Initialize;
  if IsConsole then
    with TextTestRunner.RunRegisteredTests do
      Free
  else
    GUITestRunner.RunRegisteredTests;
end.

