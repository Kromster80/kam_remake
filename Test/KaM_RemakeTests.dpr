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
  KM_CommonClasses in '..\KM_CommonClasses.pas',
  TestKM_Campaigns in 'TestKM_Campaigns.pas',
  KM_Campaigns in '..\KM_Campaigns.pas',
  TestKM_Game in 'TestKM_Game.pas',
  KM_Game in '..\KM_Game.pas';

{$R *.RES}

begin
  Application.Initialize;
  if IsConsole then
    with TextTestRunner.RunRegisteredTests do
      Free
  else
    GUITestRunner.RunRegisteredTests;
end.

