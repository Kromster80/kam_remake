program KaMNetworkTest;
{$I KaM_Remake.inc}
{$IFDEF FPC}
  {$Mode Delphi} {$H+}
{$ENDIF}

uses
  Forms,
  NetTest in 'NetTest.pas',
  KM_Client in 'KM_Client.pas',
  KM_Server in 'KM_Server.pas';

{frmNetTest}

{$IFDEF WDC}
{$R *.res}
{$ENDIF}

begin
  Application.Initialize;
  Application.CreateForm(TfrmNetTest, frmNetTest);
  Application.Run;
end.
