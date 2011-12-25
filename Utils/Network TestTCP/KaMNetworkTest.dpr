program KaMNetworkTest;
{$I KaM_Remake.inc}
{$IFDEF FPC}
  {$Mode Delphi} {$H+}
{$ENDIF}

uses
  Forms,
  NetTest in 'NetTest.pas',
  KM_NetClient in 'KM_NetClient.pas',
  KM_NetServer in 'KM_NetServer.pas';

{frmNetTest}

{$IFDEF WDC}
{$R *.res}
{$ENDIF}

begin
  Application.Initialize;
  Application.CreateForm(TfrmNetTest, frmNetTest);
  Application.Run;
end.
