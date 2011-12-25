program KaMNetworkTest;
{$I KaM_Remake.inc}
{$IFDEF FPC}
  {$Mode Delphi} {$H+}
{$ENDIF}

uses
  {$IFDEF FPC} Interfaces, {$ENDIF}
  Forms,
  NetTest in 'NetTest.pas'; {frmNetTest}

{$IFDEF WDC}
{$R *.res}
{$ENDIF}

begin
  Application.Initialize;
  Application.CreateForm(TfrmNetTest, frmNetTest);
  Application.Run;
end.
