program KaMNetworkTest;
{$I KaM_Remake.inc}
{$IFDEF FPC}
  {$Mode Delphi} {$H+}
{$ENDIF}

uses
  {$IFDEF FPC} Interfaces, {$ENDIF}
  Forms, LResources, NetTest
  { you can add units after this };

{$IFDEF WINDOWS}{$R KaMNetworkTest.rc}{$ENDIF}

begin
  {$I KaMNetworkTest.lrs}
  Application.Initialize;
  Application.CreateForm(TfrmNetTest, frmNetTest);
  Application.Run;
end.

