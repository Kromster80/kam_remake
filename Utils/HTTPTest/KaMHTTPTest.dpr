program KaMHTTPTest;
{$I KaM_Remake.inc}
{$IFDEF FPC}
  {$Mode Delphi} {$H+}
{$ENDIF}

uses
  {$IFDEF FPC} Interfaces, {$ENDIF}
  Forms,
  HTTPTest in 'HTTPTest.pas' {Form1};

{$IFDEF WDC}
{$R *.res}
{$ENDIF}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
