program KaM_DedicatedServerGUI;
{$I ..\..\KaM_Remake.inc}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  {$IFDEF FPC}
  Interfaces, // this includes the LCL widgetset
  {$ENDIF}
  Forms,
  UnitMain in 'UnitMain.pas' {TFormMain};


{$IFDEF FPC}
  {$R *.res}
{$ENDIF}


begin
  Application.Initialize;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
