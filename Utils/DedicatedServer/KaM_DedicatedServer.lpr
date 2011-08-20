program KaM_DedicatedServer;
{$I KaM_DedicatedServer.inc}
{$IFDEF FPC}
  {$Mode Delphi} {$H+}
{$ENDIF}

{$IFDEF MSWindows}
  {$APPTYPE CONSOLE}
{$ENDIF}

uses
  SysUtils,
  {$IFDEF MSWindows}Windows,{$ENDIF}
  KM_Defaults,
  KM_Log,
  KM_DedicatedServer,
  KM_ServerEventHandler in 'KM_ServerEventHandler.pas';

var
  fEventHandler: TKMServerEventHandler;
  fDedicatedServer: TKMDedicatedServer;

{$IFDEF MSWindows}
procedure MyProcessMessages;
var Msg : TMsg;
begin
  while PeekMessage(Msg,0,0,0,0) do
  begin
    GetMessage(Msg,0,0,0);
    TranslateMessage(Msg);
    DispatchMessage(Msg);
  end;
end;
{$ENDIF}


begin
  fEventHandler := TKMServerEventHandler.Create;
  Writeln('=========== KaM Remake '+GAME_VERSION+' Dedicated Server ===========');
  Writeln('');
  Writeln('Log file: '+fLog.LogPath);
  Writeln('');

  //todo: Load these options from the config file
  fDedicatedServer := TKMDedicatedServer.Create(2,20,1000,60,'http://lewin.hodgman.id.au/kam_remake_master_server/');
  fDedicatedServer.OnMessage := fEventHandler.ServerStatusMessage;
  fDedicatedServer.Start(KAM_PORT,true);

  while True do
  begin
    fDedicatedServer.UpdateState;
    {$IFDEF MSWindows}
    MyProcessMessages; //This will process network (or other) events
    {$ENDIF}
    Sleep(1); //Don't hog CPU (this can also be used to create an artifical latency)
  end;

  fDedicatedServer.Stop;
  fDedicatedServer.Free;
  fEventHandler.Free;
end.
