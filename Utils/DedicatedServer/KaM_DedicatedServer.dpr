program KaM_DedicatedServer;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  Windows,
  KM_Defaults,
  KM_DedicatedServer,
  KM_EventHandler in 'KM_EventHandler.pas';

var
  fEventHandler: TKMEventHandler;
  fDedicatedServer: TKMDedicatedServer;

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


begin
  fEventHandler := TKMEventHandler.Create;
  Writeln('=========== KaM Remake '+GAME_VERSION+' Dedicated Server ===========');
  Writeln('');
  fDedicatedServer := TKMDedicatedServer.Create;
  fDedicatedServer.OnMessage := fEventHandler.ServerStatusMessage;
  fDedicatedServer.Start;

  while true do
  begin
    fDedicatedServer.UpdateState;
    MyProcessMessages; //This will process network (or other) events
    sleep(1); //Don't hog CPU
  end;

  fDedicatedServer.Stop;
  fDedicatedServer.Free;
  fEventHandler.Free;
end.
