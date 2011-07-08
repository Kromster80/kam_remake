program KaM_DedicatedServer;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  Windows,
  KM_NetServer,
  KM_Defaults,
  KM_CommonTypes,
  KM_DedicatedServer,
  KM_EventHandler in 'KM_EventHandler.pas';

var
  fEventHandler: TKMEventHandler;
  fDedicatedServer: TKMDedicatedServer;
  c: char;

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
  fDedicatedServer := TKMDedicatedServer.Create;
  fDedicatedServer.OnMessage := fEventHandler.ServerStatusMessage;
  fDedicatedServer.Start;

  while true do
  begin
    fDedicatedServer.UpdateState;
    MyProcessMessages; //This will process network (or other) events
  end;

  fDedicatedServer.Stop;
  fDedicatedServer.Free;
  fEventHandler.Free;
end.
