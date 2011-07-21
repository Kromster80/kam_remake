program KaM_DedicatedServer;
{$I KaM_DedicatedServer.inc}
{$IFDEF FPC}
  {$Mode Delphi} {$H+}
{$ENDIF}

{$APPTYPE CONSOLE}

uses
  {$IFDEF FPC} Interfaces, {$ENDIF}
  SysUtils,
  Windows,
  KM_Defaults,
  KM_CommonTypes,
  KM_DedicatedServer,
  KM_ServerEventHandler in 'KM_ServerEventHandler.pas';

var
  fEventHandler: TKMServerEventHandler;
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
  fEventHandler := TKMServerEventHandler.Create;
  Writeln('=========== KaM Remake '+GAME_VERSION+' Dedicated Server ===========');
  Writeln('');
  Writeln('Log file: '+fLog.LogPath);
  Writeln('');

  fDedicatedServer := TKMDedicatedServer.Create;
  fDedicatedServer.OnMessage := fEventHandler.ServerStatusMessage;
  fDedicatedServer.Start;

  while True do
  begin
    fDedicatedServer.UpdateState;
    MyProcessMessages; //This will process network (or other) events
    Sleep(1); //Don't hog CPU (this can also be used to create an artifical latency)
  end;

  fDedicatedServer.Stop;
  fDedicatedServer.Free;
  fEventHandler.Free;
end.
