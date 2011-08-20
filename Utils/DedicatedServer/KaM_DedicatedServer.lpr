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
  KM_Settings,
  KM_DedicatedServer,
  KM_ServerEventHandler in 'KM_ServerEventHandler.pas';

var
  fEventHandler: TKMServerEventHandler;
  fDedicatedServer: TKMDedicatedServer;
  fSettings: TGlobalSettings;

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

  fSettings := TGlobalSettings.Create;
  fSettings.SaveSettings(true);

  fDedicatedServer := TKMDedicatedServer.Create(fSettings.MaxRooms,
                                                fSettings.AutoKickTimeout,
                                                fSettings.PingInterval,
                                                fSettings.MasterAnnounceInterval,
                                                fSettings.MasterServerAddress);
  fDedicatedServer.OnMessage := fEventHandler.ServerStatusMessage;
  fDedicatedServer.Start(fSettings.TCPPort, fSettings.AnnounceServer, true);

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
