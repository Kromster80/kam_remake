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
  fSettingsLastModified: integer;

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
  Writeln('Settings file: '+ExeDir+SETTINGS_FILE);
  Writeln('');

  fSettings := TGlobalSettings.Create;
  fSettings.SaveSettings(true);
  fSettingsLastModified := FileAge(ExeDir+SETTINGS_FILE);

  fDedicatedServer := TKMDedicatedServer.Create(fSettings.MaxRooms,
                                                fSettings.AutoKickTimeout,
                                                fSettings.PingInterval,
                                                fSettings.MasterAnnounceInterval,
                                                fSettings.MasterServerAddress,
                                                fSettings.HTMLStatusFile,
                                                fSettings.ServerWelcomeMessage);
  fDedicatedServer.OnMessage := fEventHandler.ServerStatusMessage;
  fDedicatedServer.Start(fSettings.ServerName, fSettings.ServerPort, fSettings.AnnounceServer, true);

  while True do
  begin
    fDedicatedServer.UpdateState;
    //Reload the INI file if it has changed
    if (FileAge(ExeDir+SETTINGS_FILE) <> fSettingsLastModified) then
    begin
      fEventHandler.ServerStatusMessage('Reloading updated settings from '+ExeDir+SETTINGS_FILE);
      fSettings.ReloadSettings;
      fSettingsLastModified := FileAge(ExeDir+SETTINGS_FILE);
      fDedicatedServer.UpdateSettings(fSettings.ServerName,
                                      fSettings.AnnounceServer,
                                      fSettings.AutoKickTimeout,
                                      fSettings.PingInterval,
                                      fSettings.MasterAnnounceInterval,
                                      fSettings.MasterServerAddress,
                                      fSettings.HTMLStatusFile,
                                      fSettings.ServerWelcomeMessage);
    end;
    {$IFDEF MSWindows}
    MyProcessMessages; //This will process network (or other) events
    {$ENDIF}
    Sleep(1); //Don't hog CPU (this can also be used to create an artifical latency)
  end;

  fDedicatedServer.Stop;
  fDedicatedServer.Free;
  fEventHandler.Free;
end.
