program KaM_DedicatedServer;
{$I KaM_DedicatedServer.inc}
{$IFDEF FPC}
  {$Mode Delphi} {$H+}
{$ENDIF}

{$IFDEF MSWindows}
  {$APPTYPE CONSOLE}
{$ENDIF}

uses
  {$IFDEF UNIX} cthreads, {$ENDIF} //We use a thread for deleting old log files
  SysUtils,
  {$IFDEF MSWindows}Windows,{$ENDIF}
  {$IFDEF Unix} KM_Utils, {$ENDIF} //Needed in Linux for FakeGetTickCount
  KM_Defaults,
  KM_Log,
  KM_Settings,
  KM_DedicatedServer,
  KM_ServerEventHandler in 'KM_ServerEventHandler.pas';

var
  fEventHandler: TKMServerEventHandler;
  fDedicatedServer: TKMDedicatedServer;
  fSettings: TGameSettings;
  fSettingsLastModified: integer;
  TickCount, fLastSettingsFileCheck: cardinal;

{$IFDEF MSWindows}
procedure MyProcessMessages;
var Msg: TMsg;
begin
  while PeekMessage(Msg, 0, 0, 0, 0) do
  begin
    GetMessage(Msg, 0, 0, 0);
    TranslateMessage(Msg);
    DispatchMessage(Msg);
  end;
end;
{$ENDIF}

procedure RunTheServer;
begin
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
    TickCount := {$IFDEF MSWindows}GetTickCount{$ENDIF}
                 {$IFDEF Unix} FakeGetTickCount{$ENDIF};
    //Reload the INI file if it has changed, by checking the file age every 5 seconds
    if (abs(Int64(TickCount)-Int64(fLastSettingsFileCheck)) >= 5000) then
    begin
      fLastSettingsFileCheck := TickCount;
      if FileAge(ExeDir+SETTINGS_FILE) <> fSettingsLastModified then
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
    end;
    {$IFDEF MSWindows}
    MyProcessMessages; //This will process network (or other) events
    {$ENDIF}
    Sleep(1); //Don't hog CPU (this can also be used to create an artifical latency)
  end;
end;


begin
  fEventHandler := TKMServerEventHandler.Create;
  Writeln('=========== KaM Remake '+GAME_VERSION+' Dedicated Server ===========');
  Writeln('');
  Writeln('Log file: '+fLog.LogPath);
  Writeln('Settings file: '+ExeDir+SETTINGS_FILE);
  Writeln('');

  fEventHandler.ServerStatusMessage('Using protocol for clients running '+NET_PROTOCOL_REVISON);

  fSettings := TGameSettings.Create;
  fSettings.SaveSettings(true);
  fSettingsLastModified := FileAge(ExeDir+SETTINGS_FILE);
  fLastSettingsFileCheck := 0;

  while True do
  begin
    try //Catch and log exceptions
      RunTheServer;
    except
      on E : Exception do //todo: For some reason this doesn't catch exceptions that occur within network events e.g. OnReceive (once server is running errors can really only occur in these events)
      begin
        fEventHandler.ServerStatusMessage('EXCEPTION: '+E.ClassName+': '+E.Message);
        fEventHandler.ServerStatusMessage('Server restarting...');
        FreeAndNil(fDedicatedServer);
      end;
    end;
  end;

  fDedicatedServer.Stop;
  fDedicatedServer.Free;
  fEventHandler.Free;
end.
