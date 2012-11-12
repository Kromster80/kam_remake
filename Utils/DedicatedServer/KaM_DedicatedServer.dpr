program KaM_DedicatedServer;
{$I ..\..\KaM_Remake.inc}

{$IFDEF MSWindows}
  {$APPTYPE CONSOLE}
{$ENDIF}

uses
  {$IFDEF UNIX} BaseUnix, cthreads, {$ENDIF} //We use a thread for deleting old log files
  SysUtils,
  {$IFDEF MSWindows} Windows, MMSystem, {$ENDIF}
  KM_Utils,
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
  fLastSettingsFileCheck: Cardinal;

{$IFDEF WDC}
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
                                                fSettings.ServerWelcomeMessage,
                                                True);
  fDedicatedServer.OnMessage := fEventHandler.ServerStatusMessage;
  fDedicatedServer.Start(fSettings.ServerName, fSettings.ServerPort, fSettings.AnnounceServer);

  while True do
  begin
    fDedicatedServer.UpdateState;
    //Reload the INI file if it has changed, by checking the file age every 5 seconds
    if GetTimeSince(fLastSettingsFileCheck) >= 5000 then
    begin
      fLastSettingsFileCheck := TimeGet;
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
    //In Lazarus LNet will wait for 1ms in CallAction for the OS to respond with socket events
    //so we don't need to do ProcessMessage or Sleep here.
    {$IFDEF WDC}
    MyProcessMessages; //This will process network (or other) events
    Sleep(1); //Don't hog CPU (this can also be used to create an artifical latency)
    {$ENDIF}
  end;
end;

{$IFDEF FPC}
function GetExceptionCallStack:string;
var I: Integer; p: pointer;
begin
  Result := '*** STACKTRACE ***' + LineEnding;
  Result := Result + BackTraceStrFunc(ExceptAddr);
  for I := 0 to ExceptFrameCount - 1 do
  begin
    p:=(ExceptFrames+i)^;
    Result := Result + LineEnding + BackTraceStrFunc(p);
  end;
  Result := Result + LineEnding + '*** END STACKTRACE ***';
end;
{$ENDIF}

//Called when our process is asked to terminate (by either Windows or Linux)
procedure ServerKilled;
begin
  if fEventHandler <> nil then
    fEventHandler.ServerStatusMessage('Interrupt received, shutting down server...')
  else
    Writeln('Interrupt received, shutting down server...');

  if fDedicatedServer <> nil then
  begin
    fDedicatedServer.Stop;
    fDedicatedServer.Free;
  end;
  if fEventHandler <> nil then fEventHandler.Free;

  Halt; //Terminate the process
end;

{$IFDEF MSWindows}
function OnConsoleCtrl(ACtrl: DWord): LongBool; stdcall;
begin
  Result := False;
  if (ACtrl = CTRL_C_EVENT) or (ACtrl = CTRL_BREAK_EVENT) or (ACtrl = CTRL_CLOSE_EVENT) then
  begin
    Result := True;
    ServerKilled;
  end;
end;
{$ENDIF}

{$IFDEF UNIX}
procedure OnInterrupt(ASignal: cint); cdecl;
begin
  ServerKilled;
end;
{$ENDIF}

begin
  {$IFDEF UNIX}
  //Handle interupts (requests for our process to terminate)
  FpSignal(SIGTerm, @OnInterrupt);
  FpSignal(SIGINT, @OnInterrupt);
  {$ENDIF}
  {$IFDEF MSWindows}
  TimeBeginPeriod(1); //initialize timer precision
  SetConsoleCtrlHandler(@OnConsoleCtrl, True); //Handle requests for our process to terminate
  {$ENDIF}
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
  fLastSettingsFileCheck := TimeGet;

  while True do
  begin
    try //Catch and log exceptions
      RunTheServer;
    except
      on E : Exception do //todo: For some reason this doesn't catch exceptions that occur within network events e.g. OnReceive (once server is running errors can really only occur in these events)
      begin
        fEventHandler.ServerStatusMessage('EXCEPTION: '+E.ClassName+': '+E.Message);
        {$IFDEF FPC} fEventHandler.ServerStatusMessage(GetExceptionCallStack()); {$ENDIF}
        fEventHandler.ServerStatusMessage('Server restarting...');
        FreeAndNil(fDedicatedServer);
        fEventHandler.ServerStatusMessage('Sleeping for 5 seconds...');
        Sleep(5000); //Give the OS time to close the socket before restarting
      end;
    end;
  end;

  fDedicatedServer.Stop;
  fDedicatedServer.Free;
  fEventHandler.Free;
  {$IFDEF MSWindows}
  TimeEndPeriod(1);
  {$ENDIF}
end.
