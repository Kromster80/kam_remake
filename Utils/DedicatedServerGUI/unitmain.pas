unit UnitMain;
{$I ..\..\KaM_Remake.inc}
interface
uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Spin, ComCtrls,
  KM_Defaults,
  KM_Settings,
  KM_DedicatedServer,
  KM_Log;


type
  //Distinct states for the server
  TKMServerStatus = (ssOffline, ssOnline);

  TFormMain = class(TForm)
    ButtonApply: TButton;
    cAnnounceServer: TCheckBox;
    cAutoKickTimeout: TSpinEdit;
    cHTMLStatusFile: TEdit;
    cMasterAnnounceInterval: TSpinEdit;
    cMasterServerAddress: TEdit;
    cMaxRooms: TSpinEdit;
    cPingInterval: TSpinEdit;
    cServerName: TEdit;
    cServerPort: TEdit;
    cServerWelcomeMessage: TEdit;
    Label10: TLabel;
    Label11: TLabel;
    Label2: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    PageControl1: TPageControl;
    SendCmdButton: TButton;
    Edit1: TEdit;
    Label1: TLabel;
    ListBox1: TListBox;
    LogsMemo: TMemo;
    Panel1: TPanel;
    Panel2: TPanel;
    Splitter1: TSplitter;
    StartStopButton: TButton;
    Basic: TTabSheet;
    Advanced: TTabSheet;

    //saveing setting to file and update (it will do it only if server is online)
    procedure ButtonApplyClick(Sender: TObject);

    //handles controls OnChange events
    procedure ControlChange(Sender: TObject);

    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure StartStopButtonClick(Sender: TObject);
    procedure ChangeServerStatus(aStatus: TKMServerStatus);
    procedure LoadSettings;

    //those procs run KM_Log.AppendLog() and add same log line to Memo control
    procedure ServerStatusMessage(const aData: string);
    procedure ServerStatusMessageNoTime(const aData: string);

    //this proc can change state (enable/disable) of controls that CAN'T be modyfied when server is online
    procedure ChangeEnableStateOfControls(state: Boolean);

    //whenever there is a change in the settings controls while server is online, we call this proc to enable "ButtonApply" button
    procedure ChangeEnableStateOfApplyButton(state: Boolean);
    procedure ApplicationIdle(Sender: TObject; var Done: Boolean);
  private
    fSettings: TGameSettings;
    fSettingsLastModified: integer;
    fServerStatus: TKMServerStatus;
    fDedicatedServer: TKMDedicatedServer;
  end;


var
  FormMain: TFormMain;


implementation
{$IFDEF WDC}
  {$R *.dfm}
{$ENDIF}

{$IFDEF FPC}
  {$R *.lfm}
{$ENDIF}


{ TFormMain }
procedure TFormMain.FormCreate(Sender: TObject);
begin
  fServerStatus := ssOffline;
  ChangeEnableStateOfApplyButton(False);
  Application.Title := 'KaM Remake ' + GAME_VERSION + ' Dedicated Server';

  ExeDir := ExtractFilePath(ParamStr(0));
  CreateDir(ExeDir + 'Logs');
  fLog := TKMLog.Create(ExeDir + 'Logs' + PathDelim + 'KaM_Server_' + FormatDateTime('yyyy-mm-d_hh-nn-ss-zzz', Now) + '.log');

  //this is shown only at application start (tip. check the strange -. in morse code translator ;)
  ServerStatusMessageNoTime('-.- .- -- / .-. . -- .- -.- . / .. ... / - .... . / -... . ... -');
  ServerStatusMessage      ('== KaM Remake ' + GAME_VERSION + ' Dedicated Server ==');
  ServerStatusMessageNoTime('');
  ServerStatusMessage      ('Settings file: ' + ExeDir + SETTINGS_FILE);
  ServerStatusMessage      ('Log file: ' + fLog.LogPath);
  ServerStatusMessageNoTime('-.- .- -- / .-. . -- .- -.- . / .. ... / - .... . / -... . ... -');
  ServerStatusMessageNoTime('');

  fSettings := TGameSettings.Create;
  fSettings.SaveSettings(true);
  fSettingsLastModified := FileAge(ExeDir+SETTINGS_FILE);

  //we load settings from file to controls
  LoadSettings;

  Application.OnIdle := ApplicationIdle;
end;


procedure TFormMain.FormDestroy(Sender: TObject);
begin
  //Terminate online server on exit
  if fServerStatus = ssOnline then
    ChangeServerStatus(ssOffline);

  FreeAndNil(fLog);
  fSettings.Free;
end;


procedure TFormMain.ServerStatusMessage(const aData: string);
begin
  LogsMemo.Lines.Add(FormatDateTime('yyyy-mm-dd hh-nn-ss ', Now) + aData);
  fLog.AppendLog(aData);
end;


procedure TFormMain.ServerStatusMessageNoTime(const aData: string);
begin
  LogsMemo.Lines.Add(aData);
  fLog.AppendLog(aData);
end;


procedure TFormMain.StartStopButtonClick(Sender: TObject);
begin
  ButtonApply.Enabled := True;

  //turn off server when it was on and vice-versa
  case fServerStatus of
    ssOffline: FormMain.ChangeServerStatus(ssOnline);
    ssOnline:  FormMain.ChangeServerStatus(ssOffline);
  end;
end;


procedure TFormMain.ChangeEnableStateOfControls(state: Boolean);
begin
  cMaxRooms.Enabled   := state;
  cServerPort.Enabled := state;
end;


procedure TFormMain.ChangeServerStatus(aStatus: TKMServerStatus);
begin
  case aStatus of
    ssOnline:
      begin
        ChangeEnableStateOfControls(False);

        fDedicatedServer := TKMDedicatedServer.Create(fSettings.MaxRooms,
                                                      fSettings.AutoKickTimeout,
                                                      fSettings.PingInterval,
                                                      fSettings.MasterAnnounceInterval,
                                                      fSettings.MasterServerAddress,
                                                      fSettings.HTMLStatusFile,
                                                      fSettings.ServerWelcomeMessage);
        fDedicatedServer.OnMessage := ServerStatusMessage;
        fDedicatedServer.Start(fSettings.ServerName, fSettings.ServerPort, fSettings.AnnounceServer, true);

        fServerStatus := aStatus;
        StartStopButton.Caption := 'Server is ONLINE';

        ChangeEnableStateOfApplyButton(False);
      end;
    ssOffline:
      begin
        //Reenable disabled controls
        ChangeEnableStateOfControls(True);

        FreeAndNil(fDedicatedServer);

        fServerStatus := aStatus;
        StartStopButton.Caption := 'Server is OFFLINE';
        ServerStatusMessage('Dedicated Server is now Offline');
        ServerStatusMessageNoTime('');
      end;
  end;
end;


procedure TFormMain.ChangeEnableStateOfApplyButton(state: Boolean);
begin
  ButtonApply.Enabled := state;
end;


procedure TFormMain.ApplicationIdle(Sender: TObject; var Done: Boolean);
begin
  if fServerStatus = ssOnline then
  begin
    fDedicatedServer.UpdateState;
    Sleep(1); //Don't use 100% CPU
    Done := False; //Repeats OnIdle asap without performing Form-specific idle code
  end
  else
    Done := True;
end;


//one event for each control
procedure TFormMain.ControlChange(Sender: TObject);
begin
  ChangeEnableStateOfApplyButton(True);
end;


procedure TFormMain.ButtonApplyClick(Sender: TObject);
begin
  //Disable the button asap to indicate we are at it
  ChangeEnableStateOfApplyButton(False);

  fSettings.ServerName              := cServerName.Text;
  fSettings.ServerWelcomeMessage    := cServerWelcomeMessage.Text;
  fSettings.AnnounceServer          := cAnnounceServer.Checked;
  fSettings.AutoKickTimeout         := cAutoKickTimeout.Value;
  fSettings.PingInterval            := cPingInterval.Value;
  fSettings.MasterAnnounceInterval  := cMasterAnnounceInterval.Value;
  fSettings.MasterServerAddress     := cMasterServerAddress.Text;
  fSettings.HTMLStatusFile          := cHTMLStatusFile.Text;
  fSettings.ServerPort              := cServerPort.Text;
  fSettings.MaxRooms                := cMaxRooms.Value;

  fSettings.SaveSettings(True);

  //We can update only if server is online
  if fServerStatus = ssOnline then
  begin
    fDedicatedServer.UpdateSettings(cServerName.Text,
                                    cAnnounceServer.Checked,
                                    cAutoKickTimeout.Value,
                                    cPingInterval.Value,
                                    cMasterAnnounceInterval.Value,
                                    cMasterServerAddress.Text,
                                    cHTMLStatusFile.Text,
                                    cServerWelcomeMessage.Text);
    ServerStatusMessage('Settings saved, updated and are now live.');
  end;
end;


procedure TFormMain.LoadSettings;
begin
  fSettings.ReloadSettings;

  cServerName.Text              := fSettings.ServerName;
  cServerWelcomeMessage.Text    := fSettings.ServerWelcomeMessage;
  cAnnounceServer.Checked       := fSettings.AnnounceServer;
  cAutoKickTimeout.Value        := fSettings.AutoKickTimeout;
  cPingInterval.Value           := fSettings.PingInterval;
  cMasterAnnounceInterval.Value := fSettings.MasterAnnounceInterval;
  cMasterServerAddress.Text     := fSettings.MasterServerAddress;
  cHTMLStatusFile.Text          := fSettings.HTMLStatusFile;
  cServerPort.Text              := fSettings.ServerPort;
  cMaxRooms.Value               := fSettings.MaxRooms;

  ServerStatusMessageNoTime('');
  ChangeEnableStateOfApplyButton(False);
end;


end.
