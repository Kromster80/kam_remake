unit UnitMain;
{$I ..\..\KaM_Remake.inc}
interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Spin, ComCtrls,
  KM_Defaults,
  KM_Settings,
  KM_DedicatedServer,
  KM_Log;

type

  { TFormMain }

  TFormMain = class(TForm)
    ButtonApply: TButton;
    ButtonSaveSettings: TButton;
    Button2: TButton;
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
    procedure ButtonApplyClick(Sender: TObject);
    procedure ButtonSaveSettingsClick(Sender: TObject);
    procedure cAnnounceServerChange(Sender: TObject);
    procedure cAutoKickTimeoutChange(Sender: TObject);
    procedure cHTMLStatusFileChange(Sender: TObject);
    procedure cMasterAnnounceIntervalChange(Sender: TObject);
    procedure cMasterServerAddressChange(Sender: TObject);
    procedure cMaxRoomsChange(Sender: TObject);
    procedure cPingIntervalChange(Sender: TObject);
    procedure cServerNameChange(Sender: TObject);
    procedure cServerPortChange(Sender: TObject);
    procedure cServerWelcomeMessageChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure StartStopButtonClick(Sender: TObject);
    procedure ChangeServerStatus(Status: Boolean);
    procedure LoadSettings(Sender: TObject);
    procedure ServerStatusMessage(const aData: string);
    procedure ServerStatusMessageNoTime(const aData: string);
    procedure ChangeEnableStateOfControls(state: Boolean);
    procedure ChangeEnableStateOfApplyButton(state: Boolean);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  FormMain: TFormMain;
  fSettings: TGameSettings;
  fSettingsLastModified: integer;
  ServerStatus: Boolean;
  fDedicatedServer: TKMDedicatedServer;

implementation

{$IFDEF FPC}
  {$R *.lfm}
{$ENDIF}

{ TFormMain }

procedure TFormMain.FormCreate(Sender: TObject);
begin
  ServerStatus:=False;
  ChangeEnableStateOfApplyButton(false);

  ExeDir := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0)));
  CreateDir(ExeDir + 'Logs');
  fLog := TKMLog.Create(ExeDir+'Logs'+PathDelim+'KaM_Server_'+FormatDateTime('yyyy-mm-d_hh-nn-ss-zzz',Now)+'.log');

  fSettings := TGameSettings.Create;
  fSettings.SaveSettings(true);
  fSettingsLastModified := FileAge(ExeDir+SETTINGS_FILE);

  LoadSettings(nil);
end;


procedure TFormMain.FormDestroy(Sender: TObject);
begin
  if (ServerStatus = True) then
     ChangeServerStatus(False);
  FreeAndNil(fLog);
  fSettings.Free;
end;


procedure TFormMain.ServerStatusMessage(const aData: string);
begin
  LogsMemo.Lines.Add(FormatDateTime('yyyy-mm-dd hh-nn-ss ',Now)+aData);
  fLog.AppendLog(aData);
end;

procedure TFormMain.ServerStatusMessageNoTime(const aData: string);
begin
  LogsMemo.Lines.Add(aData);
  fLog.AppendLog(aData);
end;

procedure TFormMain.StartStopButtonClick(Sender: TObject);
begin
  ButtonApply.Enabled:=True;
  if (ServerStatus = true) then
     FormMain.ChangeServerStatus(false)
  else
     FormMain.ChangeServerStatus(true);
end;


procedure TFormMain.ChangeEnableStateOfControls(state: Boolean);
begin
  cMaxRooms.Enabled                          := state;
  cAnnounceServer.Enabled                    := state;
  cMasterServerAddress.Enabled               := state;
  cServerPort.Enabled                        := state;
  cMasterAnnounceInterval.Enabled            := state;
  cPingInterval.Enabled                      := state;
end;

procedure TFormMain.ChangeServerStatus(Status: Boolean);
begin
  if (Status = true) then
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

    ServerStatus:=Status;
    StartStopButton.Caption:='Server is ONLINE';

    ServerStatusMessageNoTime     ('-.- .- -- / .-. . -- .- -.- . / .. ... / - .... . / -... . ... -');
    ServerStatusMessage           ('== KaM Remake '+GAME_VERSION+' Dedicated Server - Online ==');
    ServerStatusMessageNoTime     ('');
    ServerStatusMessage           ('Settings file: '+ExeDir+SETTINGS_FILE);
    ServerStatusMessage           ('Log file: '+fLog.LogPath);
    ServerStatusMessageNoTime     ('-.- .- -- / .-. . -- .- -.- . / .. ... / - .... . / -... . ... -');
    ServerStatusMessageNoTime     ('');
    ChangeEnableStateOfApplyButton(False);
  end
  else
  begin
    ChangeEnableStateOfControls(True);
    ChangeEnableStateOfApplyButton(False);

    FreeAndNil(fDedicatedServer);

    ServerStatus:=Status;
    StartStopButton.Caption:='Server is OFFLINE';
    ServerStatusMessage('Dedicated Server is now Offline');
    ServerStatusMessageNoTime('');
  end;
end;





procedure TFormMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
 if (CloseAction = caFree) then
  if (ServerStatus = True) then
    ChangeServerStatus(false);
  FreeAndNil(fLog);
end;

procedure TFormMain.ButtonSaveSettingsClick(Sender: TObject);
begin
    fSettings.ServerName                            := cServerName.Text;
    fSettings.ServerWelcomeMessage                  := cServerWelcomeMessage.Text;
    if (cAnnounceServer.Checked = True) then
       fSettings.AnnounceServer                     := True
    else
        fSettings.AnnounceServer                    := False;
    fSettings.AutoKickTimeout                       := cAutoKickTimeout.Value;
    fSettings.PingInterval                          := cPingInterval.Value;
    fSettings.MasterAnnounceInterval                := cMasterAnnounceInterval.Value;
    fSettings.MasterServerAddress                   := cMasterServerAddress.Text;
    fSettings.HTMLStatusFile                        := cHTMLStatusFile.Text;
    fSettings.ServerPort                            := cServerPort.Text;
    fSettings.MaxRooms                              := cMaxRooms.Value;

    fSettings.SaveSettings(true);

    ServerStatusMessage('Setting saved to: '+ExeDir+SETTINGS_FILE);
    ServerStatusMessageNoTime('');
end;

procedure TFormMain.ChangeEnableStateOfApplyButton(state: Boolean);
begin
  if (ServerStatus = True) then
     ButtonApply.Enabled:=state;
end;


procedure TFormMain.cAnnounceServerChange(Sender: TObject);
begin
  ChangeEnableStateOfApplyButton(True);
end;

procedure TFormMain.cAutoKickTimeoutChange(Sender: TObject);
begin
  ChangeEnableStateOfApplyButton(True);
end;

procedure TFormMain.cHTMLStatusFileChange(Sender: TObject);
begin
  ChangeEnableStateOfApplyButton(True);
end;

procedure TFormMain.cMasterAnnounceIntervalChange(Sender: TObject);
begin
  ChangeEnableStateOfApplyButton(True);
end;

procedure TFormMain.cMasterServerAddressChange(Sender: TObject);
begin
  ChangeEnableStateOfApplyButton(True);
end;

procedure TFormMain.cMaxRoomsChange(Sender: TObject);
begin
  ChangeEnableStateOfApplyButton(True);
end;

procedure TFormMain.cPingIntervalChange(Sender: TObject);
begin
  ChangeEnableStateOfApplyButton(True);
end;

procedure TFormMain.cServerNameChange(Sender: TObject);
begin
  ChangeEnableStateOfApplyButton(True);
end;

procedure TFormMain.cServerPortChange(Sender: TObject);
begin
  ChangeEnableStateOfApplyButton(True);
end;

procedure TFormMain.cServerWelcomeMessageChange(Sender: TObject);
begin
  ChangeEnableStateOfApplyButton(True);
end;

procedure TFormMain.ButtonApplyClick(Sender: TObject);
begin
  fSettings.SaveSettings(true);
  fDedicatedServer.UpdateSettings(cServerName.Text,
                                  cAnnounceServer.Checked,
                                  cAutoKickTimeout.Value,
                                  cPingInterval.Value,
                                  cMasterAnnounceInterval.Value,
                                  cMasterServerAddress.Text,
                                  cHTMLStatusFile.Text,
                                  cServerWelcomeMessage.Text);
  ServerStatusMessage('Settings updated and are now live.');
  ChangeEnableStateOfApplyButton(False);
end;

procedure TFormMain.LoadSettings(Sender: TObject);
begin
  fSettings.ReloadSettings;

  cServerName.Text                                := fSettings.ServerName;
  cServerWelcomeMessage.Text                      := fSettings.ServerWelcomeMessage;
  if (fSettings.AnnounceServer = True) then
     cAnnounceServer.Checked                      := True
  else
      cAnnounceServer.Checked                     := False;
  cAutoKickTimeout.Value                          := fSettings.AutoKickTimeout;
  cPingInterval.Value                             := fSettings.PingInterval;
  cMasterAnnounceInterval.Value                   := fSettings.MasterAnnounceInterval;
  cMasterServerAddress.Text                       := fSettings.MasterServerAddress;
  cHTMLStatusFile.Text                            := fSettings.HTMLStatusFile;
  cServerPort.Text                                := fSettings.ServerPort;
  cMaxRooms.Value                                 := fSettings.MaxRooms;

  ServerStatusMessageNoTime('');
end;


end.

