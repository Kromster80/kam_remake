unit UnitMain;
{$I ..\..\KaM_Remake.inc}
interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Spin,
  KM_Defaults,
  KM_Settings,
  KM_DedicatedServer,
  KM_Log;

type

  { TFormMain }

  TFormMain = class(TForm)
    ButtonSaveSettings: TButton;
    Button2: TButton;
    cAnnounceServer: TComboBox;
    cMaxRooms: TSpinEdit;
    cServerPort: TEdit;
    Label10: TLabel;
    Label11: TLabel;
    SendCmdButton: TButton;
    Edit1: TEdit;
    cServerName: TEdit;
    cMasterServerAddress: TEdit;
    cHTMLStatusFile: TEdit;
    cServerWelcomeMessage: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    ListBox1: TListBox;
    LogsMemo: TMemo;
    Panel1: TPanel;
    Panel2: TPanel;
    cAutoKickTimeout: TSpinEdit;
    cPingInterval: TSpinEdit;
    cMasterAnnounceInterval: TSpinEdit;
    Splitter1: TSplitter;
    StartStopButton: TButton;
    Timer: TTimer;
    procedure ButtonSaveSettingsClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure StartStopButtonClick(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    procedure ChangeServerStatus(Status: Boolean);
    procedure LoadSettings(Sender: TObject);
    procedure ServerStatusMessage(const aData: string);
    procedure ServerStatusMessageNoTime(const aData: string);
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
  ServerStatus:=false;

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
  FormMain.ChangeServerStatus(false);
  FreeAndNil(fLog);
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
  if (ServerStatus = true) then
     FormMain.ChangeServerStatus(false)
  else
     FormMain.ChangeServerStatus(true);
end;


procedure TFormMain.ChangeServerStatus(Status: Boolean);
begin
  if (Status = true) then
  begin
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

    ServerStatusMessage('== KaM Remake '+GAME_VERSION+' Dedicated Server - Online ==');
    ServerStatusMessageNoTime('');
    ServerStatusMessage('Settings file: '+ExeDir+SETTINGS_FILE);
    ServerStatusMessage('Log file: '+fLog.LogPath);
    ServerStatusMessageNoTime('');
  end
  else
  begin
    fDedicatedServer.Stop;
    fDedicatedServer.Free;

    ServerStatus:=Status;
    StartStopButton.Caption:='Server is OFFLINE';
    ServerStatusMessage('Dedicated Server is now Offline');
  end;
end;





procedure TFormMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if (CloseAction = caFree) then
    Timer.Enabled:=False;

  FormMain.ChangeServerStatus(false);
  FreeAndNil(fLog);
end;

procedure TFormMain.ButtonSaveSettingsClick(Sender: TObject);
begin
    fSettings.ServerName                            := cServerName.Text;
    fSettings.ServerWelcomeMessage                  := cServerWelcomeMessage.Text;
    if (cAnnounceServer.Text = 'True') then
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
end;

procedure TFormMain.LoadSettings(Sender: TObject);
begin
  fSettings.ReloadSettings;

  cServerName.Text                                := fSettings.ServerName;
  cServerWelcomeMessage.Text                      := fSettings.ServerWelcomeMessage;
  if (fSettings.AnnounceServer = True) then
     cAnnounceServer.Text                         := 'True'
  else
      cAnnounceServer.Text                        := 'False';
  cAutoKickTimeout.Value                          := fSettings.AutoKickTimeout;
  cPingInterval.Value                             := fSettings.PingInterval;
  cMasterAnnounceInterval.Value                   := fSettings.MasterAnnounceInterval;
  cMasterServerAddress.Text                       := fSettings.MasterServerAddress;
  cHTMLStatusFile.Text                            := fSettings.HTMLStatusFile;
  cServerPort.Text                                := fSettings.ServerPort;
  cMaxRooms.Value                                 := fSettings.MaxRooms;
end;

procedure TFormMain.TimerTimer(Sender: TObject);
begin
  LogsMemo.Lines.Add('test');
end;

end.

