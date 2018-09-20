unit FormMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, Buttons, ComCtrls, Spin,
  SAVIUnit;

const
 APP_TITLE = 'KMR AVI test app';
 APP_VER   = '2.0.0.0';
 WND_TITLE = APP_TITLE + ' v' + APP_VER;

type
  TFrmMain = class(TForm)
    Button1: TButton;
    Edit1: TEdit;
    Panel1: TPanel;
    Image1: TImage;
    RenderTimer: TTimer;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    Button8: TButton;
    Button9: TButton;
    Button10: TButton;
    Button11: TButton;
    Button12: TButton;
    PlayButton: TSpeedButton;
    PauseButton: TSpeedButton;
    StopButton: TSpeedButton;
    CheckBox1: TCheckBox;
    FramesLabel: TLabel;
    ProgressBar: TProgressBar;
    DoubleHeightCheck: TCheckBox;
    BlackLinesCheck: TCheckBox;
    Brightness: TSpinEdit;
    procedure FormCreate(Sender: TObject);
    procedure RenderTimerTimer(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure PlayButtonClick(Sender: TObject);
    procedure PauseButtonClick(Sender: TObject);
    procedure StopButtonClick(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure DoubleHeightCheckClick(Sender: TObject);
    procedure BlackLinesCheckClick(Sender: TObject);
    procedure BrightnessChange(Sender: TObject);
  private
    FExeDir: string;
    procedure SetCaption;
    procedure VideoIdle(aIndForce: Boolean = True);
    function AddTrailer(aValue: string): string;
  public
    AVIVideo: TAVI;
  end;

implementation
uses
  VFW;

{$R *.dfm}

procedure TFrmMain.FormCreate(Sender: TObject);
begin
  AVIFileInit;
  Application.Title := WND_TITLE;
  Caption           := ' ' + WND_TITLE;
  FExeDir           := ExtractFilePath(ParamStr(0));
  Edit1.Text        := FExeDir + 'data' + PathDelim;

  AVIVideo            := TAVI.Create;
  AVIVideo.Brightness := Brightness.Value;

  PauseButton.Left := ClientWidth div 2 - PauseButton.Width div 2;
  PlayButton.Left  := PauseButton.Left - 4 - PlayButton.Width;
  StopButton.Left  := PauseButton.Left + 4 + PauseButton.Width;

  RenderTimer.Enabled   := True;
  Panel1.DoubleBuffered := True;
end;

procedure TFrmMain.VideoIdle(aIndForce: Boolean = True);
begin
  if Assigned(AVIVideo) then
  begin
    if AVIVideo.Idle(aIndForce) then
    begin
      Image1.Picture.Assign(AVIVideo.BMP);
      FramesLabel.Caption  := IntToStr(AVIVideo.CurrentFrame) + '/' +
                              IntToStr(AVIVideo.FrameCount);
      ProgressBar.Position := AVIVideo.CurrentFrame;
    end;
  end;
end;

procedure TFrmMain.RenderTimerTimer(Sender: TObject);
begin
  VideoIdle(False);
end;

function TFrmMain.AddTrailer(aValue: string) : string;
begin
  Result := aValue;

  if (Length(Result) < 1) or (Result[Length(Result)] = PathDelim) then
    Exit;

  if CharInSet(Result[Length(Result)], ['/', '\']) then
    Result[Length(Result)] := PathDelim
  else
    Result := Result + PathDelim;
end;

procedure TFrmMain.SetCaption;
var
  NewCaption : string;
begin
  NewCaption := ' ' + WND_TITLE;

  if Assigned(AVIVideo) then
    if AVIVideo.AVIState <> aviNoFile then
      NewCaption := NewCaption + ' [' + ExtractFileName(AVIVideo.Filename) + ']';

  if NewCaption <> Caption then
    Caption := NewCaption;
end;

procedure TFrmMain.Button1Click(Sender: TObject);
begin
  AVIVideo.VidInit(
    AddTrailer(Edit1.text) + TButton(Sender).Caption,
    True,
    DoubleHeightCheck.Checked,
    nil
  );
  SetCaption;

  ProgressBar.Position := 0;
  ProgressBar.Max      := AVIVideo.FrameCount;

  if AVIVideo.AVIState = aviNoFile then
  begin
    ShowMessage('Error: Unable to find video');
    Exit;
  end;

  AVIVideo.Play;

  StopButton.Enabled  := True;
  PauseButton.Enabled := True;
  PlayButton.Enabled  := False;
end;

procedure TFrmMain.PlayButtonClick(Sender: TObject);
begin
  StopButton.Enabled  := True;
  PauseButton.Enabled := True;
  PlayButton.Enabled  := False;
  AVIVideo.Play;
end;

procedure TFrmMain.PauseButtonClick(Sender: TObject);
begin
  StopButton.Enabled  := True;
  PauseButton.Enabled := False;
  PlayButton.Enabled  := True;
  AVIVideo.Pause;
end;

procedure TFrmMain.StopButtonClick(Sender: TObject);
begin
  StopButton.Enabled  := False;
  PauseButton.Enabled := False;
  PlayButton.Enabled  := True;
  AVIVideo.Stop;

  Image1.Picture.Bitmap.Width := 0;
  FramesLabel.Caption         := IntToStr(AVIVideo.CurrentFrame) + '/' + IntToStr(AVIVideo.FrameCount);
  ProgressBar.Position        := 0;
end;

procedure TFrmMain.CheckBox1Click(Sender: TObject);
begin
  Image1.Stretch := CheckBox1.Checked;
end;

procedure TFrmMain.DoubleHeightCheckClick(Sender: TObject);
begin
  AVIVideo.DoubleHeight   := DoubleHeightCheck.Checked;
  BlackLinesCheck.Enabled := DoubleHeightCheck.Checked;
  VideoIdle;
end;

procedure TFrmMain.BlackLinesCheckClick(Sender: TObject);
begin
  AVIVideo.BlackLines := BlackLinesCheck.Checked;
  VideoIdle;
end;

procedure TFrmMain.BrightnessChange(Sender: TObject);
begin
  AVIVideo.Brightness := Brightness.Value;
  VideoIdle;
end;

end.
