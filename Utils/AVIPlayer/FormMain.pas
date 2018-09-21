unit FormMain;
{
  Copyright (c) <2018> <Stuart "Stucuk" Carey>

  This software is provided 'as-is', without any express or implied
  warranty. In no event will the authors be held liable for any damages
  arising from the use of this software.

  Permission is granted to anyone to use this software for any purpose,
  including commercial applications, and to alter it and redistribute it
  freely, subject to the following restrictions:

  1. The origin of this software must not be misrepresented; you must not
  claim that you wrote the original software. If you use this software
  in a product, an acknowledgment in the product documentation would be
  appreciated but is not required.
  2. Altered source versions must be plainly marked as such, and must not be
  misrepresented as being the original software.
  3. This notice may not be removed or altered from any source distribution.

  // Altered by Krom for KaM Remake
}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, SAVIUnit, ExtCtrls, Buttons, ComCtrls, Spin;

type
  TFrmMain = class(TForm)
    Button1: TButton;
    edPath: TEdit;
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
    fAVIVideo: TAVI;
    procedure RefreshFrame(aForce: Boolean);
  end;

var
  FrmMain: TFrmMain;

implementation

{$R *.dfm}

procedure TFrmMain.FormCreate(Sender: TObject);
begin
  fAVIVideo := TAVI.Create;
  fAVIVideo.Brightness := Brightness.Value;

  RenderTimer.Enabled := True;
  Panel1.DoubleBuffered := True;

  Panel1.ControlStyle := [csOpaque]; // Fixes flickering
end;


procedure TFrmMain.RefreshFrame(aForce: Boolean);
begin
  if fAVIVideo.RefreshFrame(aForce) then
  begin
    Image1.Picture.Assign(fAVIVideo.BMP);
    FramesLabel.Caption := Format('%d/%d', [fAVIVideo.CurrentFrame, fAVIVideo.FrameCount]);
    ProgressBar.Position := fAVIVideo.CurrentFrame;
  end;
end;


procedure TFrmMain.RenderTimerTimer(Sender: TObject);
begin
  RefreshFrame(False);
end;


procedure TFrmMain.Button1Click(Sender: TObject);
var
  path: string;
begin
  path := ExpandFileName(edPath.Text + TButton(Sender).Caption);

  fAVIVideo.VidInit(path, True, DoubleHeightCheck.Checked, nil);
  Caption := 'AVIPlayer - ' + ExtractFileName(path);

  ProgressBar.Position := 0;
  ProgressBar.Max := fAVIVideo.FrameCount;

  if fAVIVideo.AVIState = aviNoFile then
  begin
    ShowMessage('Error: Unable to find video');
    Exit;
  end;

  fAVIVideo.Play;

  StopButton.Enabled := True;
  PauseButton.Enabled := True;
  PlayButton.Enabled := False;
end;


procedure TFrmMain.PlayButtonClick(Sender: TObject);
begin
  StopButton.Enabled := True;
  PauseButton.Enabled := True;
  PlayButton.Enabled := False;
  fAVIVideo.Play;
end;


procedure TFrmMain.PauseButtonClick(Sender: TObject);
begin
  StopButton.Enabled := True;
  PauseButton.Enabled := False;
  PlayButton.Enabled := True;
  fAVIVideo.Pause;
end;


procedure TFrmMain.StopButtonClick(Sender: TObject);
begin
  StopButton.Enabled := False;
  PauseButton.Enabled := False;
  PlayButton.Enabled := True;
  fAVIVideo.Stop;

  Image1.Picture.Bitmap.Width := 0;
  FramesLabel.Caption := IntToStr(fAVIVideo.CurrentFrame) + '/' + IntToStr(fAVIVideo.FrameCount);
  ProgressBar.Position := 0;
end;


procedure TFrmMain.CheckBox1Click(Sender: TObject);
begin
  Image1.Stretch := CheckBox1.Checked;
end;


procedure TFrmMain.DoubleHeightCheckClick(Sender: TObject);
begin
  fAVIVideo.DoubleHeight := DoubleHeightCheck.Checked;
  BlackLinesCheck.Enabled := DoubleHeightCheck.Checked;
  RefreshFrame(True);
end;


procedure TFrmMain.BlackLinesCheckClick(Sender: TObject);
begin
  fAVIVideo.BlackLines := BlackLinesCheck.Checked;
  RefreshFrame(True);
end;


procedure TFrmMain.BrightnessChange(Sender: TObject);
begin
  fAVIVideo.Brightness := Brightness.Value;
  RefreshFrame(True);
end;


end.
