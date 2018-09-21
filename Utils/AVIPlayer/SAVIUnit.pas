unit SAVIUnit;
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

uses Windows, SysUtils, VFW, graphics, OALHandler;

type
  TSimpleTimer = Class(TObject)
  protected
    FFrequency: Int64;
    FLastTime: Int64; // last system time
    FFrameTime: Extended;
  public
    constructor Create;
    procedure Refresh;
    property FrameTime: Extended read FFrameTime;
  end;

  TAVI_State = (aviStopped, aviPlaying, aviPaused, aviNoFile);

  TAVI_Video = Record
    AFile: PAVIFile;
    Stream_Video, Stream_Audio: PAVIStream;
    Frame: PGETFRAME;
  end;

  TAVI = Class(TObject)
  protected
    FFilename: AnsiString;
    FBMP: TBitmap;

    FAVI: TAVI_Video;
    FAVIState: TAVI_State;

    FFPS: Extended;

    FCurrFrame, FLastFrame, FFrameCount: Integer;
    FCurrentTime: Extended;
    FLoop: Boolean;
    FTimer: TSimpleTimer;

    FWidth, FHeight: Integer;

    FDoubleHeight: Boolean;
    FBlackLines: Boolean;
    FBrightness: ShortInt;

    FSound: TOALSound;

    FOnFinish: Procedure();
    function GetFrame(Force: Boolean): Boolean;
  public

    constructor Create();
    destructor Destroy; override;

    procedure VidInit(Filename: AnsiString; Loop, DoubleHeight: Boolean; AOnFinish: Pointer);
    procedure VidFree();

    function Idle(Force: Boolean = False): Boolean;

    function GetFrameRGB: Pointer;

    procedure Restart;
    procedure Play;
    procedure Pause;
    procedure Stop;

    property Filename: AnsiString read FFilename;
    property BMP: TBitmap read FBMP;
    property DoubleHeight: Boolean read FDoubleHeight write FDoubleHeight;
    property BlackLines: Boolean read FBlackLines write FBlackLines;

    property Width: Integer read FWidth;
    property Height: Integer read FHeight;
    property FrameCount: Integer read FFrameCount;
    property CurrentFrame: Integer read FCurrFrame;

    property AVIState: TAVI_State read FAVIState;

    property Brightness: ShortInt read FBrightness write FBrightness;
  end;

implementation

uses MMSystem, openal, math;

//--------------------------------[ Utility ]---------------------------------//

procedure DrawFrameToBMP(BMP: TBitmap; const BitmapInfo: PBITMAPINFOHEADER);
var
  TempBMP: TBitmap;
  DC_Handle: HDC;
  DIB_Handle: HBITMAP;
begin
  TempBMP := TBitmap.Create;
  try
    DC_Handle := CreateDC('Display', nil, nil, nil);
    try
      DIB_Handle := CreateDIBitmap(DC_Handle, BitmapInfo^, CBM_INIT,
        Pointer(Cardinal(BitmapInfo) + BitmapInfo.biSize + BitmapInfo.biClrUsed * 4), PBITMAPINFO(BitmapInfo)^,
        DIB_RGB_COLORS);
    finally
      DeleteDC(DC_Handle);
    end;

    TempBMP.Handle := DIB_Handle;

    BMP.Height := BitmapInfo.biHeight;
    BMP.Width := BitmapInfo.biWidth;
    BMP.Canvas.Draw(0, 0, TempBMP);

    DeleteObject(DIB_Handle);
  finally
    TempBMP.Free;
  end;
end;

function BMPToRaw(BMP: TBitmap): Pointer;
var
  Y: Integer;
  P: Pointer;
begin
  GetMem(Result, 3 * BMP.Width * BMP.Height);
  P := Result;
  for Y := BMP.Height - 1 downto 0 do
  begin
    CopyMemory(P, BMP.ScanLine[Y], BMP.Width * 3);
    Inc(Cardinal(P), BMP.Width * 3);
  end;
end;

function SetRGBTriple(R, G, B: Byte): TRGBTriple;
begin
  Result.rgbtRed := R;
  Result.rgbtGreen := G;
  Result.rgbtBlue := B;
end;

function GetVideoAverage(BMP: TBitmap; X: Integer; SL1, SL2: Pointer): TRGBTriple;
var
  A, B: TRGBTriple;
begin
  if Assigned(SL1) then
    A := PRGBTriple(Cardinal(SL1) + 3 * X)^
  else
    A := SetRGBTriple(0, 0, 0);

  if Assigned(SL2) then
    B := PRGBTriple(Cardinal(SL2) + 3 * X)^
  else
    B := SetRGBTriple(0, 0, 0);

  Result.rgbtRed := Integer(A.rgbtRed + B.rgbtRed) div 2;
  Result.rgbtGreen := Integer(A.rgbtGreen + B.rgbtGreen) div 2;
  Result.rgbtBlue := Integer(A.rgbtBlue + B.rgbtBlue) div 2;
end;

procedure DoubleVideoFrame(BMP: TBitmap; BlackLines: Boolean);
var
  X, Y, OY: Integer;
  P: PRGBTriple;
  SL1, SL2: Pointer;
begin
  OY := BMP.Height;
  BMP.Height := BMP.Height * 2;
  for Y := OY - 1 downto 0 do
    CopyMemory(BMP.ScanLine[(Y + 1) * 2 - 1], BMP.ScanLine[Y], BMP.Width * 3);

  if BlackLines then
  begin
    for Y := 0 to OY - 1 do
      ZeroMemory(BMP.ScanLine[Y * 2], BMP.Width * 3);
    Exit;
  end;

  for Y := 0 to OY - 1 do
  begin
    P := BMP.ScanLine[Y * 2];

    if (Y - 1) >= 0 then
      SL1 := BMP.ScanLine[Y * 2 - 1]
    else
      SL1 := Nil;

    if (Y + 1) < BMP.Height then
      SL2 := BMP.ScanLine[Y * 2 + 1]
    else
      SL2 := Nil;

    for X := 0 to BMP.Width - 1 do
    begin
      P^ := GetVideoAverage(BMP, X, SL1, SL2);
      Inc(Cardinal(P), 3);
    end;
  end;
end;

function MMB(Value: Integer): Byte;
begin
  Result := Max(Min(255, Value), 0);
end;

procedure ApplyBrightness(BMP: TBitmap; Brightness: ShortInt);
var
  X, Y: Integer;
  P: PRGBTriple;
begin
  for Y := 0 to BMP.Height - 1 do
  begin
    P := BMP.ScanLine[Y];
    for X := 0 to BMP.Width - 1 do
    begin
      P^ := SetRGBTriple(MMB(P^.rgbtRed + Brightness), MMB(P^.rgbtGreen + Brightness), MMB(P^.rgbtBlue + Brightness));
      Inc(Cardinal(P), 3);
    end;
  end;
end;

//------------------------------[ TSimpleTimer ]------------------------------//

constructor TSimpleTimer.Create;
begin
  QueryPerformanceFrequency(FFrequency); // get high-resolution Frequency
  QueryPerformanceCounter(FLastTime);
end;

procedure TSimpleTimer.Refresh;
var
  CurTime: Int64;
begin
  QueryPerformanceCounter(CurTime);
  FFrameTime := (CurTime - FLastTime) / FFrequency;
  FLastTime := CurTime;
end;

//----------------------------------[ TAVI ]----------------------------------//

constructor TAVI.Create();
begin
  inherited Create();
  FFilename := '';
  FBMP := Nil;
  FOnFinish := Nil;
  ZeroMemory(@FAVI, SizeOf(TAVI_Video));
  FTimer := TSimpleTimer.Create;
  FSound := Nil;
  FBlackLines := False;
  FBrightness := 0;

  FAVIState := aviNoFile;
end;

destructor TAVI.Destroy;
begin
  Inherited;
  VidFree();
end;

procedure TAVI.VidFree();
begin
  if FAVIState = aviNoFile then
    Exit;

  FAVIState := aviNoFile;

  FBMP.Free;

  AVIStreamGetFrameClose(FAVI.Frame);
  AVIStreamRelease(FAVI.Stream_Video);
  AVIStreamRelease(FAVI.Stream_Audio);
  AVIFileRelease(FAVI.AFile);
  ZeroMemory(@FAVI, SizeOf(TAVI_Video));

  if Assigned(FSound) then
  begin
    FSound.Stop;
    FSound.Free;
  end;
end;

procedure TAVI.VidInit(Filename: AnsiString; Loop, DoubleHeight: Boolean; AOnFinish: Pointer);
var
  WF: PPCMWaveFormat;
  WFSize: Cardinal;
  SoundFormat: Integer;
  Buffer: Pointer;
  BufferSize: Integer;
  StreamInfo_Video: TAVISTREAMINFO;
begin
  VidFree();
  FLoop := Loop;
  FDoubleHeight := DoubleHeight;
  FLastFrame := -1;
  @FOnFinish := AOnFinish;

  FFilename := ChangeFileExt(Filename, '.avi');

  if not FileExists(FFilename) then
  begin
    if Assigned(FOnFinish) then
      FOnFinish();
    Exit;
  end;

  AVIFileOpen(FAVI.AFile, PAnsiChar(FFilename), OF_READ, nil);
  AVIFileGetStream(FAVI.AFile, FAVI.Stream_Video, streamtypeVIDEO, 0);
  AVIFileGetStream(FAVI.AFile, FAVI.Stream_Audio, streamtypeAUDIO, 0);
  AVIStreamInfoA(FAVI.Stream_Video, @StreamInfo_Video, SizeOf(TAVISTREAMINFO));

  FFPS := 1 / (StreamInfo_Video.dwRate / StreamInfo_Video.dwScale);
  FFrameCount := StreamInfo_Video.dwLength;

  if Assigned(FAVI.Stream_Audio) then
  begin
    WFSize := SizeOf(TPCMWaveFormat);
    GetMem(WF, WFSize);
    AVIStreamReadFormat(FAVI.Stream_Audio, 0, WF, @WFSize);

    AVIStreamRead(FAVI.Stream_Audio, 0, AVIStreamTimeToSample(FAVI.Stream_Audio, AVIStreamEndTime(FAVI.Stream_Audio)),
      nil, 0, @BufferSize, nil);
    GetMem(Buffer, BufferSize);
    ZeroMemory(Buffer, BufferSize);
    AVIStreamRead(FAVI.Stream_Audio, 0, AVIStreamTimeToSample(FAVI.Stream_Audio, AVIStreamEndTime(FAVI.Stream_Audio)),
      Buffer, BufferSize, nil, nil);

    case WF.WF.nChannels of
      1:
        SoundFormat := AL_FORMAT_MONO8;
      2:
        SoundFormat := AL_FORMAT_STEREO8;
    else
      Exit; // Error!
    end;

    if WF.wBitsPerSample = 16 then
      Inc(SoundFormat);

    FSound := TOALSound.Create(SoundFormat, WF.WF.nSamplesPerSec, BufferSize, Buffer, Loop);

    FreeMem(Buffer);
  end
  else
    FSound := Nil;

  FAVI.Frame := AVIStreamGetFrameOpen(FAVI.Stream_Video, Nil);

  if not Assigned(FAVI.Frame) then
  begin
    FOnFinish();
    Exit;
  end;

  FBMP := TBitmap.Create;
  FBMP.PixelFormat := pf24bit;
  FCurrentTime := 0;
  FCurrFrame := 0;

  GetFrame(True);
  FLastFrame := -1;
  FWidth := FBMP.Width;
  FHeight := FBMP.Height;

  FAVIState := aviStopped;
end;

procedure TAVI.Restart;
begin
  Stop;
  Play;
end;

function TAVI.GetFrame(Force: Boolean): Boolean;
begin
  Result := (FCurrFrame <> FLastFrame) or Force;
  if not Result then
    Exit;

  FLastFrame := FCurrFrame;

  DrawFrameToBMP(FBMP, AVIStreamGetFrame(FAVI.Frame, FCurrFrame));
  if FDoubleHeight then
    DoubleVideoFrame(FBMP, BlackLines);
  if FBrightness <> 0 then
  begin
    ApplyBrightness(FBMP, FBrightness);
  end;
end;

function TAVI.GetFrameRGB: Pointer;
begin
  Result := BMPToRaw(FBMP);
end;

function TAVI.Idle(Force: Boolean = False): Boolean;
begin
  Result := False;

  if FAVIState in [aviNoFile, aviStopped, aviPaused] then
  begin
    if Force then
      Result := GetFrame(True);
    Exit;
  end;

  FTimer.Refresh;
  FCurrentTime := FCurrentTime + FTimer.FrameTime;
  FCurrFrame := Round(FCurrentTime / FFPS);

  if (FCurrFrame >= FFrameCount) then
  begin
    if FLoop then
      Restart
    else
      Stop;
    Exit;
  end;

  Result := GetFrame(Force);
end;

procedure TAVI.Play;
begin
  if FAVIState in [aviNoFile, aviPlaying] then
    Exit;

  FAVIState := aviPlaying;

  if Assigned(FSound) then
    FSound.Play;

  FTimer.Refresh;
end;

procedure TAVI.Pause;
begin
  if FAVIState = aviNoFile then
    Exit;

  if FAVIState = aviPaused then
  begin
    Play();
    Exit;
  end;

  FAVIState := aviPaused;
  if Assigned(FSound) then
    FSound.Pause;
end;

procedure TAVI.Stop;
begin
  if FAVIState in [aviNoFile, aviStopped] then
    Exit;

  FAVIState := aviStopped;
  FCurrentTime := 0;
  FCurrFrame := 0;
  FLastFrame := -1;

  if Assigned(FSound) then
    FSound.Stop;

  if Assigned(FOnFinish) then
    FOnFinish();
end;

begin
  AVIFileInit;

end.
