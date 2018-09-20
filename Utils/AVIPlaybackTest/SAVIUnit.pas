unit SAVIUnit;
{
 By Stuart "Stucuk" Carey

 License:
  Feel Free to use this in whatever you want!
}

interface

uses
  Windows, SysUtils, Graphics, VFW, OALHandler;

type
  TFinishEvent = procedure of object;

  TSimpleTimer = Class(TObject)
  protected
    FFrequency,
    FLastTime:  Int64;    // last system time
    FFrameTime: Extended;
  public
    constructor Create;
    procedure Refresh;
    property FrameTime: Extended read FFrameTime;
  end;

  TAVI_State = (aviStopped, aviPlaying, aviPaused, aviNoFile);

  TAVI_Video = Record
    AFile:        PAVIFile;
    Stream_Video,
    Stream_Audio: PAVIStream;
    Frame:        PGETFRAME;
  end;

  TAVI = Class (TObject)
  protected
    FFilename: string;
    FBMP:      TBitmap;

    FAVI:      TAVI_Video;
    FAVIState: TAVI_State;

    FFPS: Extended;

    FCurrFrame,
    FLastFrame,
    FFrameCount:  Integer;
    FCurrentTime: Extended;
    FLoop:        Boolean;
    FTimer:       TSimpleTimer;

    FWidth,
    FHeight: Integer;

    FDoubleHeight,
    FBlackLines:   Boolean;
    FBrightness:   ShortInt;

    FSound: TOALSound;

    FOnFinish: TFinishEvent;
    function GetFrame(aIndForce: Boolean): Boolean;
  public
    constructor Create;
    destructor Destroy; override;

    procedure VidInit(aFilename: string; aIndLoop, aIndDoubleHeight: Boolean;
                      aOnFinish: TFinishEvent);
    procedure VidFree;

    function Idle(aIndForce: Boolean = False): Boolean;

    function GetFrameRGB : Pointer;

    procedure Restart;
    procedure Play;
    procedure Pause;
    procedure Stop;

    property Filename:     string  read FFilename;
    property BMP:          TBitmap read FBMP;
    property DoubleHeight: Boolean read FDoubleHeight write FDoubleHeight;
    property BlackLines:   Boolean read FBlackLines   write FBlackLines;

    property Width:        Integer read FWidth;
    property Height:       Integer read FHeight;
    property FrameCount:   Integer read FFrameCount;
    property CurrentFrame: Integer read FCurrFrame;

    property AVIState: TAVI_State read FAVIState;

    property Brightness: ShortInt read FBrightness write FBrightness;
  end;

implementation
uses
  MMSystem, openal_types, Math;

//--------------------------------[ Utility ]---------------------------------//
procedure DrawFrameToBMP(var aBmp: TBitmap; const aBmi: PBitmapInfoHeader);
var
  TempBMP:    TBitmap;
  DC_Handle:  HDC;
  DIB_Handle: HBITMAP;
begin
  TempBMP := TBitmap.Create;

  try
    DC_Handle := CreateDC('Display', nil, nil, nil);

    try
      DIB_Handle := CreateDIBitmap(
        DC_Handle,
        aBmi^,
        CBM_INIT,
        Pointer(Cardinal(aBmi) + aBmi.biSize + aBmi.biClrUsed * 4),
        PBITMAPINFO(aBmi)^,
        DIB_RGB_COLORS
      );
    finally
      DeleteDC(DC_Handle);
    end;

    TempBMP.Handle := DIB_Handle;

    aBmp.Height := aBmi.biHeight;
    aBmp.Width  := aBmi.biWidth;
    aBmp.Canvas.Draw(0, 0, TempBMP);
  
    DeleteObject(DIB_Handle);
  finally
    TempBMP.Free;
  end;
end;

function BMPToRaw(aBmp: TBitmap): Pointer;
var
  I: Integer;
begin
  GetMem(Result, 3 * aBmp.Width * aBmp.Height);

  for I := aBmp.Height - 1 downto 0 do
  begin
    CopyMemory(Result, aBmp.ScanLine[I], aBmp.Width * 3);
    Inc(Cardinal(Result), aBmp.Width * 3);
  end;
end;

function SetRGBTriple(aR, aG, aB: Byte): TRGBTriple;
begin
  Result.rgbtRed   := aR;
  Result.rgbtGreen := aG;
  Result.rgbtBlue  := aB;
end;

function GetVideoAverage(aX: Cardinal; aSL1, aSL2: Pointer): TRGBTriple;
var
  A, B: TRGBTriple;
begin
  if Assigned(aSL1) then
    A := PRGBTriple(Cardinal(aSL1) + 3 * aX)^
  else
    A := SetRGBTriple(0, 0, 0);

  if Assigned(aSL2) then
    B := PRGBTriple(Cardinal(aSL2) + 3 * aX)^
  else
    B := SetRGBTriple(0, 0, 0);

  Result.rgbtRed   := Integer(A.rgbtRed   + B.rgbtRed)   div 2;
  Result.rgbtGreen := Integer(A.rgbtGreen + B.rgbtGreen) div 2;
  Result.rgbtBlue  := Integer(A.rgbtBlue  + B.rgbtBlue)  div 2;
end;

procedure DoubleVideoFrame(aBmp: TBitmap; aIndBlackLines: Boolean);
var
  X,
  Y,
  OY:  Integer;
  P:   PRGBTriple;
  SL1,
  SL2: Pointer;
begin
  OY          := aBmp.Height;
  aBmp.Height := aBmp.Height * 2;

  for Y := OY - 1 downto 0 do
    CopyMemory(aBmp.ScanLine[(Y + 1) * 2 - 1], aBmp.ScanLine[Y], aBmp.Width * 3);

  if aIndBlackLines then
  begin
    for Y := 0 to OY-1 do
      ZeroMemory(aBmp.ScanLine[Y * 2], aBmp.Width * 3);

    Exit;
  end;

  for Y := 0 to OY-1 do
  begin
    P := aBmp.ScanLine[Y * 2];

    if (Y - 1) >= 0 then
      SL1 := aBmp.ScanLine[Y*2-1]
    else
      SL1 := Nil;

    if (Y + 1) < aBmp.Height then
      SL2 := aBmp.ScanLine[Y * 2 + 1]
    else
      SL2 := Nil;

    for X := 0 to aBmp.Width-1 do
    begin
      P^ := GetVideoAverage(X, SL1, SL2);
      Inc(Cardinal(P), 3);
    end;
  end;
end;

function MMB(Value: Integer): Byte;
begin
  Result := Max(Min(255,Value),0);
end;

procedure ApplyBrightness(BMP : TBitmap; Brightness : ShortInt);
var
  X,
  Y: Integer;
  P: PRGBTriple;
begin
 for Y := 0 to BMP.Height-1 do
 begin
  P := BMP.ScanLine[Y];
  for X := 0 to BMP.Width-1 do
  begin
   P^ := SetRGBTriple(MMB(P^.rgbtRed+Brightness),MMB(P^.rgbtGreen+Brightness),MMB(P^.rgbtBlue+Brightness));
   Inc(Cardinal(P),3);
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
  FLastTime  := CurTime;
end;

//----------------------------------[ TAVI ]----------------------------------//

constructor TAVI.Create;
begin
  inherited Create;
  ZeroMemory(@FAVI,SizeOf(TAVI_Video));
  FTimer    := TSimpleTimer.Create;
  FAVIState := aviNoFile;
end;

destructor TAVI.Destroy;
begin
  Inherited;
  VidFree;
end;

procedure TAVI.VidFree;
begin
  if FAVIState = aviNoFile then
    Exit;

  FAVIState := aviNoFile;

  FBMP.Free;

  AVIStreamGetFrameClose(FAVI.Frame);
  AVIStreamRelease(FAVI.Stream_Video);
  AVIStreamRelease(FAVI.Stream_Audio);
  AVIFileRelease(FAVI.AFile);
  ZeroMemory(@FAVI,SizeOf(TAVI_Video));

  if Assigned(FSound) then
  begin
    FSound.Stop;
    FSound.Free;
  end;
end;

procedure TAVI.VidInit(aFilename: string; aIndLoop, aIndDoubleHeight: Boolean;
                       aOnFinish: TFinishEvent);
var
  WF:               PPCMWaveFormat;
  WFSize:           Cardinal;
  SoundFormat,
  BufferSize:       Integer;
  Buffer:           Pointer;
  StreamInfo_Video: TAVISTREAMINFO;
begin
  VidFree;
  FLoop         := aIndLoop;
  FDoubleHeight := aIndDoubleHeight;
  FLastFrame    := -1;
  FOnFinish     := aOnFinish;
  FFilename     := ChangeFileExt(aFilename, '.avi');

  if not FileExists(FFilename) then
  begin
    if Assigned(FOnFinish) then
      FOnFinish;

    Exit;
  end;

  AVIFileOpen(FAVI.AFile, PAnsiChar(AnsiString(FFilename)), OF_READ, nil);
  AVIFileGetStream(FAVI.AFile,FAVI.Stream_Video, streamtypeVIDEO, 0);
  AVIFileGetStream(FAVI.AFile,FAVI.Stream_Audio, streamtypeAUDIO, 0);
  AVIStreamInfoA(FAVI.Stream_Video, @StreamInfo_Video, SizeOf(TAVISTREAMINFO));

  FFPS        := 1 / (StreamInfo_Video.dwRate / StreamInfo_Video.dwScale);
  FFrameCount := StreamInfo_Video.dwLength;

  if Assigned(FAVI.Stream_Audio) then
  begin
    WFSize := SizeOf(TPCMWaveFormat);
    GetMem(WF, WFSize);
    AVIStreamReadFormat(FAVI.Stream_Audio, 0, WF, @WFSize);

    AVIStreamRead(
      FAVI.Stream_Audio,
      0,
      AVIStreamTimeToSample(FAVI.Stream_Audio, AVIStreamEndTime(FAVI.Stream_Audio)),
      nil,
      0,
      @BufferSize,
      nil
    );
    GetMem(Buffer,BufferSize);
    ZeroMemory(Buffer,BufferSize);
    AVIStreamRead(
      FAVI.Stream_Audio,
      0,
      AVIStreamTimeToSample(FAVI.Stream_Audio, AVIStreamEndTime(FAVI.Stream_Audio)),
      Buffer,
      BufferSize,
      nil,
      nil
    );

    case WF.wf.nChannels of
      1 : SoundFormat := AL_FORMAT_MONO8;
      2 : SoundFormat := AL_FORMAT_STEREO8;
      else Exit; // Error!
    end;

    if WF.wBitsPerSample = 16 then
      Inc(SoundFormat);

    FSound := TOALSound.Create(
      SoundFormat,
      WF.wf.nSamplesPerSec,
      BufferSize,
      Buffer,
      aIndLoop
    );

    FreeMem(Buffer);
  end else
   FSound := Nil;

  FAVI.Frame := AVIStreamGetFrameOpen(FAVI.Stream_Video, Nil);

  if not Assigned(FAVI.Frame) then
  begin
    FOnFinish;
    Exit;
  end;

  FBMP             := TBitmap.Create;
  FBMP.PixelFormat := pf24bit;
  FCurrentTime     := 0;
  FCurrFrame       := 0;

  GetFrame(True);
  FLastFrame := -1;
  FWidth     := FBMP.Width;
  FHeight    := FBMP.Height;

  FAVIState := aviStopped;
end;

procedure TAVI.Restart;
begin
  Stop;
  Play;
end;

function TAVI.GetFrame(aIndForce: Boolean): Boolean;
begin
  Result := (FCurrFrame <> FLastFrame) or aIndForce;

  if not Result then
    Exit;

  FLastFrame := FCurrFrame;
  DrawFrameToBMP(FBMP, AVIStreamGetFrame(FAVI.Frame, FCurrFrame));

  if FDoubleHeight then
    DoubleVideoFrame(FBMP, BlackLines);

  if FBrightness <> 0 then
    ApplyBrightness(FBMP, FBrightness);
end;

function TAVI.GetFrameRGB: Pointer;
begin
  Result := BMPToRaw(FBMP);
end;

function TAVI.Idle(aIndForce: Boolean = False): Boolean;
begin
  Result := False;

  if FAVIState in [aviNoFile, aviStopped, aviPaused] then
  begin
    if aIndForce then
      Result := GetFrame(True);

    Exit;
  end;

  FTimer.Refresh;
  FCurrentTime := FCurrentTime + FTimer.FrameTime;
  FCurrFrame   := Round(FCurrentTime/FFPS);

  if FCurrFrame >= FFrameCount then
  begin
    if FLoop then
      Restart
    else
      Stop;

    Exit;
  end;

  Result := GetFrame(aIndForce);
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
    Play;
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

  FAVIState    := aviStopped;
  FCurrentTime := 0;
  FCurrFrame   := 0;
  FLastFrame   := -1;

  if Assigned(FSound) then
    FSound.Stop;

  if Assigned(FOnFinish) then
    FOnFinish;
end;

end.
