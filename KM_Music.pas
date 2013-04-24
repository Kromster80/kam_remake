unit KM_Music;
{$I KaM_Remake.inc}
interface

//We have two choices for music libraries:
//BASS: Free for non-commercial projects. Requires bass.dll. Website: http://www.un4seen.com/
//ZLibPlay: GNU GPL license. Requires libzplay.dll. Website: http://libzplay.sourceforge.net/

//Comparision:  - BASS's DLL is much smaller (102kb vs 2.13mb(!)) and BASS seems faster at loading tracks.
//              - ZLibPlay supports more formats, (FLAC, AC-3, AAC, PCM) but we don't care
//              - ZLibPlay is GPL but BASS is not, and BASS can only be used for free in non-commercial products

{.DEFINE USEBASS}
{$IFDEF MSWindows}
  {$DEFINE USELIBZPLAY}
{$ENDIF}

uses Classes, SysUtils, KromUtils, Math,
     KM_Defaults
     {$IFDEF USEBASS}     , Bass {$ENDIF}
     {$IFDEF USELIBZPLAY} , libZPlay {$ENDIF}
     ;

type TFadeState = (fsNone, fsFadeOut, fsFadeIn, fsFaded);

type
  TMusicLib = class
  private
    MusicCount: Integer;
    MusicIndex: Integer; //Points to the index in TrackOrder of the current track
    MusicTracks: array[1..256]of string;
    TrackOrder: array[1..256]of byte; //Each index points to an index of MusicTracks
    //MIDICount,MIDIIndex:integer;
    //MIDITracks:array[1..256]of string;
    IsMusicInitialized: Boolean;
    MusicGain: Single;
    {$IFDEF USEBASS} fBassStream, fBassOtherStream: Cardinal; {$ENDIF}
    {$IFDEF USELIBZPLAY} ZPlayer, ZPlayerOther: ZPlay; {$ENDIF} //I dislike that it's not TZPlay... Guess they don't know Delphi conventions.
    fFadeState: TFadeState;
    fFadeStarted: Cardinal;
    fToPlayAfterFade: string;
    fFadedToPlayOther: Boolean;
    fOtherVolume: Single;
    function PlayMusicFile(FileName: string): Boolean;
    function PlayOtherFile(FileName: string): Boolean;
    procedure ScanMusicTracks(Path: string);
    procedure ShuffleSongs; //should not be seen outside of this class
    procedure UnshuffleSongs;
  public
    constructor Create(aVolume: Single);
    destructor Destroy; override;
    procedure UpdateMusicVolume(Value: Single);
    procedure PlayMenuTrack;
    procedure PlayNextTrack;
    procedure PlayPreviousTrack;
    function IsMusicEnded: Boolean;
    function IsOtherEnded: Boolean;
    procedure StopMusic;
    procedure ToggleMusic(aEnableMusic: Boolean);
    procedure ToggleShuffle(aEnableShuffle: Boolean);
    procedure FadeMusic(Sender: TObject);
    procedure UnfadeMusic(Sender: TObject);
    procedure PauseMusicToPlayFile(aFileName: string; aVolume: Single);
    procedure StopPlayingOtherFile;
    function GetTrackTitle: string;
    procedure UpdateStateIdle; //Used for fading
  end;


implementation
uses KM_Log, KM_Utils;

const FADE_TIME = 2000; //Time that a fade takes to occur in ms


{Music Lib}
constructor TMusicLib.Create(aVolume: Single);
var I: Integer;
begin
  inherited Create;
  IsMusicInitialized := True;
  ScanMusicTracks(ExeDir + 'Music'+PathDelim);


  {$IFDEF USELIBZPLAY}
  ZPlayer := ZPlay.Create; //Note: They should have used TZPlay not ZPlay for a class
  ZPlayerOther := ZPlay.Create;
  {$ENDIF}

  {$IFDEF USEBASS}
  // Setup output - default device, 44100hz, stereo, 16 bits
  if not BASS_Init(-1, 44100, 0, 0, nil) then
  begin
    gLog.AddTime('Failed to initialize the music playback device');
    IsMusicInitialized := False;
  end;
  {$ENDIF}

  UpdateMusicVolume(aVolume);

  // Initialise TrackOrder
  for I := 1 to MusicCount do
    TrackOrder[I] := I;

  gLog.AddTime('Music init done, ' + IntToStr(MusicCount) + ' tracks found');
end;


destructor TMusicLib.Destroy;
begin
  {$IFDEF USELIBZPLAY}
  ZPlayer.Free;
  ZPlayerOther.Free;
  {$ENDIF}

  {$IFDEF USEBASS}
  BASS_Stop; //Stop all Bass output
  //Free the streams we may have used (will just return false if the stream is invalid)
  BASS_StreamFree(fBassStream);
  BASS_StreamFree(fBassOtherStream);
  BASS_Free; //Frees this usage of BASS, allowing it to be recreated successfully
  {$ENDIF}

  inherited;
end;


function TMusicLib.PlayMusicFile(FileName:string):boolean;
{$IFDEF USEBASS} var ErrorCode: Integer; {$ENDIF}
begin
  Result := False;
  if not IsMusicInitialized then Exit;
  if fFadeState <> fsNone then exit; //Don't start a new track while fading or faded

  //Cancel previous sound
  {$IFDEF USELIBZPLAY} ZPlayer.StopPlayback; {$ENDIF}
  {$IFDEF USEBASS} BASS_ChannelStop(fBassStream); {$ENDIF}

  if not FileExists(FileName) then exit; //Make it silent

  {$IFDEF USELIBZPLAY}
  Result := ZPlayer.OpenFile(FileName, sfAutodetect); //Detect file type automatically
  if not Result then exit; //File failed to load
  Result := ZPlayer.StartPlayback;
  if not Result then exit; //Playback failed to start
  {$ENDIF}
  {$IFDEF USEBASS}
  BASS_StreamFree(fBassStream); //Free the existing stream (will just return false if the stream is invalid)
  fBassStream := BASS_StreamCreateFile(FALSE, PChar(FileName), 0, 0, BASS_STREAM_AUTOFREE {$IFDEF UNICODE} or BASS_UNICODE{$ENDIF});

  BASS_ChannelPlay(fBassStream, True); //Start playback from the beggining

  ErrorCode := BASS_ErrorGetCode;
  if ErrorCode <> BASS_OK then exit; //Error
  {$ENDIF}

  UpdateMusicVolume(MusicGain); //Need to reset music volume after starting playback
  Result := True;
end;


function TMusicLib.PlayOtherFile(FileName: string): Boolean;
{$IFDEF USEBASS} var ErrorCode: Integer; {$ENDIF}
begin
  Result := False;
  if not IsMusicInitialized then exit;

  //Cancel previous sound
  {$IFDEF USELIBZPLAY} ZPlayerOther.StopPlayback; {$ENDIF}
  {$IFDEF USEBASS} BASS_ChannelStop(fBassOtherStream); {$ENDIF}

  if not FileExists(FileName) then exit; //Make it silent

  {$IFDEF USELIBZPLAY}
  Result := ZPlayerOther.OpenFile(FileName, sfAutodetect); //Detect file type automatically
  if not Result then exit; //File failed to load
  Result := ZPlayerOther.StartPlayback;
  if not Result then exit; //Playback failed to start
  {$ENDIF}
  {$IFDEF USEBASS}
  BASS_StreamFree(fBassOtherStream); //Free the existing stream (will just return false if the stream is invalid)
  fBassOtherStream := BASS_StreamCreateFile(FALSE, PChar(FileName), 0, 0, BASS_STREAM_AUTOFREE {$IFDEF UNICODE} or BASS_UNICODE{$ENDIF});

  BASS_ChannelPlay(fBassOtherStream, True); //Start playback from the beggining

  ErrorCode := BASS_ErrorGetCode;
  if ErrorCode <> BASS_OK then exit; //Error
  {$ENDIF}

  //Now set the volume to the desired level
  {$IFDEF USELIBZPLAY}
  ZPlayerOther.SetPlayerVolume(Round(fOtherVolume * 100), Round(fOtherVolume * 100)); //0=silent, 100=max
  {$ENDIF}
  {$IFDEF USEBASS}
  BASS_ChannelSetAttribute(fBassOtherStream, BASS_ATTRIB_VOL, fOtherVolume); //0=silent, 1=max
  {$ENDIF}

  Result := True;
end;


{Update music gain (global volume for all sounds/music)}
procedure TMusicLib.UpdateMusicVolume(Value: Single);
begin
  if not IsMusicInitialized then exit; //Keep silent
  MusicGain := Value;
  {$IFDEF USELIBZPLAY}
  ZPlayer.SetPlayerVolume(Round(Value * 100), Round(Value * 100)); //0=silent, 100=max
  {$ENDIF}
  {$IFDEF USEBASS}
  BASS_ChannelSetAttribute(fBassStream, BASS_ATTRIB_VOL, Value); //0=silent, 1=max
  {$ENDIF}
end;


procedure TMusicLib.ScanMusicTracks(Path: string);
var SearchRec: TSearchRec;
begin
  if not IsMusicInitialized then exit;
  MusicCount := 0;
  if not DirectoryExists(Path) then exit;

  FindFirst(Path + '*', faDirectory, SearchRec);
  repeat
    if (SearchRec.Attr and faDirectory <> faDirectory)
    and (SearchRec.Name <> '.') and (SearchRec.Name <> '..') then
    if (GetFileExt(SearchRec.Name) = 'MP3') or //Allow all formats supported by both libraries
       (GetFileExt(SearchRec.Name) = 'MP2') or
       (GetFileExt(SearchRec.Name) = 'MP1') or
       (GetFileExt(SearchRec.Name) = 'WAV') or
       (GetFileExt(SearchRec.Name) = 'OGG')
       {$IFDEF USEBASS} //Formats supported by BASS but not LibZPlay
       or (GetFileExt(SearchRec.Name) = 'AIFF')
       {$ENDIF}
       {$IFDEF USELIBZPLAY} //Formats supported by LibZPlay but not BASS
       or (GetFileExt(SearchRec.Name) = 'FLAC')
       or (GetFileExt(SearchRec.Name) = 'OGA')
       or (GetFileExt(SearchRec.Name) = 'AC3')
       or (GetFileExt(SearchRec.Name) = 'AAC')
       or (GetFileExt(SearchRec.Name) = 'OGA')
       {$ENDIF} then
       begin
      Inc(MusicCount);
      MusicTracks[MusicCount] := Path + SearchRec.Name;
    end;
    {if GetFileExt(SearchRec.Name)='MID' then
    begin
      inc(MIDICount);
      MIDITracks[MIDICount] := Path + SearchRec.Name;
    end;}
  until (FindNext(SearchRec) <> 0);
  FindClose(SearchRec);
  MusicIndex := 0;
end;


procedure TMusicLib.PlayMenuTrack;
begin
  if not IsMusicInitialized then exit;
  if MusicIndex = 1 then exit; //It's already playing
  MusicIndex := 1;
  PlayMusicFile(MusicTracks[1]);
end;


procedure TMusicLib.PlayNextTrack;
begin
  if not IsMusicInitialized then exit;
  if MusicCount = 0 then exit; //no music files found
  if fFadeState <> fsNone then exit;

  //Set next index, looped or random
  MusicIndex := MusicIndex mod MusicCount + 1;
  PlayMusicFile(MusicTracks[TrackOrder[MusicIndex]]);
end;


procedure TMusicLib.PlayPreviousTrack;
begin
  if not IsMusicInitialized then exit;
  if MusicCount = 0 then exit; //no music files found
  if fFadeState <> fsNone then exit;

  Dec(MusicIndex); //Set to previous
  if MusicIndex = 0 then MusicIndex := MusicCount; //Loop to the top
  PlayMusicFile(MusicTracks[TrackOrder[MusicIndex]]);
end;


//Check if Music is not playing, to know when new mp3 should be feeded
function TMusicLib.IsMusicEnded: Boolean;
{$IFDEF USELIBZPLAY} var Status: TStreamStatus; {$ENDIF}
begin
  {$IFDEF USELIBZPLAY} ZPlayer.GetStatus(Status); {$ENDIF}
  Result := IsMusicInitialized
            {$IFDEF USELIBZPLAY}
            and (not Status.fPlay and not Status.fPause) //Not playing and not paused due to fade
            {$ENDIF}
            {$IFDEF USEBASS}
            and (BASS_ChannelIsActive(fBassStream) = BASS_ACTIVE_STOPPED)
            {$ENDIF}
            ;
end;


//Check if other is not playing, to know when to return to the music
function TMusicLib.IsOtherEnded: Boolean;
{$IFDEF USELIBZPLAY} var Status: TStreamStatus; {$ENDIF}
begin
  {$IFDEF USELIBZPLAY} ZPlayerOther.GetStatus(Status); {$ENDIF}
  Result := IsMusicInitialized
            {$IFDEF USELIBZPLAY}
            and (not Status.fPlay) //Not playing and not paused due to fade
            {$ENDIF}
            {$IFDEF USEBASS}
            and (BASS_ChannelIsActive(fBassOtherStream) = BASS_ACTIVE_STOPPED)
            {$ENDIF}
            ;
end;


procedure TMusicLib.StopMusic;
begin
  if not IsMusicInitialized then exit;
  {$IFDEF USELIBZPLAY} ZPlayer.StopPlayback; {$ENDIF}
  {$IFDEF USEBASS} BASS_ChannelStop(fBassStream); {$ENDIF}
  MusicIndex := 0;
end;


procedure TMusicLib.ToggleMusic(aEnableMusic: Boolean);
begin
  if aEnableMusic then
    PlayMenuTrack //Start with the default track
  else
    StopMusic;
end;


procedure TMusicLib.ToggleShuffle(aEnableShuffle: Boolean);
begin
  if aEnableShuffle then
    ShuffleSongs
  else
    UnshuffleSongs;
end;


procedure TMusicLib.ShuffleSongs;
var I, R, NewIndex: Integer;
begin
  if MusicIndex = 0 then exit; // Music is disabled
  NewIndex := MusicIndex;
  for I := MusicCount downto 2 do
  begin
    R := RandomRange(2, I);
    //Remember the track number of the current track
    if TrackOrder[R] = MusicIndex then
      NewIndex := I;
    KromUtils.SwapInt(TrackOrder[R], TrackOrder[I]);
  end;
  MusicIndex := NewIndex;
end;


procedure TMusicLib.UnshuffleSongs;
var I: Integer;
begin
  if MusicIndex = 0 then Exit; // Music is disabled
  MusicIndex := TrackOrder[MusicIndex];
  //Reset every index of the TrackOrder array
  for I := 1 to MusicCount do
    TrackOrder[I] := I;
end;


procedure TMusicLib.FadeMusic;
{$IFDEF USELIBZPLAY} var StartTime, EndTime: TStreamTime; Left, Right:integer; {$ENDIF}
begin
  if (not IsMusicInitialized) then exit;
  fFadeState := fsFadeOut; //Fade it out
  fFadeStarted := TimeGet;
  {$IFDEF USELIBZPLAY}
  ZPlayer.GetPosition(StartTime);
  EndTime.ms := StartTime.ms + FADE_TIME;
  ZPlayer.GetPlayerVolume(Left, Right); //Start fade from the current volume
  ZPlayer.SlideVolume(tfMillisecond, StartTime, Left, Right, tfMillisecond, EndTime, 0, 0);
  {$ENDIF}
  {$IFDEF USEBASS}
  BASS_ChannelSlideAttribute(fBassStream, BASS_ATTRIB_VOL, 0, FADE_TIME);
  {$ENDIF}
end;


procedure TMusicLib.UnfadeMusic;
{$IFDEF USELIBZPLAY}
var
  StartTime, EndTime: TStreamTime;
  Left, Right: Integer;
{$ENDIF}
begin
  if (not IsMusicInitialized) then exit;
  fFadeState := fsFadeIn; //Fade it in
  fFadeStarted := TimeGet;
  {$IFDEF USELIBZPLAY}
  //LibZPlay has a nice SlideVolume function we can use
  ZPlayer.ResumePlayback; //Music may have been paused due to fade out
  ZPlayer.GetPosition(StartTime);
  EndTime.ms := StartTime.ms + FADE_TIME;
  ZPlayer.GetPlayerVolume(Left, Right); //Start fade from the current volume
  ZPlayer.SlideVolume(tfMillisecond, StartTime, Left, Right, tfMillisecond, EndTime, Round(MusicGain * 100), Round(MusicGain * 100));
  {$ENDIF}
  {$IFDEF USEBASS}
  BASS_ChannelPlay(fBassStream, False); //Music may have been paused due to fade out
  BASS_ChannelSlideAttribute(fBassStream, BASS_ATTRIB_VOL, MusicGain, FADE_TIME);
  {$ENDIF}
end;


procedure TMusicLib.UpdateStateIdle;
begin
  if not IsMusicInitialized then exit;

  if fFadeState in [fsFadeIn, fsFadeOut] then
  begin
    if GetTimeSince(fFadeStarted) > FADE_TIME then
    begin
      if fFadeState = fsFadeOut then //Fade out is complete so pause the music
      begin
        fFadeState := fsFaded;
        {$IFDEF USELIBZPLAY} ZPlayer.PausePlayback; {$ENDIF}
        {$IFDEF USEBASS} BASS_ChannelPause(fBassStream); {$ENDIF}
      end;
      if fFadeState = fsFadeIn then fFadeState := fsNone;
    end;
    //Start playback of other file half way through the fade
    if (fFadeState = fsFadeOut) and (GetTimeSince(fFadeStarted) > FADE_TIME div 2)
    and (fToPlayAfterFade <> '') then
    begin
      fFadedToPlayOther := True;
      PlayOtherFile(fToPlayAfterFade);
      fToPlayAfterFade := '';
    end;
  end;

  if fFadedToPlayOther and (fFadeState = fsFaded) and IsOtherEnded then
  begin
    fFadedToPlayOther := False;
    UnfadeMusic(nil);
  end;
end;


procedure TMusicLib.PauseMusicToPlayFile(aFileName: string; aVolume: single);
begin
  fOtherVolume := aVolume;
  if fFadeState in [fsNone, fsFadeIn] then
  begin
    FadeMusic(nil);
    fToPlayAfterFade := aFilename
  end
  else
    if (fFadeState = fsFaded) or ((fFadeState = fsFadeOut) and fFadedToPlayOther) then
    begin
      fFadedToPlayOther := True;
      PlayOtherFile(aFilename) //Switch playback immediately
    end
    else
      fToPlayAfterFade := aFilename; //We're still in the process of fading out, the file hasn't started yet
end;


procedure TMusicLib.StopPlayingOtherFile;
begin
  if not IsMusicInitialized then exit;
  {$IFDEF USELIBZPLAY} ZPlayerOther.StopPlayback; {$ENDIF}
  {$IFDEF USEBASS} BASS_ChannelStop(fBassOtherStream); {$ENDIF}
  fToPlayAfterFade := '';
  if fFadeState = fsFadeOut then fFadedToPlayOther := True; //Make sure the music starts again if we are currently fading out
end;


function TMusicLib.GetTrackTitle: string;
begin
  if not IsMusicInitialized then Exit;
  if not InRange(MusicIndex, Low(MusicTracks), High(MusicTracks)) then Exit;

  Result := TruncateExt(ExtractFileName(MusicTracks[TrackOrder[MusicIndex]]));
end;


(*
//Doesn't work unless you change volume in Windows?
s:= ExeDir + 'Music\SpiritOrig.mid';
{PlayMidiFile(s);
{StartSound(Form1.Handle, s);}
MCISendString(PChar('play ' + s), nil, 0, 0);}
*)


(*
function PlayMidiFile(FileName:string):word;
var
  wdeviceid: integer;
  mciOpen: tmci_open_parms;
  mciPlay: tmci_play_parms;
  mciStat: tmci_status_parms;
begin
  // Open the device by specifying the device and filename.
  // MCI will attempt to choose the MIDI mapper as the output port.
  mciopen.lpstrDeviceType := 'sequencer';
  mciopen.lpstrElementName := pchar (filename);
  Result := mciSendCommand ($0, mci_open , mci_open_type or mci_open_element, longint (@mciopen));
  if Result <> 0 then exit;

  // The device opened successfully; get the device ID.
  // Check if the output port is the MIDI mapper.
  wDeviceID := mciOpen.wDeviceID;
  mciStat.dwItem := MCI_SEQ_STATUS_PORT;
  Result := mciSendCommand (wDeviceID, MCI_STATUS, MCI_STATUS_ITEM, longint (@mciStat));
  if Result <> 0 then
  begin
    mciSendCommand (wDeviceID, MCI_CLOSE, 0, 0);
    exit;
  end;

  // Begin playback. The window procedure function for the parent
  // Window will be notified with an MM_MCINOTIFY message when
  // Playback is complete. At this time, the window procedure closes
  // The device.
  mciPlay.dwCallback := Form1.Handle;
  Result := mciSendCommand (wDeviceID, MCI_PLAY,
  MCI_NOTIFY, longint (@mciPlay));
  if Result <> 0 then
  begin
    mciSendCommand (wDeviceID, MCI_CLOSE, 0, 0);
    exit;
  end;
end;
*)


end.
