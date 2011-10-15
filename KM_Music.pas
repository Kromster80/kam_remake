unit KM_Music;
{$I KaM_Remake.inc}
interface
uses Forms, Bass, Classes, SysUtils, KromUtils, Math, KM_Defaults;

//We use the Bass library for music playback. It requires bass.dll to be distributed with our releases.

type
  TMusicLib = class
  private
    MusicCount:integer;
    MusicIndex:integer;
    MusicTracks:array[1..256]of string;
    //MIDICount,MIDIIndex:integer;
    //MIDITracks:array[1..256]of string;
    IsMusicInitialized:boolean;
    MusicGain:single;
    fBassStream:cardinal;
    function  CheckMusicError:boolean;
    function  PlayMusicFile(FileName:string):boolean;
    procedure ScanMusicTracks(Path:string);
  public
    constructor Create(aVolume:single);
    destructor Destroy; override;
    procedure UpdateMusicVolume(Value:single);
    procedure PlayMenuTrack;
    procedure PlayNextTrack;
    procedure PlayPreviousTrack;
    function IsMusicEnded:boolean;
    procedure StopMusic;
    procedure ToggleMusic(aOn:boolean);
    function GetTrackTitle:string;
  end;


implementation
uses
  KM_Game, KM_Log;


{Music Lib}
constructor TMusicLib.Create(aVolume:single);
begin
  Inherited Create;
  IsMusicInitialized := true;
  ScanMusicTracks(ExeDir + 'Music\');

  // Setup output - default device, 44100hz, stereo, 16 bits
  if not BASS_Init(-1, 44100, 0, 0, nil) then
  begin
    fLog.AppendLog('Failed to initialize the music playback device');
    IsMusicInitialized := false;
  end;

  UpdateMusicVolume(aVolume);

  fLog.AppendLog('Music init done, '+inttostr(MusicCount)+' tracks found');
end;


destructor TMusicLib.Destroy;
begin
  BASS_Stop(); //Stop all Bass output
  BASS_StreamFree(fBassStream); //Free the stream we may have used (will just return false if the stream is invalid)
  BASS_Free(); //Frees this usage of BASS, allowing it to be recreated successfully
  Inherited;
end;



function TMusicLib.CheckMusicError:boolean;
var ErrorCode: integer;
begin
  ErrorCode := BASS_ErrorGetCode();
  Result := ErrorCode <> BASS_OK;
end;


function TMusicLib.PlayMusicFile(FileName:string):boolean;
begin
  Result:=false;
  if not IsMusicInitialized then exit;

  BASS_ChannelStop(fBassStream); //Cancel previous sound
  if not FileExists(FileName) then exit; //Make it silent

  BASS_StreamFree(fBassStream); //Free the existing stream (will just return false if the stream is invalid)
  fBassStream := BASS_StreamCreateFile(FALSE, PChar(FileName), 0, 0, BASS_STREAM_AUTOFREE);

  BASS_ChannelPlay(fBassStream,true); //Start playback from the beggining
  UpdateMusicVolume(MusicGain); //Need to reset music volume after starting playback
  if CheckMusicError then exit;
  Result:=true;
end;


{Update music gain (global volume for all sounds/music)}
procedure TMusicLib.UpdateMusicVolume(Value:single);
begin
  if not IsMusicInitialized then exit; //Keep silent
  MusicGain := Value;
  BASS_ChannelSetAttribute(fBassStream, BASS_ATTRIB_VOL, Value); //0=silent, 1=max
end;


procedure TMusicLib.ScanMusicTracks(Path:string);
var SearchRec:TSearchRec;
begin
  if not IsMusicInitialized then exit;
  MusicCount:=0;
  if not DirectoryExists(Path) then exit;

  ChDir(Path);
  FindFirst('*', faDirectory, SearchRec);
  repeat
    if (SearchRec.Attr and faDirectory <> faDirectory)and(SearchRec.Name<>'.')and(SearchRec.Name<>'..') then
    if (GetFileExt(SearchRec.Name) = 'MP3') or //Allow all formats supported by Bass
       (GetFileExt(SearchRec.Name) = 'MP2') or
       (GetFileExt(SearchRec.Name) = 'MP1') or
       (GetFileExt(SearchRec.Name) = 'WAV') or
       (GetFileExt(SearchRec.Name) = 'OGG') or
       (GetFileExt(SearchRec.Name) = 'AIFF') then begin
      inc(MusicCount);
      MusicTracks[MusicCount] := Path + SearchRec.Name;
    end;
    {if GetFileExt(SearchRec.Name)='MID' then begin
      inc(MIDICount);
      MIDITracks[MIDICount] := Path + SearchRec.Name;
    end;}
  until (FindNext(SearchRec)<>0);
  FindClose(SearchRec);
  MusicIndex:=0;
end;


procedure TMusicLib.PlayMenuTrack;
begin
  if not IsMusicInitialized then exit;
  if MusicIndex = 1 then exit; //It's already playing
  MusicIndex := 1; //First track (Spirit) is always menu music
  PlayMusicFile(MusicTracks[MusicIndex]);
end;


procedure TMusicLib.PlayNextTrack;
begin
  if not IsMusicInitialized then exit;
  if not fGame.GlobalSettings.MusicOn then exit;
  if MusicCount=0 then exit; //no music files found
  MusicIndex := MusicIndex mod MusicCount + 1; //Set next index, looped
  PlayMusicFile(MusicTracks[MusicIndex]);
end;


procedure TMusicLib.PlayPreviousTrack;
begin
  if not IsMusicInitialized then exit;
  if not fGame.GlobalSettings.MusicOn then exit;
  if MusicCount=0 then exit; //no music files found
  MusicIndex := MusicIndex - 1; //Set to previous
  if MusicIndex = 0 then MusicIndex := MusicCount; //Loop to the top
  PlayMusicFile(MusicTracks[MusicIndex]);
end;


//Check if Music is not playing, to know when new mp3 should be feeded
function TMusicLib.IsMusicEnded:boolean;
begin
  Result := (not IsMusicInitialized) or (BASS_ChannelIsActive(fBassStream) = BASS_ACTIVE_STOPPED);
end;


procedure TMusicLib.StopMusic;
begin
  if not IsMusicInitialized then exit;
  BASS_ChannelStop(fBassStream);
  MusicIndex := 0;
end;


procedure TMusicLib.ToggleMusic(aOn:boolean);
begin
  if aOn then
    PlayMenuTrack //Start with the default track
  else
    StopMusic;
end;


function TMusicLib.GetTrackTitle:string;
begin
  if not IsMusicInitialized then exit;
  if not InRange(MusicIndex, low(MusicTracks), high(MusicTracks)) then exit;
  //May not display the correct title as not all LIBs are correct. Should also do range checking
  //Result := fTextLibrary.GetTextString(siTrackNames+MusicIndex);

  Result := TruncateExt(ExtractFileName(MusicTracks[MusicIndex])); //@Lewin: I think we should do it this way eventually
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
  if Result <> 0 then begin
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
  if Result <> 0 then begin
    mciSendCommand (wDeviceID, MCI_CLOSE, 0, 0);
    exit;
  end;
end;
*)


end.
