unit KM_Music;
{$I KaM_Remake.inc}
interface
uses Forms,
  {$IFDEF WDC}MMSystem, Windows, MPlayer, {$ENDIF}
  Classes, SysUtils, KromUtils, Math, KM_Defaults, KM_CommonTypes;

type
  TMusicLib = class(TObject)
  private
    {$IFDEF WDC} MediaPlayer: TMediaPlayer; {$ENDIF}
    MusicCount,MusicIndex:integer;
    MusicTracks:array[1..256]of string;
    //MIDICount,MIDIIndex:integer;
    //MIDITracks:array[1..256]of string;
    IsMusicInitialized:boolean;
    MusicGain:single;
    function CheckMusicError():boolean;
    function PlayMusicFile(FileName:string):boolean;
  public
    constructor Create();
    destructor Destroy(); override;
    procedure UpdateMusicVolume(Value:single);
    procedure ScanMusicTracks(Path:string);
    procedure PlayMenuTrack(JustInit:boolean);
    procedure PlayNextTrack();
    procedure PlayPreviousTrack();
    function IsMusicEnded():boolean;
    procedure StopMusic;
    function GetTrackTitle:string;
  end;


implementation
uses
  KM_Unit1, {Access to MediaPlayer?} //todo:reference MP through .Create
  KM_Game;


{Music Lib}
constructor TMusicLib.Create();
begin
  Inherited Create;
  IsMusicInitialized := true;
  ScanMusicTracks(ExeDir + 'Music\');
  {$IFDEF WDC} MediaPlayer := Form1.MediaPlayer1; {$ENDIF}
  //IsMusicInitialized := MediaPlayer.DeviceID <> 0; //Is this true, that if there's no soundcard then DeviceID = -1 ? I doubt..
  fLog.AppendLog('Music init done, '+inttostr(MusicCount)+' mp3 tracks found');
end;


destructor TMusicLib.Destroy();
begin
  //MediaPlayer.Close;
  //FreeAndNil(MediaPlayer);
  Inherited;
end;



function TMusicLib.CheckMusicError():boolean;
begin
  Result := false;
  {$IFDEF WDC}
  if MediaPlayer.Error<>0 then begin
    fLog.AddToLog(MediaPlayer.errormessage);
   // Application.MessageBox(@(MediaPlayer.errormessage)[1],'MediaPlayer error', MB_OK + MB_ICONSTOP);
   // IsMusicInitialized := false;
   // Result:=true; //Error is there
  end;
  {$ENDIF}
end;


function TMusicLib.PlayMusicFile(FileName:string):boolean;
begin
  Result:=false;
  {$IFDEF WDC}
  if not IsMusicInitialized then exit;

  if MediaPlayer.FileName<>'' then
  MediaPlayer.Close; //Cancel previous sound
  if CheckMusicError then exit;
  if not CheckFileExists(FileName,true) then exit; //Make it silent
  if GetFileExt(FileName)<>'MP3' then exit;
  MediaPlayer.FileName:=FileName;
  MediaPlayer.DeviceType:=dtAutoSelect; //Plays mp3's only in this mode, which works only if file extension is 'mp3'
  //Application.MessageBox(@FileName[1],'Now playing',MB_OK);
  MediaPlayer.Open; //Needs to be done for every new file
  if CheckMusicError then exit;
  UpdateMusicVolume(MusicGain); //Need to reset music volume after Open
  if CheckMusicError then exit;
  MediaPlayer.Play; //Start actual playback
  if CheckMusicError then exit;
  Result:=true;
  {$ENDIF}
end;


{Update music gain (global volume for all sounds/music)}
procedure TMusicLib.UpdateMusicVolume(Value:single);
{$IFDEF WDC}
const
  MCI_SETAUDIO = $0873;
  MCI_DGV_SETAUDIO_VOLUME = $4002;
  MCI_DGV_SETAUDIO_ITEM = $00800000;
  MCI_DGV_SETAUDIO_VALUE = $01000000;
var
  P:record
  dwCallback: DWORD;
  dwItem: DWORD;
  dwValue: DWORD;
  dwOver: DWORD;
  lpstrAlgorithm: PChar;
  lpstrQuality: PChar;
  end;
{$ENDIF}
begin
  {$IFDEF WDC}
  if not IsMusicInitialized then exit; //Keep silent
  MusicGain:=Value;
  P.dwCallback := 0;
  P.dwItem := MCI_DGV_SETAUDIO_VOLUME;
  P.dwValue := round(Value*1000);
  P.dwOver := 0;
  P.lpstrAlgorithm := nil;
  P.lpstrQuality := nil;
  mciSendCommand(MediaPlayer.DeviceID, MCI_SETAUDIO, MCI_DGV_SETAUDIO_VALUE or MCI_DGV_SETAUDIO_ITEM, Cardinal(@P)) ;
  {$ENDIF}
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
    if GetFileExt(SearchRec.Name)='MP3' then begin
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


procedure TMusicLib.PlayMenuTrack(JustInit:boolean);
begin
  if not IsMusicInitialized then exit;
  if MusicIndex = 1 then exit; //Don't change unless needed
  MusicIndex := 1; //First track (Spirit) is always menu music
  PlayMusicFile(MusicTracks[MusicIndex]);
  if JustInit then StopMusic; //This way music gets initialized irregardless of On/Off
                              //switch state on game launch. This means there's no 2sec
                              //lag when enabling music that was set to Off.
  //BUG: Attempt to play MPEG 1.0 Layer 3 silently crashes Remake on some PCs
end;


procedure TMusicLib.PlayNextTrack();
begin
  if not IsMusicInitialized then exit;
  if not fGame.fGlobalSettings.IsMusic then exit;
  if MusicCount=0 then exit; //no music files found
  MusicIndex := MusicIndex mod MusicCount + 1; //Set next index, looped
  PlayMusicFile(MusicTracks[MusicIndex]);
end;


procedure TMusicLib.PlayPreviousTrack();
begin
  if not IsMusicInitialized then exit;
  if not fGame.fGlobalSettings.IsMusic then exit;
  if MusicCount=0 then exit; //no music files found
  MusicIndex := MusicIndex - 1; //Set to previous
  if MusicIndex = 0 then MusicIndex := MusicCount; //Loop to the top
  PlayMusicFile(MusicTracks[MusicIndex]);
end;


//Check if Music is not playing, to know when new mp3 should be feeded
function TMusicLib.IsMusicEnded():boolean;
begin
  Result:=false;
  {$IFDEF WDC}
  if not IsMusicInitialized then exit;
  if fGame.fGlobalSettings.IsMusic then begin
    Result := ((MediaPlayer.Mode=mpStopped)or(MediaPlayer.FileName=''));
    if CheckMusicError then exit;
  end;
  {$ENDIF}
end;


procedure TMusicLib.StopMusic;
begin
  {$IFDEF WDC}
  if not IsMusicInitialized then exit;
  MediaPlayer.Close;
  //if CheckMusicError then exit;
  MediaPlayer.FileName:='';
  //if CheckMusicError then exit;
  MusicIndex := 0;
  {$ENDIF}
end;


function TMusicLib.GetTrackTitle:string;
begin
  if not IsMusicInitialized then exit;
  if not InRange(MusicIndex, low(MusicTracks), high(MusicTracks)) then exit;
  //May not display the correct title as not all LIBs are correct. Should also do range checking
  //Result := fTextLibrary.GetTextString(siTrackNames+MusicIndex);

  Result := ExtractFileName(MusicTracks[MusicIndex]); //@Lewin: I think we should do it this way eventually
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
