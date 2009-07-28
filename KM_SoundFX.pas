unit KM_SoundFX;
interface
uses Forms, Windows, MMSystem, MPlayer, Classes, SysUtils, KromUtils, OpenAL, Math, KM_Defaults, KM_CommonTypes, KM_Utils;

const MaxWaves = 200;
const MaxSourceCount = 16; //Actually it depends on hardware

type
  TWAVHeaderEx = record
    RIFFHeader: array [1..4] of Char;
    FileSize: Integer;
    WAVEHeader: array [1..4] of Char;
    FormatHeader: array [1..4] of Char;
    FormatHeaderSize: Integer;
    FormatCode: Word;
    ChannelNumber: Word;
    SampleRate: Integer;
    BytesPerSecond: Integer;
    BytesPerSample: Word;
    BitsPerSample: Word;
    DATAHeader: array [1..4] of Char; //Extension
    DataSize: Integer; //Extension
  end;

type
  TSoundLib = class(TObject)
  private
    MediaPlayer: TMediaPlayer;
    Waves: array[1..MaxWaves] of record
      Head: TWAVHeaderEx;
      Data: array of char;
      Foot: array of char;
    end;
    Props: array[1..MaxWaves] of packed record
      SampleRate,Volume,a,b:integer;
      i,j,k,l,Index:word;
    end;
    Listener:record
      Pos: array [1..3] of TALfloat; //Position in 3D space
      Vel: array [1..3] of TALfloat; //Velocity, used in doppler effect calculation
      Ori: array [1..6] of TALfloat; //Orientation LookingAt and UpVector
    end;
    MusicCount,MusicIndex:integer;
    MusicTracks:array[1..256]of string;
    IsOpenALInitialized:boolean;
    IsMusicInitialized:boolean;
    //Buffer used to store the wave data, Source is sound position in space
    ALSource,ALBuffer: array [1..64] of TALuint;
    SoundGain,MusicGain:single;
    procedure LoadSoundsDAT();
    function CheckMusicError():boolean;
  public
    constructor Create();
    destructor Destroy(); override;
    procedure ExportSounds();
    procedure UpdateListener(Pos:TKMPoint);
    procedure UpdateSFXVolume(Value:single);
    procedure UpdateMusicVolume(Value:single);
    procedure Play(SoundID:TSoundFX; const Volume:single=1.0); overload;
    procedure Play(SoundID:TSoundFX; Loc:TKMPoint; const Attenuated:boolean=true; const Volume:single=1.0); overload;
    procedure ScanMusicTracks(Path:string);
    procedure PlayMenuTrack();
    procedure PlayNextTrack();
    procedure PlayPreviousTrack();
    function IsMusicEnded():boolean;
    function PlayMusicFile(FileName:string):boolean;
    procedure StopMusic;
    function GetTrackTitle:string;
end;

var
  fSoundLib: TSoundLib;

implementation
uses
  KM_Settings, KM_LoadLib, KM_Unit1;


constructor TSoundLib.Create();
var
  Context: PALCcontext;
  Device: PALCdevice;
  ErrCode:integer;
begin
  Inherited Create;

  IsOpenALInitialized := true;
  IsMusicInitialized := true;

  MediaPlayer := Form1.MediaPlayer1;
  IsMusicInitialized := MediaPlayer.DeviceID <> 0; //Is this true, that if there's no soundcard then DeviceID = -1 ? I doubt..
  ScanMusicTracks(ExeDir + 'Music\');

  IsOpenALInitialized := InitOpenAL;
  if not IsOpenALInitialized then begin
    fLog.AddToLog('OpenAL warning. OpenAL could not be initialized.');
    Application.MessageBox('OpenAL could not be initialized. Please refer to Readme.txt for solution','OpenAL warning', MB_OK + MB_ICONEXCLAMATION);
    IsOpenALInitialized := false;
    exit;
  end;

  //Open device
  Device := alcOpenDevice(nil); // this is supposed to select the "preferred device"
  if Device = nil then begin
    fLog.AddToLog('OpenAL warning. Device could not be opened.');
    Application.MessageBox('Device could not be opened. Please refer to Readme.txt for solution','OpenAL warning', MB_OK + MB_ICONEXCLAMATION);
    IsOpenALInitialized := false;
    exit;
  end;

  //Create context(s)
  Context := alcCreateContext(Device, nil);  
  if Context = nil then begin
    fLog.AddToLog('OpenAL warning. Context could not be created.');
    Application.MessageBox('Context could not be created. Please refer to Readme.txt for solution','OpenAL warning', MB_OK + MB_ICONEXCLAMATION);
    IsOpenALInitialized := false;
    exit;
  end;

  //Set active context
  if alcMakeContextCurrent(Context) > 1 then begin //valid returns are AL_NO_ERROR=0 and AL_TRUE=1
    fLog.AddToLog('OpenAL warning. Context could not be made current.');
    Application.MessageBox('Context could not be made current. Please refer to Readme.txt for solution','OpenAL warning', MB_OK + MB_ICONEXCLAMATION);
    IsOpenALInitialized := false;
    exit;
  end;

  ErrCode:=alcGetError(Device);
  if ErrCode <> ALC_NO_ERROR then begin
    fLog.AddToLog('OpenAL warning. There is OpenAL error '+inttostr(ErrCode)+' raised. Sound will be disabled.');
    Application.MessageBox(@('There is OpenAL error '+inttostr(ErrCode)+' raised. Sound will be disabled.')[1],'OpenAL error', MB_OK + MB_ICONEXCLAMATION);
    IsOpenALInitialized := false;
    exit;
  end;

  //Set attenuation model
  alDistanceModel(AL_LINEAR_DISTANCE_CLAMPED);
  fLog.AppendLog('Pre-LoadSFX init',true);
  
  LoadSoundsDAT();
  fLog.AppendLog('Load Sounds.dat',true);
  AlGenBuffers(MaxSourceCount, @ALBuffer); //64 looks like the limit, depends on hardware
  AlGenSources(MaxSourceCount, @ALSource);
  //Set default Listener orientation
  Listener.Ori[1]:=0; Listener.Ori[2]:=1; Listener.Ori[3]:=0; //Look-at vector
  Listener.Ori[4]:=0; Listener.Ori[5]:=0; Listener.Ori[6]:=1; //Up vector
  AlListenerfv ( AL_ORIENTATION, @Listener.Ori);
end;


destructor TSoundLib.Destroy();
begin
  //MediaPlayer.Close;
  //FreeAndNil(MediaPlayer);

  AlDeleteBuffers(MaxSourceCount, @ALBuffer);
  AlDeleteSources(MaxSourceCount, @ALSource);
  AlutExit();

  Inherited;  
end;


function TSoundLib.CheckMusicError():boolean;
begin
  Result:=false;
  if MediaPlayer.Error<>0 then begin
    fLog.AddToLog(MediaPlayer.errormessage);
   // Application.MessageBox(@(MediaPlayer.errormessage)[1],'MediaPlayer error', MB_OK + MB_ICONSTOP);
   // IsMusicInitialized := false;
   // Result:=true; //Error is there
  end;
end;


procedure TSoundLib.LoadSoundsDAT();
var
  f:file;
  Head:record Size,Count:word; end;
  Tab1:array[1..200]of integer;
  Tab2:array[1..200]of smallint;
  i,Tmp:integer;
  c: array[1..20] of char;
begin
  if not IsOpenALInitialized then exit;
  if not CheckFileExists(ExeDir+'data\sfx\sounds.dat') then exit;
  AssignFile(f, ExeDir+'data\sfx\sounds.dat'); Reset(f,1);

  BlockRead(f, Head, 4); //Read 4 bytes into Head record
  BlockRead(f, Tab1, Head.Count*4); //Read Count*4bytes into Tab1(WaveSizes)
  BlockRead(f, Tab2, Head.Count*2); //Read Count*2bytes into Tab2(No idea what is it)

  for i:=1 to Head.Count do begin
    BlockRead(f,Tmp,4); //Always '1' for existing waves
    if Tab1[i]<>0 then begin
      BlockRead(f,Waves[i].Head,SizeOf(Waves[i].Head));
      setlength(Waves[i].Data,Waves[i].Head.DataSize);
      BlockRead(f,Waves[i].Data[0],Waves[i].Head.DataSize);
      setlength(Waves[i].Foot,Tab1[i]-SizeOf(Waves[i].Head)-Waves[i].Head.DataSize);
      BlockRead(f,Waves[i].Foot[0],Tab1[i]-SizeOf(Waves[i].Head)-Waves[i].Head.DataSize);
    end;
  end;

  BlockRead(f,c,20);
  BlockRead(f,Props[1],26*Head.Count);

  CloseFile(f);
end;


procedure TSoundLib.ExportSounds();
var f:file; i:integer;
begin
  if not IsOpenALInitialized then exit;
  CreateDir(ExeDir+'Export\');
  CreateDir(ExeDir+'Export\Sounds.dat\');
  for i:=1 to MaxWaves do if length(Waves[i].Data)>0 then begin
    assignfile(f,ExeDir+'Export\Sounds.dat\sound_'+int2fix(i,3)+'.wav'); rewrite(f,1);
    blockwrite(f,Waves[i].Head,SizeOf(Waves[i].Head));
    blockwrite(f,Waves[i].Data[0],length(Waves[i].Data));
    blockwrite(f,Waves[i].Foot[0],length(Waves[i].Foot));
    closefile(f);
  end;
end;


{Update listener position in 3D space}
procedure TSoundLib.UpdateListener(Pos:TKMPoint);
begin
  if not IsOpenALInitialized then exit;
  Listener.Pos[1]:=Pos.X;
  Listener.Pos[2]:=Pos.Y;
  Listener.Pos[3]:=12; //Place Listener above the surface
  AlListenerfv ( AL_POSITION, @Listener.Pos);
end;


{Update sound gain (global volume for all sounds/music)}
procedure TSoundLib.UpdateSFXVolume(Value:single);
begin
  if not IsOpenALInitialized then exit;
  SoundGain:=Value;
//  alListenerf ( AL_GAIN, SoundGain );
end;


{Update music gain (global volume for all sounds/music)}
procedure TSoundLib.UpdateMusicVolume(Value:single);
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
begin
  if not IsMusicInitialized then exit; //Keep silent
  MusicGain:=Value;
  P.dwCallback := 0;
  P.dwItem := MCI_DGV_SETAUDIO_VOLUME;
  P.dwValue := round(Value*1000);
  P.dwOver := 0;
  P.lpstrAlgorithm := nil;
  P.lpstrQuality := nil;
  mciSendCommand(MediaPlayer.DeviceID, MCI_SETAUDIO, MCI_DGV_SETAUDIO_VALUE or MCI_DGV_SETAUDIO_ITEM, Cardinal(@P)) ;
end;  


{Wrapper with fewer options for non-attenuated sounds}
procedure TSoundLib.Play(SoundID:TSoundFX; const Volume:single=1.0);
begin
  if not IsOpenALInitialized then exit;
  Play(SoundID, KMPoint(0,0), false, Volume);
end;


{Call to this procedure will find free spot and start to play sound immediately}
{Will need to make another one for unit sounds, which will take WAV file path as parameter}
{Attenuated means if sound should fade over distance or not}
procedure TSoundLib.Play(SoundID:TSoundFX; Loc:TKMPoint; const Attenuated:boolean=true; const Volume:single=1.0);
var Dif:array[1..3]of single; FreeBuf,ID:integer; i:integer; ALState:TALint;
begin
  if not IsOpenALInitialized then exit;

  //fLog.AddToLog('SoundPlay ID'+inttostr(byte(SoundID))+' at '+TypeToString(Loc));

  //Here should be some sort of RenderQueue/List/Clip

  //Find free buffer and use it
  FreeBuf:=1;
  for i:=1 to MaxSourceCount do begin
    alGetSourcei(ALSource[i], AL_SOURCE_STATE, @ALState);
    if ALState<>AL_PLAYING then begin
      FreeBuf:=i;
      break;
    end;
  end;

  //fLog.AddToLog('Assigned buffer '+inttostr(FreeBuf));

  if i>=MaxSourceCount then exit;//Don't play if there's no room left, will need to replace with better scheme sometime

  ID:=word(SoundID);

  //Stop previously playing sound and release buffer
  AlSourceStop(ALSource[FreeBuf]);
  AlSourcei ( ALSource[FreeBuf], AL_BUFFER, 0);

  //Assign new data to buffer and assign it to source
  AlBufferData(ALBuffer[FreeBuf], AL_FORMAT_MONO8, @Waves[ID].Data[0], Waves[ID].Head.DataSize, Waves[ID].Head.SampleRate);

  //Set source properties
  AlSourcei ( ALSource[FreeBuf], AL_BUFFER, ALBuffer[FreeBuf]);
  AlSourcef ( ALSource[FreeBuf], AL_PITCH, 1.0 );
  AlSourcef ( ALSource[FreeBuf], AL_GAIN, 1.0 * Volume * SoundGain);
  if Attenuated then begin
    Dif[1]:=Loc.X; Dif[2]:=Loc.Y; Dif[3]:=0;
    AlSourcefv( ALSource[FreeBuf], AL_POSITION, @Dif[1]);
  end else
    AlSourcefv( ALSource[FreeBuf], AL_POSITION, @Listener.Pos);
  AlSourcef ( ALSource[FreeBuf], AL_REFERENCE_DISTANCE, 4.0 );
  AlSourcef ( ALSource[FreeBuf], AL_MAX_DISTANCE, 20.0 );
  AlSourcef ( ALSource[FreeBuf], AL_ROLLOFF_FACTOR, 1.0 );
  AlSourcei ( ALSource[FreeBuf], AL_LOOPING, AL_FALSE );

  //Start playing
  AlSourcePlay(ALSource[FreeBuf]);
end;


{Here goes Music lib}


procedure TSoundLib.ScanMusicTracks(Path:string);
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
      MusicTracks[MusicCount]:=Path+SearchRec.Name;
    end;
  until (FindNext(SearchRec)<>0);
  FindClose(SearchRec);
  MusicIndex:=0;
end;


procedure TSoundLib.StopMusic;
begin
  if not IsMusicInitialized then exit;
  MediaPlayer.Close;
  //if CheckMusicError then exit;
  //MediaPlayer.FileName:='';
  //if CheckMusicError then exit;
  MusicIndex := 0;
end;


function TSoundLib.GetTrackTitle:string;
begin
  if not IsMusicInitialized then exit;
  //May not display the correct title as not all LIBs are correct. Should also do range checking
  if InRange(MusicIndex,1,256) then
    Result := fTextLibrary.GetTextString(siTrackNames+MusicIndex)
  else Result := 'Unknown';

  Result:=ExtractFileName(MusicTracks[MusicIndex]); //@Lewin: I think we should do it this way eventually
end;


function TSoundLib.PlayMusicFile(FileName:string):boolean;
begin
  Result:=false;
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
end;


procedure TSoundLib.PlayMenuTrack();
begin
  if not IsMusicInitialized then exit;
  if MusicIndex = 1 then exit; //Don't change unless needed
  MusicIndex := 1; //First track (Spirit) is always menu music
  PlayMusicFile(MusicTracks[MusicIndex]);
  if not fGameSettings.IsMusic then StopMusic; //This way music gets initialized irregardless of On/Off
                                               //switch state on game launch. This means there's no 2sec
                                               //lag when enabling music that was set to Off.
  //BUG: Attempt to play MPEG 1.0 Layer 3 silently crashes Remake on some PCs
end;


procedure TSoundLib.PlayNextTrack();
begin
  if not IsMusicInitialized then exit;
  if not fGameSettings.IsMusic then exit;
  if MusicCount=0 then exit; //no music files found
  MusicIndex := MusicIndex mod MusicCount + 1; //Set next index, looped
  PlayMusicFile(MusicTracks[MusicIndex]);
end;


procedure TSoundLib.PlayPreviousTrack();
begin
  if not IsMusicInitialized then exit;
  if not fGameSettings.IsMusic then exit;
  if MusicCount=0 then exit; //no music files found
  MusicIndex := MusicIndex - 1; //Set to previous
  if MusicIndex = 0 then MusicIndex := MusicCount; //Loop to the top
  PlayMusicFile(MusicTracks[MusicIndex]);
end;

//Check if Music is not playing, to know when new mp3 should be feeded
function TSoundLib.IsMusicEnded():boolean;
begin
  Result:=false;
  if not IsMusicInitialized then exit;
  Result := fGameSettings.IsMusic and ((MediaPlayer.Mode=mpStopped)or(MediaPlayer.FileName=''));
  if CheckMusicError then exit;
end;


end.
