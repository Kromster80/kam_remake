unit KM_Sound;
{$I KaM_Remake.inc}
interface
uses Classes, Dialogs, Forms, SysUtils,
  {$IFDEF MSWindows} Windows, {$ENDIF}
  {$IFDEF Unix} LCLIntf, LCLType, {$ENDIF}
  OpenAL, KromUtils, KM_Defaults, KM_Points;


const
  MAX_SOUNDS = 16; //64 looks like the limit, depends on hardware
  MAX_ATTENUATED_SOUNDS = (3/4)*MAX_SOUNDS; //Attenuated sounds are less important, always save space for others
  MAX_FAR_SOUNDS = (1/2)*MAX_SOUNDS; //Sounds that are too far away can only access this many slots

type
  TWAVHeaderEx = record
    RIFFHeader: array [1..4] of AnsiChar;
    FileSize: Integer;
    WAVEHeader: array [1..4] of AnsiChar;
    FormatHeader: array [1..4] of AnsiChar;
    FormatHeaderSize: Integer;
    FormatCode: Word;
    ChannelNumber: Word;
    SampleRate: Integer;
    BytesPerSecond: Integer;
    BytesPerSample: Word;
    BitsPerSample: Word;
    DATAHeader: array [1..4] of AnsiChar; //Extension
    DataSize: Integer; //Extension
  end;


  TSoundLib = class
  private
    fALDevice: PALCdevice;
    fWavesCount:integer;
    fWaves: array of record
      Head: TWAVHeaderEx;
      Data: array of byte;
      Foot: array of byte;
      IsLoaded:boolean;
    end;
    fListener:record
      Pos: array [1..3] of TALfloat; //Position in 3D space
      Vel: array [1..3] of TALfloat; //Velocity, used in doppler effect calculation
      Ori: array [1..6] of TALfloat; //Orientation LookingAt and UpVector
    end;
    fIsSoundInitialized:boolean;
    //Buffer used to store the wave data, Source is sound position in space
    {Buffers:array [1..MAX_SOUNDS] of record
      ALBuffer:TALuint;
      RefCount:integer; //How many references do we have
      WaveID:integer; //Reference to wave
    end;
    Sources:array [1..MAX_SOURCES] of record
      ALSource:TALuint;
      BufferRef:integer; //Reference to Buffer
      Position:TKMPoint;
      PlaySince:cardinal;
    end;}

    fSound:array [1..MAX_SOUNDS] of record
      ALBuffer:TALuint;
      ALSource:TALuint;
      Name:string;
      Position:TKMPointF;
      Duration:cardinal; //MSec
      PlaySince:cardinal;
      FadesMusic:boolean;
    end;

    fSoundGain:single; //aka "Global volume"
    fMusicIsFaded:boolean;
    fLocale:string; //Locale used to access warrior sounds
    fNotificationSoundCount: array[TAttackNotification] of byte;
    fWarriorSoundCount: array[WARRIOR_MIN..WARRIOR_MAX, TWarriorSpeech] of byte;
    fWarriorUseBackup: array[WARRIOR_MIN..WARRIOR_MAX] of boolean;

    fOnFadeMusic:TNotifyEvent;
    fOnUnfadeMusic:TNotifyEvent;
    procedure CheckOpenALError;
    procedure LoadSoundsDAT;
    procedure ScanWarriorSounds;
    function LoadWarriorSoundsFromFile(const aFile: String): Boolean;
    procedure SaveWarriorSoundsToFile(const aFile: String);
    function WarriorSoundFile(aUnitType:TUnitType; aSound:TWarriorSpeech; aNumber:byte):string;
    function NotificationSoundFile(aSound:TAttackNotification; aNumber:byte):string;
    procedure PlayWave(const aFile:string; Loc:TKMPointF; Attenuated:boolean=true; Volume:single=1.0; FadeMusic:boolean=false); overload;
    procedure PlaySound(SoundID:TSoundFX; const aFile:string; Loc:TKMPointF; Attenuated:boolean=true; Volume:single=1.0; FadeMusic:boolean=false);
  public
    constructor Create(aLocale:string; aVolume:single);
    destructor Destroy; override;
    function ActiveCount:byte;

    property OnFadeMusic:TNotifyEvent write fOnFadeMusic;
    property OnUnfadeMusic:TNotifyEvent write fOnUnfadeMusic;

    procedure ExportSounds;
    procedure UpdateListener(X,Y:single);
    procedure UpdateSoundVolume(Value:single);

    procedure PlayNotification(aSound:TAttackNotification);

    procedure PlayCitizen(aUnitType:TUnitType; aSound:TWarriorSpeech); overload;
    procedure PlayCitizen(aUnitType:TUnitType; aSound:TWarriorSpeech; aLoc:TKMPointF); overload;
    procedure PlayWarrior(aUnitType:TUnitType; aSound:TWarriorSpeech); overload;
    procedure PlayWarrior(aUnitType:TUnitType; aSound:TWarriorSpeech; aLoc:TKMPointF); overload;
    procedure Play(SoundID:TSoundFX; Volume:single=1.0); overload;
    procedure Play(SoundID:TSoundFX; Loc:TKMPoint; Attenuated:boolean=true; Volume:single=1.0); overload;
    procedure Play(SoundID:TSoundFX; Loc:TKMPointF; Attenuated:boolean=true; Volume:single=1.0); overload;

    procedure Play(SoundID:TSoundFXNew; Volume:single=1.0; FadeMusic:boolean=false); overload;
    procedure Play(SoundID:TSoundFXNew; Loc:TKMPoint; Attenuated:boolean=true; Volume:single=1.0; FadeMusic:boolean=false); overload;

    procedure Paint;
    procedure UpdateStateIdle;
  end;


var
  fSoundLib: TSoundLib;


implementation
uses KM_CommonClasses, KM_RenderAux, KM_Log, KM_Locales;


const
  MAX_BUFFERS = 16; //16/24/32 looks like the limit, depends on hardware
  MAX_SOURCES = 32; //depends on hardware as well
  MAX_DISTANCE = 32; //After this distance sounds are completely mute
  MAX_PLAY_DISTANCE = (3/4)*MAX_DISTANCE; //In all my tests sounds are not audible at past this distance, OpenAL makes them too quiet
  MAX_PRIORITY_DISTANCE = (1/2)*MAX_DISTANCE; //Sounds past this distance will not play if there are few slots left (gives close sounds priority)

  WarriorSFXFolder: array[WARRIOR_MIN..WARRIOR_MAX] of string = (
    'militia', 'axeman', 'swordman', 'bowman', 'crossbowman',
    'lanceman', 'pikeman', 'cavalry', 'knights', 'barbarian',
    'rebel', 'rogue', 'warrior', 'vagabond');

  //TPR warriors reuse TSK voices in some languages, so if the specific ones don't exist use these
  WarriorSFXFolderBackup: array[WARRIOR_MIN..WARRIOR_MAX] of string = (
    '', '', '', '', '',
    '', '', '', '', '',
    'bowman', 'lanceman', 'barbarian', 'cavalry');

  WarriorSFX: array[TWarriorSpeech] of string = (
    'select', 'eat', 'left', 'right', 'halve',
    'join', 'halt', 'send', 'attack', 'format',
    'death', 'battle', 'storm');

  AttackNotifications: array[TAttackNotification] of string = ('citiz', 'town', 'units');

  CitizenSFX: array[CITIZEN_MIN..CITIZEN_MAX] of record
    WarriorVoice: TUnitType;
    SelectID, DeathID: byte;
  end = (
    (WarriorVoice: ut_Militia;      SelectID:3; DeathID:1), //ut_Serf
    (WarriorVoice: ut_AxeFighter;   SelectID:0; DeathID:0), //ut_Woodcutter
    (WarriorVoice: ut_Bowman;       SelectID:2; DeathID:1), //ut_Miner
    (WarriorVoice: ut_Swordsman;    SelectID:0; DeathID:2), //ut_AnimalBreeder
    (WarriorVoice: ut_Militia;      SelectID:1; DeathID:2), //ut_Farmer
    (WarriorVoice: ut_Arbaletman;   SelectID:1; DeathID:0), //ut_Lamberjack
    (WarriorVoice: ut_Pikeman;      SelectID:1; DeathID:0), //ut_Baker
    (WarriorVoice: ut_HorseScout;   SelectID:0; DeathID:2), //ut_Butcher
    (WarriorVoice: ut_Horseman;     SelectID:2; DeathID:0), //ut_Fisher
    (WarriorVoice: ut_Cavalry;      SelectID:1; DeathID:1), //ut_Worker
    (WarriorVoice: ut_Hallebardman; SelectID:1; DeathID:1), //ut_StoneCutter
    (WarriorVoice: ut_Cavalry;      SelectID:3; DeathID:4), //ut_Smith
    (WarriorVoice: ut_Hallebardman; SelectID:3; DeathID:2), //ut_Metallurgist
    (WarriorVoice: ut_Bowman;       SelectID:3; DeathID:0)  //ut_Recruit
    );


{ TSoundLib }
constructor TSoundLib.Create(aLocale:string; aVolume:single);
var
  Context: PALCcontext;
  i:integer;
  NumMono,NumStereo:TALCint;
begin
  Inherited Create;

  if DirectoryExists(ExeDir+'data\sfx\speech.'+aLocale+'\') then
    fLocale := aLocale
  else
    if DirectoryExists(ExeDir+'data\sfx\speech.'+fLocales.GetLocale(aLocale).FallbackLocale+'\') then
      fLocale := fLocales.GetLocale(aLocale).FallbackLocale //Use fallback local when primary doesn't exist
    else
      fLocale := DEFAULT_LOCALE; //Use English voices when no language specific voices exist

  fIsSoundInitialized := InitOpenAL;
  if not fIsSoundInitialized then begin
    fLog.AddToLog('OpenAL warning. OpenAL could not be initialized.');
    Application.MessageBox('OpenAL could not be initialized. Please refer to Readme.txt for solution','OpenAL warning', MB_OK + MB_ICONEXCLAMATION);
    fIsSoundInitialized := false;
    Exit;
  end;

  //Open device
  fALDevice := alcOpenDevice(nil); // this is supposed to select the "preferred device"
  if fALDevice = nil then begin
    fLog.AddToLog('OpenAL warning. Device could not be opened.');
    Application.MessageBox('Device could not be opened. Please refer to Readme.txt for solution','OpenAL warning', MB_OK + MB_ICONEXCLAMATION);
    fIsSoundInitialized := false;
    Exit;
  end;

  //Create context(s)
  Context := alcCreateContext(fALDevice, nil);
  if Context = nil then begin
    fLog.AddToLog('OpenAL warning. Context could not be created.');
    Application.MessageBox('Context could not be created. Please refer to Readme.txt for solution','OpenAL warning', MB_OK + MB_ICONEXCLAMATION);
    fIsSoundInitialized := false;
    Exit;
  end;

  //Set active context
  if alcMakeContextCurrent(Context) > 1 then begin //valid returns are AL_NO_ERROR=0 and AL_TRUE=1
    fLog.AddToLog('OpenAL warning. Context could not be made current.');
    Application.MessageBox('Context could not be made current. Please refer to Readme.txt for solution','OpenAL warning', MB_OK + MB_ICONEXCLAMATION);
    fIsSoundInitialized := false;
    Exit;
  end;

  CheckOpenALError;
  if not fIsSoundInitialized then Exit;

  //Set attenuation model
  alDistanceModel(AL_LINEAR_DISTANCE_CLAMPED);
  fLog.AppendLog('Pre-LoadSFX init', True);

  alcGetIntegerv(fALDevice, ALC_MONO_SOURCES, 4, @NumMono);
  alcGetIntegerv(fALDevice, ALC_STEREO_SOURCES, 4, @NumStereo);

  fLog.AppendLog('ALC_MONO_SOURCES',NumMono);
  fLog.AppendLog('ALC_STEREO_SOURCES',NumStereo);

  for i:=1 to MAX_SOUNDS do begin
    AlGenBuffers(1, @fSound[i].ALBuffer);
    AlGenSources(1, @fSound[i].ALSource);
  end;

  CheckOpenALError;
  if not fIsSoundInitialized then Exit;

  //Set default Listener orientation
  fListener.Ori[1]:=0; fListener.Ori[2]:=0; fListener.Ori[3]:=-1; //Look-at vector
  fListener.Ori[4]:=0; fListener.Ori[5]:=1; fListener.Ori[6]:=0; //Up vector
  AlListenerfv(AL_ORIENTATION, @fListener.Ori);
  fSoundGain := aVolume;

  fLog.AppendLog('OpenAL init done');

  LoadSoundsDAT;
  fLog.AppendLog('Load Sounds.dat',true);

  ScanWarriorSounds;
  fLog.AppendLog('Warrior sounds scanned',true);
end;


destructor TSoundLib.Destroy;
var i:integer;
begin
  if fIsSoundInitialized then
  begin
    for i:=1 to MAX_SOUNDS do begin
      AlDeleteBuffers(1, @fSound[i].ALBuffer);
      AlDeleteSources(1, @fSound[i].ALSource);
    end;
    AlutExit;
  end;
  Inherited;
end;


procedure TSoundLib.CheckOpenALError;
var ErrCode:integer;
begin
  ErrCode := alcGetError(fALDevice);
  if ErrCode <> ALC_NO_ERROR then begin
    fLog.AddToLog('OpenAL warning. There is OpenAL error '+inttostr(ErrCode)+' raised. Sound will be disabled.');
    Application.MessageBox(PChar('There is OpenAL error '+inttostr(ErrCode)+' raised. Sound will be disabled.'),'OpenAL error', MB_OK + MB_ICONEXCLAMATION);
    fIsSoundInitialized := false;
  end;
end;


procedure TSoundLib.LoadSoundsDAT;
var
  S:TMemoryStream;
  Head:record Size,Count:word; end;
  Tab1:array[1..200]of integer;
  Tab2:array[1..200]of smallint;
  i,Tmp:integer;
begin
  if not fIsSoundInitialized then Exit;
  if not CheckFileExists(ExeDir+'data\sfx\sounds.dat') then Exit;

  S := TMemoryStream.Create;
  S.LoadFromFile(ExeDir + 'data\sfx\sounds.dat');
  S.Read(Head, 4);
  S.Read(Tab1, Head.Count*4); //Read Count*4bytes into Tab1(WaveSizes)
  S.Read(Tab2, Head.Count*2); //Read Count*2bytes into Tab2(No idea what is it)

  fWavesCount := Head.Count;
  SetLength(fWaves, fWavesCount+1);

  for i:=1 to Head.Count do begin
    S.Read(Tmp, 4); //Always '1' for existing waves
    if Tab1[i]<>0 then begin
      S.Read(fWaves[i].Head, SizeOf(fWaves[i].Head));
      SetLength(fWaves[i].Data, fWaves[i].Head.DataSize);
      S.Read(fWaves[i].Data[0], fWaves[i].Head.DataSize);
      SetLength(fWaves[i].Foot, Tab1[i]-SizeOf(fWaves[i].Head)-fWaves[i].Head.DataSize);
      S.Read(fWaves[i].Foot[0], Tab1[i]-SizeOf(fWaves[i].Head)-fWaves[i].Head.DataSize);
    end;
    fWaves[i].IsLoaded := True;
  end;

  {BlockRead(f,c,20);
  //Packed record
  //SampleRate,Volume,a,b:integer;
  //i,j,k,l,Index:word;
  BlockRead(f,Props[1],26*Head.Count);}

  S.Free;
end;


procedure TSoundLib.ExportSounds;
var
  i: Integer;
  S: TMemoryStream;
begin
  if not fIsSoundInitialized then Exit;

  ForceDirectories(ExeDir + 'Export\SoundsDat\');

  for i:=1 to fWavesCount do
  if Length(fWaves[i].Data) > 0 then
  begin
    S := TMemoryStream.Create;
    S.Write(fWaves[i].Head, SizeOf(fWaves[i].Head));
    S.Write(fWaves[i].Data[0], Length(fWaves[i].Data));
    S.Write(fWaves[i].Foot[0], Length(fWaves[i].Foot));
      S.SaveToFile(ExeDir + 'Export\SoundsDat\sound_' + int2fix(i, 3) + '_' + SSoundFX[TSoundFX(i)] + '.wav');
    S.Free;
  end;
end;


{Update listener position in 3D space}
procedure TSoundLib.UpdateListener(X,Y:single);
begin
  if not fIsSoundInitialized then Exit;
  fListener.Pos[1] := X;
  fListener.Pos[2] := Y;
  fListener.Pos[3] := 24; //Place Listener above the surface
  AlListenerfv(AL_POSITION, @fListener.Pos);
end;


{ Update sound gain (global volume for all sounds) }
procedure TSoundLib.UpdateSoundVolume(Value:single);
begin
  if not fIsSoundInitialized then Exit;
  fSoundGain := Value;
  //alListenerf(AL_GAIN, fSoundGain); //Set in source property
end;


{Wrapper with fewer options for non-attenuated sounds}
procedure TSoundLib.Play(SoundID:TSoundFX; Volume:single=1.0);
begin
  if not fIsSoundInitialized then Exit;
  Play(SoundID, KMPointF(0,0), false, Volume); //Redirect
end;


procedure TSoundLib.Play(SoundID:TSoundFXNew; Volume:single=1.0; FadeMusic:boolean=false);
begin
  Play(SoundID, KMPoint(0,0), false, Volume, FadeMusic);
end;


procedure TSoundLib.Play(SoundID:TSoundFXNew; Loc:TKMPoint; Attenuated:boolean=true; Volume:single=1.0; FadeMusic:boolean=false);
begin
  PlayWave(ExeDir+NewSFXFolder+NewSFXFile[SoundID], KMPointF(Loc), Attenuated, Volume, FadeMusic);
end;


{Wrapper for TSoundFX}
procedure TSoundLib.Play(SoundID:TSoundFX; Loc:TKMPoint; Attenuated:boolean=true; Volume:single=1.0);
begin
  if not fIsSoundInitialized then Exit;
  PlaySound(SoundID, '', KMPointF(Loc), Attenuated, Volume); //Redirect
end;


procedure TSoundLib.Play(SoundID:TSoundFX; Loc:TKMPointF; Attenuated:boolean=true; Volume:single=1.0);
begin
  if not fIsSoundInitialized then Exit;
  PlaySound(SoundID, '', Loc, Attenuated, Volume); //Redirect
end;


{Wrapper WAV files}
procedure TSoundLib.PlayWave(const aFile:string; Loc:TKMPointF; Attenuated:boolean=true; Volume:single=1.0; FadeMusic:boolean=false);
begin
  if not fIsSoundInitialized then Exit;
  PlaySound(sfx_None, aFile, Loc, Attenuated, Volume, FadeMusic); //Redirect
end;


{Call to this procedure will find free spot and start to play sound immediately}
{Will need to make another one for unit sounds, which will take WAV file path as parameter}
{Attenuated means if sound should fade over distance or not}
procedure TSoundLib.PlaySound(SoundID:TSoundFX; const aFile:string; Loc:TKMPointF; Attenuated:boolean=true; Volume:single=1.0; FadeMusic:boolean=false);
var Dif:array[1..3]of single;
  FreeBuf{,FreeSrc}:integer;
  i,ID:integer;
  Distance:single;
  ALState:TALint;
  WAVformat: TALenum;
  WAVdata: TALvoid;
  WAVsize: TALsizei;
  WAVfreq: TALsizei;
  WAVloop: TALint;
  WAVDuration:cardinal;
begin
  if not fIsSoundInitialized then Exit;
  if (SoundID = sfx_None) and (aFile = '') then Exit;

  Distance := GetLength(Loc.X-fListener.Pos[1], Loc.Y-fListener.Pos[2]);
  //If sound source is further than MAX_DISTANCE away then don't play it. This stops the buffer being filled with sounds on the other side of the map.
  if Attenuated and (Distance >= MAX_PLAY_DISTANCE) then Exit;
  //If the sounds is a fairly long way away it should not play when we are short of slots
  if Attenuated and (Distance >= MAX_PRIORITY_DISTANCE) and (ActiveCount >= MAX_FAR_SOUNDS) then Exit;
  //Attenuated sounds are always lower priority, so save a few slots for non-attenuated so that troops
  //and menus always make sounds
  if Attenuated and (ActiveCount >= MAX_ATTENUATED_SOUNDS) then exit;

  //Here should be some sort of RenderQueue/List/Clip

  //1. Find matching buffer
  //Found - add refCount and reference it
  //Not found
  //2. Find free buffer
  //


  //Find free buffer and use it
  FreeBuf := 0;
  for i:=1 to MAX_SOUNDS do begin
    alGetSourcei(fSound[i].ALSource, AL_SOURCE_STATE, @ALState);
    if ALState<>AL_PLAYING then begin
      FreeBuf := i;
      Break;
    end;
  end;
  if FreeBuf = 0 then Exit;//Don't play if there's no room left

  //Fade music if required (don't fade it if the user has SoundGain = 0, that's confusing)
  if FadeMusic and (fSoundGain > 0) and not fMusicIsFaded then
  begin
    if Assigned(fOnFadeMusic) then fOnFadeMusic(Self);
    fMusicIsFaded := true;
  end;

  //Stop previously playing sound and release buffer
  AlSourceStop(fSound[FreeBuf].ALSource);
  AlSourcei(fSound[FreeBuf].ALSource, AL_BUFFER, 0);

  //Assign new data to buffer and assign it to source
  if SoundID = sfx_None then
  begin
    alutLoadWAVFile(aFile,WAVformat,WAVdata,WAVsize,WAVfreq,WAVloop);
    AlBufferData(fSound[FreeBuf].ALBuffer,WAVformat,WAVdata,WAVsize,WAVfreq);
    alutUnloadWAV(WAVformat,WAVdata,WAVsize,WAVfreq);
    WAVDuration := round(WAVsize / WAVfreq * 1000);
    case WAVformat of
      AL_FORMAT_STEREO16: WAVDuration := WAVDuration div 4;
      AL_FORMAT_STEREO8: WAVDuration := WAVDuration div 2;
      AL_FORMAT_MONO16: WAVDuration := WAVDuration div 2;
    end;
  end
  else
  begin
    ID := word(SoundID);
    Assert(fWaves[ID].IsLoaded and (ID <= fWavesCount), 'Sounds.dat seems to be short');
    AlBufferData(fSound[FreeBuf].ALBuffer, AL_FORMAT_MONO8, @fWaves[ID].Data[0], fWaves[ID].Head.DataSize, fWaves[ID].Head.SampleRate);
    WAVsize := fWaves[ID].Head.FileSize;
    WAVfreq := fWaves[ID].Head.BytesPerSecond;
    WAVDuration := round(WAVsize / WAVfreq * 1000);
  end;

  //Set source properties
  AlSourcei(fSound[FreeBuf].ALSource, AL_BUFFER, fSound[FreeBuf].ALBuffer);
  AlSourcef(fSound[FreeBuf].ALSource, AL_PITCH, 1.0);
  AlSourcef(fSound[FreeBuf].ALSource, AL_GAIN, 1.0 * Volume * fSoundGain);
  if Attenuated then begin
    Dif[1]:=Loc.X; Dif[2]:=Loc.Y; Dif[3]:=0;
    AlSourcefv(fSound[FreeBuf].ALSource, AL_POSITION, @Dif[1]);
    AlSourcei(fSound[FreeBuf].ALSource, AL_SOURCE_RELATIVE, AL_FALSE); //If Attenuated then it is not relative to the listener
  end else
  begin
    //For sounds that do not change over distance, set to SOURCE_RELATIVE and make the position be 0,0,0 which means it will follow the listener
    //Do not simply set position to the listener as the listener could change while the sound is playing
    Dif[1]:=0; Dif[2]:=0; Dif[3]:=0;
    AlSourcefv(fSound[FreeBuf].ALSource, AL_POSITION, @Dif[1]);
    AlSourcei(fSound[FreeBuf].ALSource, AL_SOURCE_RELATIVE, AL_TRUE); //Relative to the listener, meaning it follows us
  end;
  AlSourcef(fSound[FreeBuf].ALSource, AL_REFERENCE_DISTANCE, 4.0);
  AlSourcef(fSound[FreeBuf].ALSource, AL_MAX_DISTANCE, MAX_DISTANCE);
  AlSourcef(fSound[FreeBuf].ALSource, AL_ROLLOFF_FACTOR, 1.0);
  AlSourcei(fSound[FreeBuf].ALSource, AL_LOOPING, AL_FALSE);

  //Start playing
  AlSourcePlay(fSound[FreeBuf].ALSource);
  if SoundID <> sfx_None then
    fSound[FreeBuf].Name := SSoundFX[SoundID]
  else
    fSound[FreeBuf].Name := ExtractFileName(aFile);
  fSound[FreeBuf].Position := Loc;
  fSound[FreeBuf].Duration := WAVDuration;
  fSound[FreeBuf].PlaySince := GetTickCount;
  fSound[FreeBuf].FadesMusic := FadeMusic;
end;


function TSoundLib.WarriorSoundFile(aUnitType:TUnitType; aSound:TWarriorSpeech; aNumber:byte):string;
var S:string;
begin
  if not fIsSoundInitialized then Exit;
  S := ExeDir + 'data\sfx\speech.'+fLocale+'\';
  if fWarriorUseBackup[aUnitType] then
    S := S + WarriorSFXFolderBackup[aUnitType]
  else
    S := S + WarriorSFXFolder[aUnitType];
  S := S + '\' + WarriorSFX[aSound] + IntToStr(aNumber);
  Result := '';
  if FileExists(S+'.snd') then Result := S+'.snd'; //Some languages use .snd files, but inside they are just WAVs renamed
  if FileExists(S+'.wav') then Result := S+'.wav';
end;


function TSoundLib.NotificationSoundFile(aSound:TAttackNotification; aNumber:byte):string;
var S:string;
begin
  if not fIsSoundInitialized then Exit;
  S := ExeDir + 'data\sfx\speech.'+fLocale+ '\' + AttackNotifications[aSound] + int2fix(aNumber,2);
  Result := '';
  if FileExists(S+'.snd') then Result := S+'.snd';
  if FileExists(S+'.wav') then Result := S+'.wav'; //In Russian version there are WAVs
end;


procedure TSoundLib.PlayCitizen(aUnitType:TUnitType; aSound:TWarriorSpeech);
begin
  PlayCitizen(aUnitType, aSound, KMPointF(0,0));
end;


procedure TSoundLib.PlayCitizen(aUnitType:TUnitType; aSound:TWarriorSpeech; aLoc:TKMPointF);
var Wave:string; HasLoc:boolean; SoundID: byte;
begin
  if not fIsSoundInitialized then Exit;
  if not (aUnitType in [CITIZEN_MIN..CITIZEN_MAX]) then Exit;

  if aSound = sp_Death then
    SoundID := CitizenSFX[aUnitType].DeathID
  else
    SoundID := CitizenSFX[aUnitType].SelectID;

  HasLoc := not KMSamePointF(aLoc, KMPointF(0,0));
  Wave := WarriorSoundFile(CitizenSFX[aUnitType].WarriorVoice, aSound, SoundID);
  if FileExists(Wave) then
    PlayWave(Wave, aLoc, HasLoc, 1 + 3*byte(HasLoc)); //Attenuate sounds when aLoc is valid
end;


procedure TSoundLib.PlayNotification(aSound:TAttackNotification);
var Wave:string; Count:byte;
begin
  if not fIsSoundInitialized then Exit;

  Count := fNotificationSoundCount[aSound];

  Wave := NotificationSoundFile(aSound, Random(Count));
  if FileExists(Wave) then
    PlayWave(Wave, KMPointF(0,0), false, 1.0);
end;


procedure TSoundLib.PlayWarrior(aUnitType:TUnitType; aSound:TWarriorSpeech);
begin
  PlayWarrior(aUnitType, aSound, KMPointF(0,0));
end;


procedure TSoundLib.PlayWarrior(aUnitType:TUnitType; aSound:TWarriorSpeech; aLoc:TKMPointF);
var Wave:string; HasLoc:boolean; Count:byte;
begin
  if not fIsSoundInitialized then Exit;
  if not (aUnitType in [WARRIOR_MIN..WARRIOR_MAX]) then Exit;

  Count := fWarriorSoundCount[aUnitType, aSound];

  HasLoc := not KMSamePointF(aLoc, KMPointF(0,0));
  Wave := WarriorSoundFile(aUnitType, aSound, Random(Count));
  if FileExists(Wave) then
    PlayWave(Wave, aLoc, HasLoc, 1 + 3*byte(HasLoc)); //Attenuate sounds when aLoc is valid
end;


function TSoundLib.ActiveCount:byte;
var i:integer;
begin
  Result := 0;
  for i:=1 to MAX_SOUNDS do
  if (fSound[i].PlaySince<>0) and (fSound[i].PlaySince+fSound[i].Duration > GetTickCount) then
    inc(Result)
  else
    fSound[i].PlaySince := 0;
end;


procedure TSoundLib.Paint;
var i:integer;
begin
  fRenderAux.Circle(fListener.Pos[1], fListener.Pos[2], MAX_DISTANCE, $00000000, $FFFFFFFF);
  for i:=1 to MAX_SOUNDS do
  if (fSound[i].PlaySince<>0) and (fSound[i].PlaySince+fSound[i].Duration > GetTickCount) then
  begin
    fRenderAux.Circle(fSound[i].Position.X, fSound[i].Position.Y, 5, $4000FFFF, $FFFFFFFF);
    fRenderAux.Text(Round(fSound[i].Position.X), Round(fSound[i].Position.Y), fSound[i].Name, $FFFFFFFF);
  end else
    fSound[i].PlaySince := 0;
end;


procedure TSoundLib.UpdateStateIdle;
var i:integer; FoundFaded:boolean;
begin
  if not fMusicIsFaded then exit;

  FoundFaded := false;
  for i:=1 to MAX_SOUNDS do
    if fSound[i].FadesMusic then
    begin
      FoundFaded := true;
      if (fSound[i].PlaySince<>0) and (fSound[i].PlaySince+fSound[i].Duration > GetTickCount) then
        exit //There is still a faded sound playing
      else
        fSound[i].FadesMusic := false; //Make sure we don't resume more than once for this sound
    end;
  //If we reached the end without exiting then we need to resume the music
  fMusicIsFaded := false;
  if FoundFaded and Assigned(fOnUnfadeMusic) then  fOnUnfadeMusic(Self);
end;


//Scan and count the number of warrior sounds
procedure TSoundLib.ScanWarriorSounds;
var
  i:integer;
  U:TUnitType;
  WS:TWarriorSpeech;
  AN:TAttackNotification;
begin
  //Try to load counts from file
  if LoadWarriorSoundsFromFile(ExeDir + 'data\sfx\speech.'+fLocale+'\count.dat') then
    Exit;

  //Reset counts from previous locale/unsuccessful load
  FillChar(fWarriorSoundCount, SizeOf(fWarriorSoundCount), #0);
  FillChar(fNotificationSoundCount, SizeOf(fNotificationSoundCount), #0);
  FillChar(fWarriorUseBackup, SizeOf(fWarriorUseBackup), #0);

  if not DirectoryExists(ExeDir + 'data\sfx\speech.'+fLocale+'\') then Exit;

  //First inspect folders, if the prefered ones don't exist use the backups
  for U:=WARRIOR_MIN to WARRIOR_MAX do
    if not DirectoryExists(ExeDir+'data\sfx\speech.'+fLocale+'\'+WarriorSFXFolder[U]+'\') then
      fWarriorUseBackup[U] := true;

  //If the folder exists it is likely all the sounds are there
  for U:=WARRIOR_MIN to WARRIOR_MAX do
    for WS:=Low(TWarriorSpeech) to High(TWarriorSpeech) do
      for i:=0 to 255 do
        if not FileExists(WarriorSoundFile(U, WS, i)) then
        begin
          fWarriorSoundCount[U,WS] := i;
          break;
        end;

  //Scan warning messages (e.g. under attack)
  for AN:=Low(TAttackNotification) to High(TAttackNotification) do
    for i:=0 to 255 do
      if not FileExists(NotificationSoundFile(AN, i)) then
      begin
        fNotificationSoundCount[AN] := i;
        break;
      end;

  //Save counts to file for faster access next time
  SaveWarriorSoundsToFile(ExeDir + 'data\sfx\speech.'+fLocale+'\count.dat');
end;


function TSoundLib.LoadWarriorSoundsFromFile(const aFile: String): Boolean;
var
  S:String;
  MS:TKMemoryStream;
begin
  Result := False;
  if not FileExists(aFile) then Exit;

  MS := TKMemoryStream.Create;
  try
    MS.LoadFromFile(aFile);
    MS.Read(S);
    if S = GAME_VERSION then
    begin
      MS.Read(fWarriorSoundCount, SizeOf(fWarriorSoundCount));
      MS.Read(fWarriorUseBackup, SizeOf(fWarriorUseBackup));
      MS.Read(fNotificationSoundCount, SizeOf(fNotificationSoundCount));
      Result := True;
    end;
  finally
    MS.Free;
  end;
end;


procedure TSoundLib.SaveWarriorSoundsToFile(const aFile: String);
var
  MS:TKMemoryStream;
begin
  MS := TKMemoryStream.Create;
  MS.Write(GAME_VERSION);
  MS.Write(fWarriorSoundCount, SizeOf(fWarriorSoundCount));
  MS.Write(fWarriorUseBackup, SizeOf(fWarriorUseBackup));
  MS.Write(fNotificationSoundCount, SizeOf(fNotificationSoundCount));
  MS.SaveToFile(aFile);
  MS.Free;
end;


end.
