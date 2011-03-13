unit KM_Sound;
{$I KaM_Remake.inc}
interface
uses Classes, Forms, Windows, SysUtils, {$IFDEF WDC} MMSystem,  {$ENDIF}
  OpenAL, KromUtils,
  KM_CommonTypes, KM_Defaults, KM_Utils;


const
    MAX_SOUNDS = 16; //64 looks like the limit, depends on hardware
    MAX_BUFFERS = 16; //16/24/32 looks like the limit, depends on hardware
    MAX_SOURCES = 32; //depends on hardware as well
    MAX_DISTANCE = 32; //After this distance sounds are completely mute

    WarriorSFXFolder: array[15..24] of string = (
        'MILITIA','AXEMAN','SWORDMAN',
        'BOWMAN','CROSSBOWMAN','LANCEMAN',
        'PIKEMAN','CAVALRY','KNIGHTS','BARBARIAN');

    WarriorSFX: array[TSoundToPlay] of string = (
        'SELECT','EAT','LEFT','RIGHT','HALVE','JOIN','HALT','SEND', 'ATTACK',
        'FORMAT','DEATH','BATTLE','STORM');

type
  TSoundLib = class
  private
    fALDevice: PALCdevice;
    fWavesCount:integer;
    fWaves: array of record
      Head: TWAVHeaderEx;
      Data: array of char;
      Foot: array of char;
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
      Position:TKMPoint;
      Duration:word; //MSec
      PlaySince:cardinal;
    end;

    fSoundGain:single; //aka "Global volume"
    fWarriorSoundCount: array[15..24, TSoundToPlay] of byte;
    procedure LoadSoundsDAT;
    procedure CheckOpenALError;
    function GetWarriorSoundFile(aUnitType:TUnitType; aSound:TSoundToPlay; aNumber:byte; aLocale:string=''):string;
  public
    constructor Create(aLocale:string; aVolume:single);
    destructor Destroy; override;
    function ActiveCount:byte;
    procedure ExportSounds;
    procedure UpdateListener(X,Y:single);
    procedure UpdateSoundVolume(Value:single);
    procedure PlayWarrior(aUnitType:TUnitType; aSound:TSoundToPlay);
    procedure Play(SoundID:TSoundFX; const Volume:single=1.0); overload;
    procedure Play(SoundID:TSoundFX; Loc:TKMPoint; const Attenuated:boolean=true; const Volume:single=1.0); overload;
    procedure Paint;
end;


var
  fSoundLib: TSoundLib;


implementation
uses KM_Render, KM_Game, Dialogs;


constructor TSoundLib.Create(aLocale:string; aVolume:single);
var
  Context: PALCcontext;

  i,k:integer;
  NumMono,NumStereo:TALCint;
  s:TSoundToPlay;
begin
  Inherited Create;

  fIsSoundInitialized := InitOpenAL;
  if not fIsSoundInitialized then begin
    fLog.AddToLog('OpenAL warning. OpenAL could not be initialized.');
    Application.MessageBox('OpenAL could not be initialized. Please refer to Readme.txt for solution','OpenAL warning', MB_OK + MB_ICONEXCLAMATION);
    fIsSoundInitialized := false;
    exit;
  end;

  //Open device
  fALDevice := alcOpenDevice(nil); // this is supposed to select the "preferred device"
  if fALDevice = nil then begin
    fLog.AddToLog('OpenAL warning. Device could not be opened.');
    Application.MessageBox('Device could not be opened. Please refer to Readme.txt for solution','OpenAL warning', MB_OK + MB_ICONEXCLAMATION);
    fIsSoundInitialized := false;
    exit;
  end;

  //Create context(s)
  Context := alcCreateContext(fALDevice, nil);
  if Context = nil then begin
    fLog.AddToLog('OpenAL warning. Context could not be created.');
    Application.MessageBox('Context could not be created. Please refer to Readme.txt for solution','OpenAL warning', MB_OK + MB_ICONEXCLAMATION);
    fIsSoundInitialized := false;
    exit;
  end;

  //Set active context
  if alcMakeContextCurrent(Context) > 1 then begin //valid returns are AL_NO_ERROR=0 and AL_TRUE=1
    fLog.AddToLog('OpenAL warning. Context could not be made current.');
    Application.MessageBox('Context could not be made current. Please refer to Readme.txt for solution','OpenAL warning', MB_OK + MB_ICONEXCLAMATION);
    fIsSoundInitialized := false;
    exit;
  end;

  CheckOpenALError;
  if not fIsSoundInitialized then exit;

  //Set attenuation model
  alDistanceModel(AL_LINEAR_DISTANCE_CLAMPED);
  fLog.AppendLog('Pre-LoadSFX init',true);

  alcGetIntegerv(fALDevice, ALC_MONO_SOURCES, 4, @NumMono);
  alcGetIntegerv(fALDevice, ALC_STEREO_SOURCES, 4, @NumStereo);

  fLog.AppendLog('ALC_MONO_SOURCES',NumMono);
  fLog.AppendLog('ALC_STEREO_SOURCES',NumStereo);

  for i:=1 to MAX_SOUNDS do begin
    AlGenBuffers(1, @fSound[i].ALBuffer);
    AlGenSources(1, @fSound[i].ALSource);
  end;

  CheckOpenALError;
  if not fIsSoundInitialized then exit;

  //Set default Listener orientation
  fListener.Ori[1]:=0; fListener.Ori[2]:=1; fListener.Ori[3]:=0; //Look-at vector
  fListener.Ori[4]:=0; fListener.Ori[5]:=0; fListener.Ori[6]:=1; //Up vector
  AlListenerfv(AL_ORIENTATION, @fListener.Ori);
  fSoundGain := aVolume;

  fLog.AppendLog('OpenAL init done');

  LoadSoundsDAT;
  fLog.AppendLog('Load Sounds.dat',true);

  //Scan and count the number of warrior sounds
  for i:=15 to 24 do
    for s:=low(TSoundToPlay) to high(TSoundToPlay) do
      for k:=0 to 255 do
        if not FileExists(GetWarriorSoundFile(TUnitType(i),s,k,aLocale)) then
        begin
          fWarriorSoundCount[i,s] := k;
          break;
        end;
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
  f:file;
  Head:record Size,Count:word; end;
  Tab1:array[1..200]of integer;
  Tab2:array[1..200]of smallint;
  i,Tmp:integer;
begin
  if not fIsSoundInitialized then exit;
  if not CheckFileExists(ExeDir+'data\sfx\sounds.dat') then exit;
  AssignFile(f, ExeDir+'data\sfx\sounds.dat');
  FileMode := 0;
  Reset(f,1);
  FileMode := 2;

  BlockRead(f, Head, 4); //Read 4 bytes into Head record
  BlockRead(f, Tab1, Head.Count*4); //Read Count*4bytes into Tab1(WaveSizes)
  BlockRead(f, Tab2, Head.Count*2); //Read Count*2bytes into Tab2(No idea what is it)

  fWavesCount := Head.Count;
  setlength(fWaves, fWavesCount+1);

  for i:=1 to Head.Count do begin
    BlockRead(f,Tmp,4); //Always '1' for existing waves
    if Tab1[i]<>0 then begin
      BlockRead(f,fWaves[i].Head,SizeOf(fWaves[i].Head));
      setlength(fWaves[i].Data,fWaves[i].Head.DataSize);
      BlockRead(f,fWaves[i].Data[0],fWaves[i].Head.DataSize);
      setlength(fWaves[i].Foot,Tab1[i]-SizeOf(fWaves[i].Head)-fWaves[i].Head.DataSize);
      BlockRead(f,fWaves[i].Foot[0],Tab1[i]-SizeOf(fWaves[i].Head)-fWaves[i].Head.DataSize);
    end;
    fWaves[i].IsLoaded := true;
  end;

  {BlockRead(f,c,20);
  //Packed record
  //SampleRate,Volume,a,b:integer;
  //i,j,k,l,Index:word;
  BlockRead(f,Props[1],26*Head.Count);}

  CloseFile(f);
end;


procedure TSoundLib.ExportSounds;
var f:file; i:integer;
begin
  if not fIsSoundInitialized then exit;
  CreateDir(ExeDir+'Export\');
  CreateDir(ExeDir+'Export\Sounds.dat\');
  for i:=1 to fWavesCount do if length(fWaves[i].Data)>0 then begin
    assignfile(f,ExeDir+'Export\Sounds.dat\sound_'+int2fix(i,3)+'_'+SSoundFX[TSoundFX(i)]+'.wav'); rewrite(f,1);
    //Waves[i].Head.SampleRate := Waves[i].Head.SampleRate div 2; //Make it half speed?
    blockwrite(f,fWaves[i].Head,SizeOf(fWaves[i].Head));
    blockwrite(f,fWaves[i].Data[0],length(fWaves[i].Data));
    blockwrite(f,fWaves[i].Foot[0],length(fWaves[i].Foot));
    closefile(f);
  end;
end;


{Update listener position in 3D space}
procedure TSoundLib.UpdateListener(X,Y:single);
begin
  if not fIsSoundInitialized then exit;
  fListener.Pos[1] := X;
  fListener.Pos[2] := Y;
  fListener.Pos[3] := 12; //Place Listener above the surface
  AlListenerfv(AL_POSITION, @fListener.Pos);
end;


{ Update sound gain (global volume for all sounds) }
procedure TSoundLib.UpdateSoundVolume(Value:single);
begin
  if not fIsSoundInitialized then exit;
  fSoundGain := Value;
  //alListenerf(AL_GAIN, fSoundGain); //Set in source property
end;


{Wrapper with fewer options for non-attenuated sounds}
procedure TSoundLib.Play(SoundID:TSoundFX; const Volume:single=1.0);
begin
  if not fIsSoundInitialized then exit;
  Play(SoundID, KMPoint(0,0), false, Volume); //Redirect
end;


{Call to this procedure will find free spot and start to play sound immediately}
{Will need to make another one for unit sounds, which will take WAV file path as parameter}
{Attenuated means if sound should fade over distance or not}
procedure TSoundLib.Play(SoundID:TSoundFX; Loc:TKMPoint; const Attenuated:boolean=true; const Volume:single=1.0);
var Dif:array[1..3]of single;
  FreeBuf{,FreeSrc}:integer;
  i,ID:integer;
  ALState:TALint;
begin
  if not fIsSoundInitialized then exit;

  //If sound source is further than MAX_DISTANCE away then don't play it. This stops the buffer being filled with sounds on the other side of the map.
  if Attenuated and (GetLength(Loc,KMPoint(Round(fListener.Pos[1]),Round(fListener.Pos[2]))) > MAX_DISTANCE) then exit;

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
      FreeBuf:=i;
      break;
    end;
  end;
  if FreeBuf = 0 then exit;//Don't play if there's no room left

  ID := word(SoundID);
  Assert(fWaves[ID].IsLoaded);

  //Stop previously playing sound and release buffer
  AlSourceStop(fSound[FreeBuf].ALSource);
  AlSourcei(fSound[FreeBuf].ALSource, AL_BUFFER, 0);

  //Assign new data to buffer and assign it to source
  AlBufferData(fSound[FreeBuf].ALBuffer, AL_FORMAT_MONO8, @fWaves[ID].Data[0], fWaves[ID].Head.DataSize, fWaves[ID].Head.SampleRate);

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
  fSound[FreeBuf].Name := SSoundFX[SoundID];
  fSound[FreeBuf].Position := Loc;
  fSound[FreeBuf].Duration := round(fWaves[ID].Head.FileSize / fWaves[ID].Head.BytesPerSecond * 1000);
  fSound[FreeBuf].PlaySince := GetTickCount;
end;


function TSoundLib.GetWarriorSoundFile(aUnitType:TUnitType; aSound:TSoundToPlay; aNumber:byte; aLocale:string=''):string;
begin
  if not fIsSoundInitialized then exit;
  if (aLocale = '') and (fGame <> nil) and (fGame.fGlobalSettings <> nil) then
    aLocale := fGame.fGlobalSettings.Locale;
  if not (aUnitType in [ut_Militia .. ut_Barbarian]) then
    Result := ''
  else
    Result := ExeDir + 'data\Sfx\Speech.'+aLocale+'\' + WarriorSFXFolder[byte(aUnitType)] + '\' + WarriorSFX[aSound] + IntToStr(aNumber) + '.wav';
end;


//todo: Use sound playing system that allows for multiple sounds at once and a listener position (e.g. deaths and cries in a battle)
//@Krom: Why can't we simply use OpenAL? Because it needs a listener position so you don't hear battles from the other side of the map. Do the sounds need to be stored in memory? Can we buffer them or something?
//@Lewin: I think we will do this eventually. I'll be working on it
procedure TSoundLib.PlayWarrior(aUnitType:TUnitType; aSound:TSoundToPlay);
var wave:string;
begin
  if not fIsSoundInitialized then exit;
  if not (aUnitType in [ut_Militia .. ut_Barbarian]) then exit;
  //File extension must be .wav as well as the file contents itself
  wave := GetWarriorSoundFile(aUnitType, aSound, PseudoRandom(fWarriorSoundCount[byte(aUnitType),aSound]));
  {$IFDEF WDC}
  if FileExists(wave) then
    sndPlaySound(@wave[1], SND_NODEFAULT or SND_ASYNC) //Override any previous voice playing
  else
    fLog.AppendLog('Speech file not found for '+TypeToString(aUnitType)+' sound ID '+IntToStr(byte(aSound))+': '+wave);
  {$ENDIF}
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
  fRender.RenderDebugCircle(fListener.Pos[1], fListener.Pos[2], MAX_DISTANCE, $00000000, $FFFFFFFF);
  for i:=1 to MAX_SOUNDS do
  if (fSound[i].PlaySince<>0) and (fSound[i].PlaySince+fSound[i].Duration > GetTickCount) then
  begin
    fRender.RenderDebugCircle(fSound[i].Position.X, fSound[i].Position.Y, 5, $4000FFFF, $FFFFFFFF);
    fRender.RenderDebugText(fSound[i].Position.X, fSound[i].Position.Y, fSound[i].Name, $FFFFFFFF);
  end else
    fSound[i].PlaySince := 0;
end;


end.
