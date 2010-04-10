unit KM_Sound;
interface
uses Forms, Windows, MMSystem,
  {$IFDEF VER140} MPlayer, {$ENDIF}
  Classes, SysUtils, KromUtils, OpenAL, KM_Defaults, KM_CommonTypes, KM_Utils;

const MaxWaves = 200;
//@Krom: On large maps lots of sounds get skipped. Would it be possible to make this larger, as 16 sounds at once isn't very much.
const MaxSourceCount = 16; //Actually it depends on hardware

const WarriorSFXFolder: array[15..24] of string = (
  'MILITIA','AXEMAN','SWORDMAN',
  'BOWMAN','CROSSBOWMAN','LANCEMAN',
  'PIKEMAN','CAVALRY','KNIGHTS','BARBARIAN');

const WarriorSFX: array[TSoundToPlay] of string = (
  'SELECT','EAT','LEFT','RIGHT','HALVE','JOIN','HALT','SEND', 'ATTACK',
  'FORMAT','DEATH','BATTLE','STORM');

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
    IsOpenALInitialized:boolean;
    //Buffer used to store the wave data, Source is sound position in space
    ALSource,ALBuffer: array [1..64] of TALuint;
    SoundGain:single;
    WarriorSoundCount: array[15..24,TSoundToPlay] of byte;
    procedure LoadSoundsDAT();
    function GetWarriorSoundFile(aUnitType:TUnitType; aSound:TSoundToPlay; aNumber:byte; aLocale:string=''):string;
  public
    constructor Create(aLocale:string);
    destructor Destroy(); override;
    procedure ExportSounds();
    procedure UpdateListener(Pos:TKMPointF);
    procedure UpdateSoundVolume(Value:single);
    procedure PlayWarrior(aUnitType:TUnitType; aSound:TSoundToPlay); overload;
    procedure Play(SoundID:TSoundFX; const Volume:single=1.0); overload;
    procedure Play(SoundID:TSoundFX; Loc:TKMPoint; const Attenuated:boolean=true; const Volume:single=1.0); overload;
end;


var
  fSoundLib: TSoundLib;


implementation
uses
  KM_Unit1, KM_Game, Dialogs;


constructor TSoundLib.Create(aLocale:string);
var
  Context: PALCcontext;
  Device: PALCdevice;
  i,k,ErrCode:integer;
  s:TSoundToPlay;
begin
  Inherited Create;

  //Scan and count the number of warrior sounds
  for i:=15 to 24 do
    for s:=low(TSoundToPlay) to high(TSoundToPlay) do
      for k:=0 to 255 do
        if not FileExists(GetWarriorSoundFile(TUnitType(i),s,k,aLocale)) then
        begin
          WarriorSoundCount[i,s] := k;
          break;
        end;

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
    Application.MessageBox(@(String('There is OpenAL error '+inttostr(ErrCode)+' raised. Sound will be disabled.'))[1],'OpenAL error', MB_OK + MB_ICONEXCLAMATION);
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
  if IsOpenALInitialized then
  begin
    AlDeleteBuffers(MaxSourceCount, @ALBuffer);
    AlDeleteSources(MaxSourceCount, @ALSource);
    AlutExit();
  end;  
  Inherited;
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
    Waves[i].Head.SampleRate := Waves[i].Head.SampleRate div 2;
    blockwrite(f,Waves[i].Head,SizeOf(Waves[i].Head));
    blockwrite(f,Waves[i].Data[0],length(Waves[i].Data));
    blockwrite(f,Waves[i].Foot[0],length(Waves[i].Foot));
    closefile(f);
  end;
end;


{Update listener position in 3D space}
procedure TSoundLib.UpdateListener(Pos:TKMPointF);
begin
  if not IsOpenALInitialized then exit;
  Listener.Pos[1]:=Pos.X;
  Listener.Pos[2]:=Pos.Y;
  Listener.Pos[3]:=12; //Place Listener above the surface
  AlListenerfv ( AL_POSITION, @Listener.Pos);
end;


{Update sound gain (global volume for all sounds/music)}
procedure TSoundLib.UpdateSoundVolume(Value:single);
begin
  if not IsOpenALInitialized then exit;
  SoundGain := Value;
//  alListenerf ( AL_GAIN, SoundGain );
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
const MAX_DISTANCE = 35.0;
begin
  if not IsOpenALInitialized then exit;

  //fLog.AddToLog('SoundPlay ID'+inttostr(byte(SoundID))+' at '+TypeToString(Loc));

  //If sound source is further than MAX_DISTANCE away then don't play it. This stops the buffer being fill with sounds on the other side of the map.
  if Attenuated and (GetLength(Loc,KMPoint(Round(Listener.Pos[1]),Round(Listener.Pos[2]))) > MAX_DISTANCE) then exit;

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
    AlSourcei ( ALSource[FreeBuf], AL_SOURCE_RELATIVE, AL_FALSE); //If Attenuated then it is not relative to the listener
  end else
  begin
    //For sounds that do not change over distance, set to SOURCE_RELATIVE and make the position be 0,0,0 which means it will follow the listener
    //Do not simply set position to the listener as the listener could change while the sound is playing
    Dif[1]:=0; Dif[2]:=0; Dif[3]:=0;
    AlSourcefv( ALSource[FreeBuf], AL_POSITION, @Dif[1]);
    AlSourcei ( ALSource[FreeBuf], AL_SOURCE_RELATIVE, AL_TRUE); //Relative to the listener, meaning it follows us
  end;
  AlSourcef ( ALSource[FreeBuf], AL_REFERENCE_DISTANCE, 4.0 );
  AlSourcef ( ALSource[FreeBuf], AL_MAX_DISTANCE, MAX_DISTANCE );
  AlSourcef ( ALSource[FreeBuf], AL_ROLLOFF_FACTOR, 1.0 );
  AlSourcei ( ALSource[FreeBuf], AL_LOOPING, AL_FALSE );

  //Start playing
  AlSourcePlay(ALSource[FreeBuf]);
end;


function TSoundLib.GetWarriorSoundFile(aUnitType:TUnitType; aSound:TSoundToPlay; aNumber:byte; aLocale:string=''):string;
begin
  if (aLocale = '') and (fGame <> nil) and (fGame.fGlobalSettings <> nil) then
    aLocale := fGame.fGlobalSettings.GetLocale;
  if (byte(aUnitType) < 15) or (byte(aUnitType) > 24) then
    Result := ''
  else
    Result := ExeDir + 'data\Sfx\Speech.'+aLocale+'\' + WarriorSFXFolder[byte(aUnitType)] + '\' + WarriorSFX[aSound] + IntToStr(aNumber) + '.snd';
end;


procedure TSoundLib.PlayWarrior(aUnitType:TUnitType; aSound:TSoundToPlay);
var wave:string;
begin
  //wave := 'E:\KnightsAndMerchants\data\Sfx\Speech.eng\Axeman\SELECT0.wav';
  if (byte(aUnitType) < 15) or (byte(aUnitType) > 24) then exit;
  wave := GetWarriorSoundFile(aUnitType,aSound,Random(WarriorSoundCount[byte(aUnitType),aSound]));
  if FileExists(wave) then
    ShowMessage(wave);
    //sndPlaySound(@wave[1], SND_NODEFAULT or SND_ASYNC or SND_NOSTOP); //@Krom: This isn't playing anything for me :(
end;


end.
