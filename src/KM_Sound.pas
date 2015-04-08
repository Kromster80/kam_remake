unit KM_Sound;
{$I KaM_Remake.inc}
interface
uses
  Classes, Dialogs, Forms, SysUtils, TypInfo,
  {$IFDEF WDC} UITypes, {$ENDIF}
  {$IFDEF Unix} LCLIntf, LCLType, {$ENDIF}
  OpenAL, KromUtils,
  KM_Defaults, KM_CommonClasses, KM_Points, KM_ResSound;

const
  MAX_SOUNDS = 16; //64 looks like the limit, depends on hardware
  MAX_LOOP_SOUNDS = 4;

type
  TKMSoundPlayer = class
  private
    fALDevice: PALCdevice;

    fListener: record
      Pos: array [1..3] of TALfloat; //Position in 3D space
      Vel: array [1..3] of TALfloat; //Velocity, used in doppler effect calculation
      Ori: array [1..6] of TALfloat; //Orientation LookingAt and UpVector
    end;
    fIsSoundInitialized: boolean;

    fSound: array [1..MAX_SOUNDS] of record
      ALBuffer: TALuint;
      ALSource: TALuint;
      Name: UnicodeString;
      Position: TKMPointF;
      Duration: cardinal; //MSec
      PlaySince: cardinal;
      Volume: Single;
      Looped: Boolean;
      FadesMusic: Boolean;
    end;

    fLoopSoundIndex: array[1..MAX_LOOP_SOUNDS] of Integer;

    fSoundGain:single; //aka "Global volume"
    fMusicIsFaded:boolean;

    fOnFadeMusic: TNotifyEvent;
    fOnUnfadeMusic: TNotifyEvent;
    procedure CheckOpenALError;

    function PlayWave(const aFile: UnicodeString; Loc: TKMPointF; Attenuated:boolean=true; Volume:single=1; FadeMusic:boolean=false; aLoop: Boolean=False): Integer;
    function PlaySound(SoundID: TSoundFX; const aFile: UnicodeString; Loc: TKMPointF;
                       Attenuated: Boolean; Volume: Single; Radius: Single;
                       FadeMusic: Boolean; aLoop: Boolean): Integer;
  public
    constructor Create(aVolume: Single);
    destructor Destroy; override;
    function ActiveCount: Byte;

    property OnRequestFade: TNotifyEvent write fOnFadeMusic;
    property OnRequestUnfade: TNotifyEvent write fOnUnfadeMusic;
    procedure AbortAllFadeSounds;
    procedure AbortAllLoopedSounds;
    procedure AbortAllLongSounds;

    procedure UpdateListener(X,Y: Single);
    procedure UpdateSoundVolume(Value: Single);

    procedure PlayNotification(aSound: TAttackNotification);
    procedure PlayWAVFromScript(aFileName: UnicodeString; Loc: TKMPoint; Attenuated:Boolean; Volume:Single; Radius: Single; aFadesMusic: Boolean);

    procedure PlayCitizen(aUnitType: TUnitType; aSound: TWarriorSpeech); overload;
    procedure PlayCitizen(aUnitType: TUnitType; aSound: TWarriorSpeech; aLoc: TKMPointF); overload;
    procedure PlayWarrior(aUnitType: TUnitType; aSound: TWarriorSpeech); overload;
    procedure PlayWarrior(aUnitType: TUnitType; aSound: TWarriorSpeech; aLoc: TKMPointF); overload;
    procedure Play(SoundID: TSoundFX; Volume: Single = 1); overload;
    procedure Play(SoundID: TSoundFX; Loc: TKMPoint; Attenuated:boolean=true; Volume:single=1); overload;
    procedure Play(SoundID: TSoundFX; Loc: TKMPointF; Attenuated:boolean=true; Volume:single=1); overload;

    procedure Play(SoundID: TSoundFXNew; Volume:Single = 1; FadeMusic:boolean=false); overload;
    procedure Play(SoundID: TSoundFXNew; Loc: TKMPoint; Attenuated:boolean=true; Volume:single=1; FadeMusic:boolean=false); overload;

    function PlayLoopSound(const aFile: UnicodeString; aLoc: TKMPointF; aAttenuate: Boolean; aVolume: Single; aRadius: Single): Integer;
    procedure StopLoopSound(aIndex: Integer);

    procedure Paint;
    procedure UpdateStateIdle;
  end;

  TKMLoopSoundsManager = class
  private
    fListener: TKMPointF;
    fLastScriptIndex: Integer;
    fCount: Integer;
    fSounds: array of record
                        PlayingIndex: Integer; //Index into gSoundPlayer, or -1 if not playing
                        //Fields below are saved
                        ScriptIndex: Integer;
                        SoundName: AnsiString; //Just sound name, not the path
                        Volume: Single;
                        Radius: Single;
                        Attenuate: Boolean;
                        Loc: TKMPoint;
                        HandIndex: TKMHandIndex;
                      end;
    function CanPlay(aIndex: Integer): Boolean;
    procedure StartLoopSound(aIndex: Integer);
    procedure StopLoopSound(aIndex: Integer);
  public
    destructor Destroy; override;
    property Count: Integer read fCount;

    procedure Save(SaveStream: TKMemoryStream);
    procedure Load(LoadStream: TKMemoryStream);
    procedure UpdateState;

    function AddLoopSound(aHandIndex: TKMHandIndex; const aSoundName: AnsiString; aLoc: TKMPoint; aAttenuate: Boolean; aVolume: Single; aRadius: Single): Integer;
    procedure RemoveLoopSound(aScriptIndex: Integer);
    procedure UpdateListener(X,Y: Single);
  end;


var
  gSoundPlayer: TKMSoundPlayer;
  gLoopSounds: TKMLoopSoundsManager;


implementation
uses
  KM_RenderAux, KM_Log, KM_Utils, KM_Resource, KM_HandsCollection, KM_Game;


const
  MAX_ATTENUATED_SOUNDS = (3/4)*MAX_SOUNDS; //Attenuated sounds are less important, always save space for others
  MAX_FAR_SOUNDS = (1/2)*MAX_SOUNDS; //Sounds that are too far away can only access this many slots

  MAX_BUFFERS = 16; //16/24/32 looks like the limit, depends on hardware
  MAX_SOURCES = 32; //depends on hardware as well
  MAX_DISTANCE = 32; //After this distance sounds are completely mute
  MAX_PRIORITY_DISTANCE_FACTOR = (1/2); //Sounds past this distance will not play if there are few slots left (gives close sounds priority)


{ TKMSoundPlayer }
constructor TKMSoundPlayer.Create(aVolume:single);
var
  Context: PALCcontext;
  I: Integer;
  NumMono,NumStereo: TALCint;
begin
  inherited Create;

  for I := Low(fLoopSoundIndex) to High(fLoopSoundIndex) do
    fLoopSoundIndex[I] := -1;

  if SKIP_SOUND then Exit;

  fIsSoundInitialized := InitOpenAL;
  Set8087CW($133F); //Above OpenAL call messes up FPU settings
  if not fIsSoundInitialized then begin
    gLog.AddNoTime('OpenAL warning. OpenAL could not be initialized.');
    //MessageDlg works better than Application.MessageBox or others, it stays on top and pauses here until the user clicks ok.
    MessageDlg('OpenAL could not be initialized. Please refer to Readme.html for solution', mtWarning, [mbOk], 0);
    fIsSoundInitialized := false;
    Exit;
  end;

  //Open device
  fALDevice := alcOpenDevice(nil); // this is supposed to select the "preferred device"
  Set8087CW($133F); //Above OpenAL call messes up FPU settings
  if fALDevice = nil then begin
    gLog.AddNoTime('OpenAL warning. Device could not be opened.');
    //MessageDlg works better than Application.MessageBox or others, it stays on top and pauses here until the user clicks ok.
    MessageDlg('OpenAL device could not be opened. Please refer to Readme.html for solution', mtWarning, [mbOk], 0);
    fIsSoundInitialized := false;
    Exit;
  end;

  //Create context(s)
  Context := alcCreateContext(fALDevice, nil);
  Set8087CW($133F); //Above OpenAL call messes up FPU settings
  if Context = nil then begin
    gLog.AddNoTime('OpenAL warning. Context could not be created.');
    //MessageDlg works better than Application.MessageBox or others, it stays on top and pauses here until the user clicks ok.
    MessageDlg('OpenAL context could not be created. Please refer to Readme.html for solution', mtWarning, [mbOk], 0);
    fIsSoundInitialized := false;
    Exit;
  end;

  //Set active context
  I := alcMakeContextCurrent(Context);
  Set8087CW($133F); //Above OpenAL call messes up FPU settings
  if I > 1 then begin //valid returns are AL_NO_ERROR=0 and AL_TRUE=1
    gLog.AddNoTime('OpenAL warning. Context could not be made current.');
    //MessageDlg works better than Application.MessageBox or others, it stays on top and pauses here until the user clicks ok.
    MessageDlg('OpenAL context could not be made current. Please refer to Readme.html for solution', mtWarning, [mbOk], 0);
    fIsSoundInitialized := false;
    Exit;
  end;

  CheckOpenALError;
  if not fIsSoundInitialized then Exit;

  //Set attenuation model
  alDistanceModel(AL_LINEAR_DISTANCE_CLAMPED);
  gLog.AddTime('Pre-LoadSFX init', True);

  alcGetIntegerv(fALDevice, ALC_MONO_SOURCES, 4, @NumMono);
  alcGetIntegerv(fALDevice, ALC_STEREO_SOURCES, 4, @NumStereo);

  gLog.AddTime('ALC_MONO_SOURCES',NumMono);
  gLog.AddTime('ALC_STEREO_SOURCES',NumStereo);

  for I:=1 to MAX_SOUNDS do begin
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

  gLog.AddTime('OpenAL init done');
end;


destructor TKMSoundPlayer.Destroy;
var
  I: Integer;
begin
  if fIsSoundInitialized then
  begin
    for I := 1 to MAX_SOUNDS do
    begin
      AlDeleteBuffers(1, @fSound[I].ALBuffer);
      AlDeleteSources(1, @fSound[I].ALSource);
    end;
    AlutExit;
  end;
  inherited;
end;


procedure TKMSoundPlayer.CheckOpenALError;
var ErrCode: Integer;
begin
  ErrCode := alcGetError(fALDevice);
  if ErrCode <> ALC_NO_ERROR then begin
    gLog.AddNoTime('OpenAL warning. There is OpenAL error '+inttostr(ErrCode)+' raised. Sound will be disabled.');
    //MessageDlg works better than Application.MessageBox or others, it stays on top and pauses here until the user clicks ok.
    MessageDlg('There is OpenAL error '+IntToStr(ErrCode)+' raised. Sound will be disabled.', mtWarning, [mbOk], 0);
    fIsSoundInitialized := False;
  end;
end;


procedure TKMSoundPlayer.AbortAllFadeSounds;
var I: Integer;
begin
  if SKIP_SOUND or not fIsSoundInitialized then Exit;

  fMusicIsFaded := False;
  for I := 1 to MAX_SOUNDS do
    if fSound[I].FadesMusic then
      alSourceStop(fSound[i].ALSource);
end;


procedure TKMSoundPlayer.AbortAllLongSounds;
var I: Integer;
begin
  if SKIP_SOUND or not fIsSoundInitialized then Exit;

  //This is used to abort long sounds from the game when you quit so they don't play in the menu
  for I := 1 to MAX_SOUNDS do
    if (fSound[I].PlaySince <> 0) and (GetTimeSince(fSound[I].PlaySince) < fSound[I].Duration)
    and not fSound[I].Looped //Looped sounds manage themselves
    and (fSound[I].Duration > 8000) then //Sounds <= 8 seconds can keep playing (e.g. victory music)
    begin
      fSound[I].PlaySince := 0;
      alSourceStop(fSound[i].ALSource);
    end;
end;


{Update listener position in 3D space}
procedure TKMSoundPlayer.UpdateListener(X,Y:single);
begin
  if SKIP_SOUND or not fIsSoundInitialized then Exit;
  fListener.Pos[1] := X;
  fListener.Pos[2] := Y;
  fListener.Pos[3] := 24; //Place Listener above the surface
  AlListenerfv(AL_POSITION, @fListener.Pos);
end;


{ Update sound gain (global volume for all sounds) }
procedure TKMSoundPlayer.UpdateSoundVolume(Value:single);
var I: Integer;
begin
  if SKIP_SOUND or not fIsSoundInitialized then Exit;
  fSoundGain := Value;
  //alListenerf(AL_GAIN, fSoundGain); //Set in source property

  //Loop sounds must be updated separately
  for I:=Low(fLoopSoundIndex) to High(fLoopSoundIndex) do
    if fLoopSoundIndex[I] <> -1 then
      AlSourcef(fSound[fLoopSoundIndex[I]].ALSource, AL_GAIN, 1 * fSound[fLoopSoundIndex[I]].Volume * fSoundGain);

  //Update the volume of all other playing sounds
  for I := 1 to MAX_SOUNDS do
    if not fSound[I].Looped //Looped sounds are handled above
    and (fSound[I].PlaySince <> 0) and (GetTimeSince(fSound[I].PlaySince) < fSound[I].Duration) then
      AlSourcef(fSound[I].ALSource, AL_GAIN, 1 * fSound[I].Volume * fSoundGain);
end;


{Wrapper with fewer options for non-attenuated sounds}
procedure TKMSoundPlayer.Play(SoundID: TSoundFX; Volume:single=1);
begin
  if SKIP_SOUND or not fIsSoundInitialized then Exit;
  Play(SoundID, KMPointF(0,0), false, Volume); //Redirect
end;


procedure TKMSoundPlayer.Play(SoundID: TSoundFXNew; Volume:single=1; FadeMusic:boolean=false);
begin
  if SKIP_SOUND or not fIsSoundInitialized then Exit;
  Play(SoundID, KMPoint(0,0), false, Volume, FadeMusic);
end;


procedure TKMSoundPlayer.Play(SoundID: TSoundFXNew; Loc: TKMPoint; Attenuated:boolean=true; Volume:single=1; FadeMusic:boolean=false);
begin
  if SKIP_SOUND or not fIsSoundInitialized then Exit;
  PlayWave(gRes.Sounds.FileOfNewSFX(SoundID), KMPointF(Loc), Attenuated, Volume, FadeMusic);
end;


{Wrapper for TSoundFX}
procedure TKMSoundPlayer.Play(SoundID: TSoundFX; Loc: TKMPoint; Attenuated:boolean=true; Volume:single=1);
begin
  if SKIP_SOUND or not fIsSoundInitialized then Exit;
  PlaySound(SoundID, '', KMPointF(Loc), Attenuated, Volume, MAX_DISTANCE, False, False); //Redirect
end;


procedure TKMSoundPlayer.Play(SoundID: TSoundFX; Loc: TKMPointF; Attenuated:boolean=true; Volume:single=1);
begin
  if SKIP_SOUND or not fIsSoundInitialized then Exit;
  PlaySound(SoundID, '', Loc, Attenuated, Volume, MAX_DISTANCE, False, False); //Redirect
end;


{Wrapper WAV files}
function TKMSoundPlayer.PlayWave(const aFile: UnicodeString; Loc: TKMPointF; Attenuated:boolean=true; Volume:single=1; FadeMusic:boolean=false; aLoop: Boolean=False): Integer;
begin
  Result := -1;
  if not fIsSoundInitialized then Exit;
  Result := PlaySound(sfx_None, aFile, Loc, Attenuated, Volume, MAX_DISTANCE, FadeMusic, aLoop); //Redirect
end;


{Call to this procedure will find free spot and start to play sound immediately}
{Will need to make another one for unit sounds, which will take WAV file path as parameter}
{Attenuated means if sound should fade over distance or not}
function TKMSoundPlayer.PlaySound(SoundID: TSoundFX; const aFile: UnicodeString; Loc: TKMPointF;
                                  Attenuated: Boolean; Volume: Single; Radius: Single;
                                  FadeMusic: Boolean; aLoop: Boolean): Integer;
var Dif:array[1..3]of single;
  FreeBuf{,FreeSrc}:integer;
  i,ID:integer;
  W: TKMSoundData;
  Distance:single;
  ALState: TALint;
  WAVformat: TALenum;
  WAVdata: TALvoid;
  WAVsize: TALsizei;
  WAVfreq: TALsizei;
  WAVloop: TALint;
  WAVDuration:cardinal;
begin
  Result := -1;
  if not fIsSoundInitialized then Exit;
  if (SoundID = sfx_None) and (aFile = '') then Exit;

  if Attenuated then
  begin
    Distance := GetLength(Loc.X-fListener.Pos[1], Loc.Y-fListener.Pos[2]);
    //If sound source is further than Radius away then don't play it. This stops the buffer being filled with sounds on the other side of the map.
    if (Distance >= Radius) then Exit;
    //If the sounds is a fairly long way away it should not play when we are short of slots
    if (Distance >= Radius*MAX_PRIORITY_DISTANCE_FACTOR) and (ActiveCount >= MAX_FAR_SOUNDS) then Exit;
    //Attenuated sounds are always lower priority, so save a few slots for non-attenuated so that troops
    //and menus always make sounds
    if (ActiveCount >= MAX_ATTENUATED_SOUNDS) then exit;
  end;

  //Here should be some sort of RenderQueue/List/Clip

  //1. Find matching buffer
  //Found - add refCount and reference it
  //Not found
  //2. Find free buffer
  //


  //Find free buffer and use it
  FreeBuf := 0;
  for i:=1 to MAX_SOUNDS do
  begin
    alGetSourcei(fSound[i].ALSource, AL_SOURCE_STATE, @ALState);
    if ALState<>AL_PLAYING then
    begin
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
    try
      alutLoadWAVFile(aFile,WAVformat,WAVdata,WAVsize,WAVfreq,WAVloop);
      AlBufferData(fSound[FreeBuf].ALBuffer,WAVformat,WAVdata,WAVsize,WAVfreq);
      alutUnloadWAV(WAVformat,WAVdata,WAVsize,WAVfreq);
    except
      //This happens regularly if you run two copies of the game out of one folder and they share the MP chat sound.
      //We ignore the error to make it possible to run two copies out of one folder (especially for debugging) without
      //continual clashes over sound files.
      on E: EFOpenError do
      begin
        gLog.AddTime('Error loading sound file: '+E.Message);
        Exit;
      end;
    end;
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
    W := gRes.Sounds.fWaves[ID];

    Assert(W.IsLoaded and (ID <= gRes.Sounds.fWavesCount), 'Sounds.dat seems to be short');
    AlBufferData(fSound[FreeBuf].ALBuffer, AL_FORMAT_MONO8, @W.Data[0], W.Head.DataSize, W.Head.SampleRate);
    WAVsize := W.Head.FileSize;
    WAVfreq := W.Head.BytesPerSecond;
    WAVDuration := round(WAVsize / WAVfreq * 1000);
  end;

  //Set source properties
  AlSourcei(fSound[FreeBuf].ALSource, AL_BUFFER, fSound[FreeBuf].ALBuffer);
  AlSourcef(fSound[FreeBuf].ALSource, AL_PITCH, 1);
  AlSourcef(fSound[FreeBuf].ALSource, AL_GAIN, 1 * Volume * fSoundGain);
  if Attenuated then
  begin
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
  AlSourcef(fSound[FreeBuf].ALSource, AL_REFERENCE_DISTANCE, 4);
  AlSourcef(fSound[FreeBuf].ALSource, AL_MAX_DISTANCE, Radius);
  AlSourcef(fSound[FreeBuf].ALSource, AL_ROLLOFF_FACTOR, 1);
  if aLoop then
    AlSourcei(fSound[FreeBuf].ALSource, AL_LOOPING, AL_TRUE)
  else
    AlSourcei(fSound[FreeBuf].ALSource, AL_LOOPING, AL_FALSE);

  //Start playing
  AlSourcePlay(fSound[FreeBuf].ALSource);
  if SoundID <> sfx_None then
    fSound[FreeBuf].Name := GetEnumName(TypeInfo(TSoundFX), Integer(SoundID))
  else
    fSound[FreeBuf].Name := ExtractFileName(aFile);
  fSound[FreeBuf].Position := Loc;
  fSound[FreeBuf].Duration := WAVDuration;
  fSound[FreeBuf].PlaySince := TimeGet;
  fSound[FreeBuf].Volume := Volume;
  fSound[FreeBuf].Looped := aLoop;
  fSound[FreeBuf].FadesMusic := FadeMusic;

  Result := FreeBuf;
end;


procedure TKMSoundPlayer.PlayCitizen(aUnitType: TUnitType; aSound: TWarriorSpeech);
begin
  if SKIP_SOUND or not fIsSoundInitialized then Exit;

  PlayCitizen(aUnitType, aSound, KMPointF(0,0));
end;


procedure TKMSoundPlayer.PlayCitizen(aUnitType: TUnitType; aSound: TWarriorSpeech; aLoc: TKMPointF);
var
  Wave: UnicodeString;
  HasLoc: Boolean;
begin
  if SKIP_SOUND or not fIsSoundInitialized then Exit;
  if not (aUnitType in [CITIZEN_MIN..CITIZEN_MAX]) then Exit;

  HasLoc := not KMSamePointF(aLoc, KMPointF(0,0));
  Wave := gRes.Sounds.FileOfCitizen(aUnitType, aSound);
  if FileExists(Wave) then
    PlayWave(Wave, aLoc, HasLoc, 1 + 3*byte(HasLoc)); //Attenuate sounds when aLoc is valid
end;


procedure TKMSoundPlayer.PlayNotification(aSound: TAttackNotification);
var
  Wave: UnicodeString;
  Count: Byte;
begin
  if SKIP_SOUND or not fIsSoundInitialized then Exit;

  Count := gRes.Sounds.NotificationSoundCount[aSound];

  Wave := gRes.Sounds.FileOfNotification(aSound, Random(Count));
  if FileExists(Wave) then
    PlayWave(Wave, KMPointF(0,0), False, 1);
end;


procedure TKMSoundPlayer.PlayWAVFromScript(aFileName: UnicodeString; Loc: TKMPoint; Attenuated:Boolean; Volume:Single; Radius: Single; aFadesMusic: Boolean);
begin
  if SKIP_SOUND or not fIsSoundInitialized then Exit;

  PlaySound(sfx_None, aFileName, KMPointF(Loc), Attenuated, Volume, Radius, aFadesMusic, False);
end;


procedure TKMSoundPlayer.PlayWarrior(aUnitType: TUnitType; aSound: TWarriorSpeech);
begin
  if SKIP_SOUND or not fIsSoundInitialized then Exit;

  PlayWarrior(aUnitType, aSound, KMPointF(0,0));
end;


procedure TKMSoundPlayer.PlayWarrior(aUnitType: TUnitType; aSound: TWarriorSpeech; aLoc: TKMPointF);
var
  Wave: UnicodeString;
  HasLoc: Boolean;
  Count: Byte;
begin
  if SKIP_SOUND or not fIsSoundInitialized then Exit;
  if not (aUnitType in [WARRIOR_MIN..WARRIOR_MAX]) then Exit;

  Count := gRes.Sounds.WarriorSoundCount[aUnitType, aSound];

  HasLoc := not KMSamePointF(aLoc, KMPointF(0,0));
  Wave := gRes.Sounds.FileOfWarrior(aUnitType, aSound, Random(Count));
  if FileExists(Wave) then
    PlayWave(Wave, aLoc, HasLoc, 1 + 3*byte(HasLoc)); //Attenuate sounds when aLoc is valid
end;


function TKMSoundPlayer.PlayLoopSound(const aFile: UnicodeString; aLoc: TKMPointF; aAttenuate: Boolean; aVolume: Single; aRadius: Single): Integer;
var I: Integer;
begin
  Result := -1; //Failed to play
  for I:=Low(fLoopSoundIndex) to High(fLoopSoundIndex) do
    if fLoopSoundIndex[I] = -1 then
    begin
      fLoopSoundIndex[I] := PlaySound(sfx_None, aFile, aLoc, aAttenuate, aVolume, aRadius, False, True);
      if fLoopSoundIndex[I] <> -1 then
        Result := I; //Successfully playing
      Exit;
    end;
end;


procedure TKMSoundPlayer.StopLoopSound(aIndex: Integer);
begin
  if not fIsSoundInitialized then Exit;
  if fLoopSoundIndex[aIndex] = -1 then Exit;

  //Stop previously playing sound and release buffer
  AlSourceStop(fSound[fLoopSoundIndex[aIndex]].ALSource);
  AlSourcei(fSound[fLoopSoundIndex[aIndex]].ALSource, AL_BUFFER, 0);

  fSound[fLoopSoundIndex[aIndex]].PlaySince := 0;
  fLoopSoundIndex[aIndex] := -1;
end;


procedure TKMSoundPlayer.AbortAllLoopedSounds;
var I: Integer;
begin
  for I := Low(fLoopSoundIndex) to High(fLoopSoundIndex) do
    StopLoopSound(I);
end;


function TKMSoundPlayer.ActiveCount: Byte;
var I: Integer;
begin
  Result := 0;
  for I := 1 to MAX_SOUNDS do
  if (fSound[I].PlaySince <> 0)
  and ((GetTimeSince(fSound[I].PlaySince) < fSound[I].Duration) or fSound[I].Looped) then
    Inc(Result)
  else
    fSound[I].PlaySince := 0;
end;


procedure TKMSoundPlayer.Paint;
var I: Integer;
begin
  gRenderAux.CircleOnTerrain(fListener.Pos[1], fListener.Pos[2], MAX_DISTANCE, $00000000, $FFFFFFFF);
  for I := 1 to MAX_SOUNDS do
  if (fSound[I].PlaySince <> 0)
  and ((GetTimeSince(fSound[I].PlaySince) < fSound[I].Duration)  or fSound[I].Looped) then
  begin
    gRenderAux.CircleOnTerrain(fSound[I].Position.X, fSound[I].Position.Y, 5, $4000FFFF, $FFFFFFFF);
    gRenderAux.Text(Round(fSound[I].Position.X), Round(fSound[I].Position.Y), fSound[I].Name, $FFFFFFFF);
  end else
    fSound[I].PlaySince := 0;
end;


procedure TKMSoundPlayer.UpdateStateIdle;
var I: Integer;
begin
  if not fMusicIsFaded then Exit;

  for I := 1 to MAX_SOUNDS do
    if fSound[I].FadesMusic then
    begin
      if (fSound[I].PlaySince <> 0) and (GetTimeSince(fSound[I].PlaySince) < fSound[I].Duration) then
        Exit //There is still a faded sound playing
      else
        fSound[I].FadesMusic := False; //Make sure we don't resume more than once for this sound
    end;
  //If we reached the end without exiting then we need to resume the music
  fMusicIsFaded := False;
  if Assigned(fOnUnfadeMusic) then
    fOnUnfadeMusic(Self);
end;


{TKMLoopSoundsManager}
destructor TKMLoopSoundsManager.Destroy;
begin
  gSoundPlayer.AbortAllLoopedSounds;
end;


procedure TKMLoopSoundsManager.UpdateState;
var I: Integer;
begin
  //Check whether a sound needs starting or stopping
  for I := 0 to fCount-1 do
    if (fSounds[I].PlayingIndex = -1) and CanPlay(I) then
      StartLoopSound(I)
    else
      if (fSounds[I].PlayingIndex <> -1) and not CanPlay(I) then
        StopLoopSound(I);
end;


function TKMLoopSoundsManager.CanPlay(aIndex: Integer): Boolean;
var DistanceSqr: Single;
begin
  Result := ((fSounds[aIndex].HandIndex = MySpectator.HandIndex) or (fSounds[aIndex].HandIndex = PLAYER_NONE))
             and (not fSounds[aIndex].Attenuate or (MySpectator.FogOfWar.CheckTileRevelation(fSounds[aIndex].Loc.X, fSounds[aIndex].Loc.Y) > 0));
  if not Result then Exit;

  if fSounds[aIndex].Attenuate then
  begin
    DistanceSqr := KMDistanceSqr(KMPointF(fSounds[aIndex].Loc), fListener);
    Result := Result and (DistanceSqr < Sqr(fSounds[aIndex].Radius));
  end;
end;


procedure TKMLoopSoundsManager.StartLoopSound(aIndex: Integer);
var S: UnicodeString;
begin
  S := ExeDir + gGame.GetScriptSoundFile(fSounds[aIndex].SoundName);

  //Silently ignore missing files
  if not FileExists(S) or not CanPlay(aIndex) then
    Exit;

  fSounds[aIndex].PlayingIndex := gSoundPlayer.PlayLoopSound(S, KMPointF(fSounds[aIndex].Loc), fSounds[aIndex].Attenuate, fSounds[aIndex].Volume, fSounds[aIndex].Radius);
end;


procedure TKMLoopSoundsManager.StopLoopSound(aIndex: Integer);
begin
  if fSounds[aIndex].PlayingIndex = -1 then Exit;

  gSoundPlayer.StopLoopSound(fSounds[aIndex].PlayingIndex);
  fSounds[aIndex].PlayingIndex := -1;
end;


function TKMLoopSoundsManager.AddLoopSound(aHandIndex: TKMHandIndex; const aSoundName: AnsiString; aLoc: TKMPoint; aAttenuate: Boolean; aVolume: Single; aRadius: Single): Integer;
var NewIndex: Integer;
begin
  Inc(fCount);
  Inc(fLastScriptIndex);
  NewIndex := fCount-1;
  if Length(fSounds) < fCount then
    SetLength(fSounds, fCount+8);

  fSounds[NewIndex].ScriptIndex := fLastScriptIndex;
  fSounds[NewIndex].PlayingIndex := -1;
  fSounds[NewIndex].SoundName := aSoundName;
  fSounds[NewIndex].Loc := aLoc;
  fSounds[NewIndex].Attenuate := aAttenuate;
  fSounds[NewIndex].Volume := aVolume;
  fSounds[NewIndex].Radius := aRadius;
  fSounds[NewIndex].HandIndex := aHandIndex;

  StartLoopSound(NewIndex);
  Result := fSounds[NewIndex].ScriptIndex;
end;


procedure TKMLoopSoundsManager.RemoveLoopSound(aScriptIndex: Integer);
var I: Integer;
begin
  for I := 0 to fCount-1 do
    if fSounds[I].ScriptIndex = aScriptIndex then
    begin
      StopLoopSound(I);

      if I <> fCount - 1 then
        Move(fSounds[I+1], fSounds[I], (fCount - 1 - I) * SizeOf(fSounds[I]));

      Dec(fCount);
      Exit; //Only one sound will have this aScriptIndex
    end;
end;


procedure TKMLoopSoundsManager.UpdateListener(X,Y: Single);
begin
  fListener := KMPointF(X, Y);
end;


procedure TKMLoopSoundsManager.Save(SaveStream: TKMemoryStream);
var I: Integer;
begin
  SaveStream.Write(fLastScriptIndex);
  SaveStream.Write(fCount);
  for I := 0 to fCount-1 do
  begin
    SaveStream.Write(fSounds[I].ScriptIndex);
    SaveStream.WriteA(fSounds[I].SoundName);
    SaveStream.Write(fSounds[I].Volume);
    SaveStream.Write(fSounds[I].Radius);
    SaveStream.Write(fSounds[I].Attenuate);
    SaveStream.Write(fSounds[I].Loc);
    SaveStream.Write(fSounds[I].HandIndex);
  end;
end;


procedure TKMLoopSoundsManager.Load(LoadStream: TKMemoryStream);
var I: Integer;
begin
  LoadStream.Read(fLastScriptIndex);
  LoadStream.Read(fCount);
  SetLength(fSounds, fCount);
  for I := 0 to fCount-1 do
  begin
    LoadStream.Read(fSounds[I].ScriptIndex);
    LoadStream.ReadA(fSounds[I].SoundName);
    LoadStream.Read(fSounds[I].Volume);
    LoadStream.Read(fSounds[I].Radius);
    LoadStream.Read(fSounds[I].Attenuate);
    LoadStream.Read(fSounds[I].Loc);
    LoadStream.Read(fSounds[I].HandIndex);
    fSounds[I].PlayingIndex := -1; //Indicates that it is not currently playing
  end;
end;


end.
