unit KM_Sound;
{$I KaM_Remake.inc}
interface
uses
  OpenAL,
  KM_ResSound,
  KM_Defaults, KM_CommonClasses, KM_CommonTypes, KM_Points;


const
  MAX_SOUNDS = 16; //64 looks like the limit, depends on hardware
  MAX_LOOP_SOUNDS = 4;
  WAV_FILE_EXT = '.wav';
  OGG_FILE_EXT = '.ogg';

type

  TKMAudioFormat = (af_Wav, af_Ogg);

  TKMSoundPlayer = class
  private
    fALDevice: PALCdevice;

    fListener: record
      Pos: array [1..3] of TALfloat; //Position in 3D space
      Vel: array [1..3] of TALfloat; //Velocity, used in doppler effect calculation
      Ori: array [1..6] of TALfloat; //Orientation LookingAt and UpVector
    end;
    fIsSoundInitialized: Boolean;

    fSound: array [1..MAX_SOUNDS] of record
      ALBuffer: TALuint;
      ALSource: TALuint;
      Name: UnicodeString;
      Position: TKMPointF;
      Duration: cardinal; //MSec
      PlaySince: cardinal;
      Volume: Single;
      Looped: Boolean;
      FromScript: Boolean;
      FadesMusic: Boolean;
    end;

    fScriptSoundIndex: array[1..MAX_LOOP_SOUNDS] of Integer;

    fLastMessageNoticeTime: Cardinal; // Last time message notice were played

    fSoundGain: Single; //aka "Global volume"
    fMusicIsFaded: Boolean;

    fOnFadeMusic: TEvent;
    fOnUnfadeMusic: TBooleanEvent;
    procedure CheckOpenALError;
    function IsSoundPlaying(aIndex: Integer): Boolean;

    function PlayWave(const aFile: UnicodeString; Loc: TKMPointF; Attenuated: Boolean = True; Volume: Single = 1;
                      FadeMusic: Boolean = False; aLoop: Boolean = False): Integer;
    function PlaySound(SoundID: TSoundFX; const aFile: UnicodeString; Loc: TKMPointF;
                       Attenuated: Boolean; Volume: Single; Radius: Single;
                       FadeMusic, aLooped: Boolean; aFromScript: Boolean = False): Integer;
  public
    constructor Create(aVolume: Single);
    destructor Destroy; override;
    function ActiveCount: Byte;

    property OnRequestFade: TEvent write fOnFadeMusic;
    property OnRequestUnfade: TBooleanEvent write fOnUnfadeMusic;
    procedure AbortAllFadeSounds;
    procedure AbortAllScriptSounds;
    procedure AbortAllLongSounds;

    procedure UpdateListener(X,Y: Single);
    procedure UpdateSoundVolume(Value: Single);

    procedure PlayNotification(aSound: TAttackNotification);
    procedure PlaySoundFromScript(const aFileName: UnicodeString; Loc: TKMPoint; Attenuated: Boolean; Volume: Single; Radius: Single; aFadesMusic: Boolean);

    procedure PlayCitizen(aUnitType: TUnitType; aSound: TWarriorSpeech); overload;
    procedure PlayCitizen(aUnitType: TUnitType; aSound: TWarriorSpeech; const aLoc: TKMPointF); overload;
    procedure PlayWarrior(aUnitType: TUnitType; aSound: TWarriorSpeech); overload;
    procedure PlayWarrior(aUnitType: TUnitType; aSound: TWarriorSpeech; const aLoc: TKMPointF); overload;
    procedure Play(SoundID: TSoundFX; Volume: Single = 1); overload;
    procedure Play(SoundID: TSoundFX; Loc: TKMPoint; Attenuated: Boolean = True; Volume: Single = 1); overload;
    procedure Play(SoundID: TSoundFX; Loc: TKMPointF; Attenuated: Boolean = True; Volume: Single = 1); overload;

    procedure Play(SoundID: TSoundFXNew; Volume:Single = 1; FadeMusic: Boolean = False); overload;
    procedure Play(SoundID: TSoundFXNew; Loc: TKMPoint; Attenuated: Boolean = True; Volume: Single = 1; FadeMusic: Boolean = False); overload;

    function PlayScriptSound(const aFile: UnicodeString; aLoc: TKMPointF; aAttenuate: Boolean; aVolume: Single; aRadius: Single; aFadeMusic, aLooped: Boolean): Integer;
    procedure StopScriptSound(aIndex: Integer);

    procedure Paint;
    procedure UpdateStateIdle;
  end;

  TKMScriptSoundsManager = class
  private
    fListener: TKMPointF;
    fLastScriptIndex: Integer;
    fCount: Integer;
    fSounds: array of record
                        PlayingIndex: Integer; //Index into gSoundPlayer, or -1 if not playing
                        //Fields below are saved
                        Looped: Boolean;
                        FadeMusic: Boolean;
                        ScriptIndex: Integer;
                        SoundName: AnsiString; //Just sound name, not the path
                        AudioFormat: TKMAudioFormat;
                        Volume: Single;
                        Radius: Single;
                        Attenuate: Boolean;
                        Loc: TKMPoint;
                        HandIndex: TKMHandIndex;
                      end;
    function CanPlay(aIndex: Integer): Boolean;
    procedure StartSound(aIndex: Integer);
    procedure StopSound(aIndex: Integer);
  public
    destructor Destroy; override;
    property Count: Integer read fCount;

    procedure Save(SaveStream: TKMemoryStream);
    procedure Load(LoadStream: TKMemoryStream);
    procedure UpdateState;

    function AddSound(aHandIndex: TKMHandIndex; const aSoundName: AnsiString; aSoundFormat: TKMAudioFormat; aLoc: TKMPoint;
                          aAttenuate: Boolean; aVolume: Single; aRadius: Single; aFadeMusic, aLooped: Boolean): Integer;
    procedure RemoveLoopSound(aScriptIndex: Integer);
    procedure RemoveSound(aScriptIndex: Integer; aLoopedOnly: Boolean = False);
    procedure UpdateListener(X,Y: Single);
  end;


var
  gSoundPlayer: TKMSoundPlayer;
  gScriptSounds: TKMScriptSoundsManager;


implementation
uses
  Classes, Dialogs, SysUtils, TypInfo, KromUtils,
  {$IFDEF WDC} UITypes, {$ENDIF}
  Codec, VorbisFile,
  KM_Game, KM_Resource, KM_HandsCollection, KM_RenderAux,
  KM_Log, KM_CommonUtils;


const
  MAX_ATTENUATED_SOUNDS = (3/4)*MAX_SOUNDS; //Attenuated sounds are less important, always save space for others
  MAX_FAR_SOUNDS = (1/2)*MAX_SOUNDS; //Sounds that are too far away can only access this many slots

  MAX_BUFFERS = 16; //16/24/32 looks like the limit, depends on hardware
  MAX_SOURCES = 32; //depends on hardware as well
  MAX_DISTANCE = 32; //After this distance sounds are completely mute
  MAX_PRIORITY_DISTANCE_FACTOR = (1/2); //Sounds past this distance will not play if there are few slots left (gives close sounds priority)
  MAX_DURATION_FROM_LAST_SND_MESSAGE_NOTICE = 100; //Maximum time in ms from lsat message notite. To avoid 'echo' effect for multiple messages at one time


{ TKMSoundPlayer }
constructor TKMSoundPlayer.Create(aVolume:single);
var
  Context: PALCcontext;
  I: Integer;
  NumMono,NumStereo: TALCint;
begin
  inherited Create;

  for I := Low(fScriptSoundIndex) to High(fScriptSoundIndex) do
    fScriptSoundIndex[I] := -1;

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
    and not fSound[I].FromScript //Looped sounds manage themselves
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
procedure TKMSoundPlayer.UpdateSoundVolume(Value: Single);
  procedure UpdateSound(aI: Integer);
  begin
    AlSourcef(fSound[aI].ALSource, AL_GAIN, 1 * fSound[aI].Volume * fSoundGain);
  end;

var I: Integer;
begin
  if SKIP_SOUND or not fIsSoundInitialized then Exit;
  fSoundGain := Value;
  //alListenerf(AL_GAIN, fSoundGain); //Set in source property

  //Loop sounds must be updated separately
  for I := Low(fScriptSoundIndex) to High(fScriptSoundIndex) do
    if fScriptSoundIndex[I] <> -1 then
      UpdateSound(fScriptSoundIndex[I]);//AlSourcef(fSound[fScriptSoundIndex[I]].ALSource, AL_GAIN, 1 * fSound[fScriptSoundIndex[I]].Volume * fSoundGain);

  //Update the volume of all other playing sounds
  for I := 1 to MAX_SOUNDS do
    if not fSound[I].FromScript //Looped sounds are handled above
      and (fSound[I].PlaySince <> 0) and (GetTimeSince(fSound[I].PlaySince) < fSound[I].Duration) then
      UpdateSound(I);//AlSourcef(fSound[I].ALSource, AL_GAIN, 1 * fSound[I].Volume * fSoundGain);
end;


{Wrapper with fewer options for non-attenuated sounds}
procedure TKMSoundPlayer.Play(SoundID: TSoundFX; Volume:single=1);
begin
  if SKIP_SOUND or not fIsSoundInitialized then Exit;

  // Check for consecutive messageNotices
  // When many warrior groups are hungry at the same time or many houses are not occupied at the same time
  // Sound should not be played N times, 1 is enought
  if SoundID = sfx_MessageNotice then
  begin
    if (fLastMessageNoticeTime > 0)
      and (GetTimeSince(fLastMessageNoticeTime) < MAX_DURATION_FROM_LAST_SND_MESSAGE_NOTICE) then
      Exit
    else
      fLastMessageNoticeTime := TimeGet;
  end;

  Play(SoundID, KMPOINTF_ZERO, false, Volume); //Redirect
end;


procedure TKMSoundPlayer.Play(SoundID: TSoundFXNew; Volume: Single = 1; FadeMusic: Boolean = False);
begin
  if SKIP_SOUND or not fIsSoundInitialized then Exit;
  Play(SoundID, KMPOINT_ZERO, false, Volume, FadeMusic);
end;


procedure TKMSoundPlayer.Play(SoundID: TSoundFXNew; Loc: TKMPoint; Attenuated:boolean=true; Volume:single=1; FadeMusic:boolean=false);
begin
  if SKIP_SOUND or not fIsSoundInitialized then Exit;
  PlayWave(gRes.Sounds.FileOfNewSFX(SoundID), KMPointF(Loc), Attenuated, Volume, FadeMusic);
end;


{Wrapper for TSoundFX}
procedure TKMSoundPlayer.Play(SoundID: TSoundFX; Loc: TKMPoint; Attenuated: Boolean = True; Volume: Single = 1);
begin
  if SKIP_SOUND or not fIsSoundInitialized then Exit;
  PlaySound(SoundID, '', KMPointF(Loc), Attenuated, Volume, MAX_DISTANCE, False, False); //Redirect
end;


procedure TKMSoundPlayer.Play(SoundID: TSoundFX; Loc: TKMPointF; Attenuated: Boolean = True; Volume: Single = 1);
begin
  if SKIP_SOUND or not fIsSoundInitialized then Exit;
  PlaySound(SoundID, '', Loc, Attenuated, Volume, MAX_DISTANCE, False, False); //Redirect
end;


{Wrapper WAV files}
function TKMSoundPlayer.PlayWave(const aFile: UnicodeString; Loc: TKMPointF; Attenuated: Boolean = True; Volume: Single = 1;
                                 FadeMusic: Boolean = False; aLoop: Boolean = False): Integer;
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
                                  FadeMusic, aLooped: Boolean; aFromScript: Boolean = False): Integer;
var Dif:array[1..3]of single;
  FreeBuf{,FreeSrc}: Integer;
  I, ID, OggOpenResult: Integer;
  W: TKMSoundData;
  Distance: Single;
  ALState: TALint;
  WAVformat: TALenum;
  WAVdata: TALvoid;
  WAVsize: TALsizei;
  WAVfreq: TALsizei;
  WAVloop: TALint;
  WAVDuration: Cardinal;
  FileExt: String;
  OggFileStream: TFileStream;
  OggVorbisFile: OggVorbis_File;
  VorbisInfo: P_Vorbis_Info;
  OggBytesRead, OggBytesChanged: Longword;
  OggBuffer: PChar;
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
    if (ActiveCount >= MAX_ATTENUATED_SOUNDS) then Exit;
  end;

  //Here should be some sort of RenderQueue/List/Clip

  //1. Find matching buffer
  //Found - add refCount and reference it
  //Not found
  //2. Find free buffer
  //


  //Find free buffer and use it
  FreeBuf := 0;
  for I := 1 to MAX_SOUNDS do
  begin
    alGetSourcei(fSound[i].ALSource, AL_SOURCE_STATE, @ALState);
    if ALState<>AL_PLAYING then
    begin
      FreeBuf := I;
      Break;
    end;
  end;
  if FreeBuf = 0 then Exit;//Don't play if there's no room left

  //Fade music if required (don't fade it if the user has SoundGain = 0, that's confusing)
  if FadeMusic and (fSoundGain > 0) and not fMusicIsFaded then
  begin
    if Assigned(fOnFadeMusic) then fOnFadeMusic;
    fMusicIsFaded := true;
  end;

  //Stop previously playing sound and release buffer
  AlSourceStop(fSound[FreeBuf].ALSource);
  AlSourcei(fSound[FreeBuf].ALSource, AL_BUFFER, 0);

  //Assign new data to buffer and assign it to source
  if SoundID = sfx_None then
  begin
    FileExt := ExtractFileExt(aFile);
    try
      if LowerCase(FileExt) = WAV_FILE_EXT then
      begin
        alutLoadWAVFile(aFile,WAVformat,WAVdata,WAVsize,WAVfreq,WAVloop);
        AlBufferData(fSound[FreeBuf].ALBuffer,WAVformat,WAVdata,WAVsize,WAVfreq);
        alutUnloadWAV(WAVformat,WAVdata,WAVsize,WAVfreq);
      end else if LowerCase(FileExt) = OGG_FILE_EXT then
      begin
        OggFileStream := TFileStream.Create(aFile, fmOpenRead or fmShareDenyNone);
        try
          OggOpenResult := ov_open_callbacks(OggFileStream, OggVorbisFile, nil, 0, ops_callbacks);
          if OggOpenResult <> 0 then
          begin
            gLog.AddTime('Error loading OGG sound file ''' + aFile + ''': ' + GetVorbisErrorName(OggOpenResult));
            Exit; // Ignore all errors
          end;

          // get ogg file info
          VorbisInfo := ov_info(OggVorbisFile, -1);

          if VorbisInfo.Channels = 1 then
            WAVformat := AL_FORMAT_MONO16
          else
            WAVformat := AL_FORMAT_STEREO16;

          WAVsize := ov_pcm_total(OggVorbisFile, -1) * 4;
          WAVfreq := VorbisInfo.Rate;

          GetMem(OggBuffer, WAVsize);
          try
            OggBytesRead := 0;
            // Load ogg file into the buffer with ov_read
            repeat
              OggBytesChanged := ov_read(OggVorbisFile, PKMStaticByteArray(OggBuffer)^[OggBytesRead], WAVsize - OggBytesRead, 0, 2, 1, nil);
              OggBytesRead := OggBytesRead + OggBytesChanged;
            until (OggBytesChanged = 0) or (OggBytesRead >= WAVsize);

            AlBufferData(fSound[FreeBuf].ALBuffer,WAVformat,OggBuffer,WAVsize,WAVfreq);
          finally
            FreeMem(OggBuffer, WAVsize);
          end;
        finally
          OggFileStream.Free;
        end;
      end
      else
        raise Exception.Create('Unsupported sound file format: ' + FileExt);
        
      WAVDuration := round(WAVsize / WAVfreq * 1000);
      case WAVformat of
        AL_FORMAT_STEREO16: WAVDuration := WAVDuration div 4;
        AL_FORMAT_STEREO8: WAVDuration := WAVDuration div 2;
        AL_FORMAT_MONO16: WAVDuration := WAVDuration div 2;
      end;
    except
      //This happens regularly if you run two copies of the game out of one folder and they share the MP chat sound.
      //We ignore the error to make it possible to run two copies out of one folder (especially for debugging) without
      //continual clashes over sound files.
      on E: EFOpenError do
      begin
        gLog.AddTime('Error loading sound file: ' + E.Message);
        Exit;
      end;
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

  if aLooped then
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
  fSound[FreeBuf].FromScript := aFromScript;
  fSound[FreeBuf].FromScript := aLooped;
  fSound[FreeBuf].FadesMusic := FadeMusic;

  Result := FreeBuf;
end;


procedure TKMSoundPlayer.PlayCitizen(aUnitType: TUnitType; aSound: TWarriorSpeech);
begin
  if SKIP_SOUND
    or not fIsSoundInitialized
    or ((gMySpectator.Selected <> nil) and not gMySpectator.IsSelectedMyObj) then // Do not play sound for ally's citizens selection
    Exit;

  PlayCitizen(aUnitType, aSound, KMPOINTF_ZERO);
end;


procedure TKMSoundPlayer.PlayCitizen(aUnitType: TUnitType; aSound: TWarriorSpeech; const aLoc: TKMPointF);
var
  Wave: UnicodeString;
  HasLoc: Boolean;
begin
  if SKIP_SOUND or not fIsSoundInitialized then Exit;
  if not (aUnitType in [CITIZEN_MIN..CITIZEN_MAX]) then Exit;

  HasLoc := not KMSamePointF(aLoc, KMPOINTF_ZERO);
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
    PlayWave(Wave, KMPOINTF_ZERO, False, 1);
end;


procedure TKMSoundPlayer.PlaySoundFromScript(const aFileName: UnicodeString; Loc: TKMPoint; Attenuated: Boolean; Volume: Single; Radius: Single; aFadesMusic: Boolean);
begin
  if SKIP_SOUND or not fIsSoundInitialized then Exit;

  PlaySound(sfx_None, aFileName, KMPointF(Loc), Attenuated, Volume, Radius, aFadesMusic, False);
end;


procedure TKMSoundPlayer.PlayWarrior(aUnitType: TUnitType; aSound: TWarriorSpeech);
begin
  if SKIP_SOUND
    or not fIsSoundInitialized
    or ((gMySpectator.Selected <> nil) and not gMySpectator.IsSelectedMyObj) then // Do not play sound for ally's warriors
    Exit;

  PlayWarrior(aUnitType, aSound, KMPOINTF_ZERO);
end;


procedure TKMSoundPlayer.PlayWarrior(aUnitType: TUnitType; aSound: TWarriorSpeech; const aLoc: TKMPointF);
var
  Wave: UnicodeString;
  HasLoc: Boolean;
  Count: Byte;
begin
  if SKIP_SOUND or not fIsSoundInitialized then Exit;
  if not (aUnitType in [WARRIOR_MIN..WARRIOR_MAX]) then Exit;

  Count := gRes.Sounds.WarriorSoundCount[aUnitType, aSound];

  HasLoc := not KMSamePointF(aLoc, KMPOINTF_ZERO);
  Wave := gRes.Sounds.FileOfWarrior(aUnitType, aSound, Random(Count));
  if FileExists(Wave) then
    PlayWave(Wave, aLoc, HasLoc, 1 + 3*byte(HasLoc)); //Attenuate sounds when aLoc is valid
end;


function TKMSoundPlayer.PlayScriptSound(const aFile: UnicodeString; aLoc: TKMPointF; aAttenuate: Boolean; aVolume: Single;
                                        aRadius: Single; aFadeMusic, aLooped: Boolean): Integer;
var I: Integer;
begin
  Result := -1; //Failed to play
  for I := Low(fScriptSoundIndex) to High(fScriptSoundIndex) do
    if fScriptSoundIndex[I] = -1 then
    begin
      fScriptSoundIndex[I] := PlaySound(sfx_None, aFile, aLoc, aAttenuate, aVolume, aRadius, aFadeMusic, aLooped, True);
      if fScriptSoundIndex[I] <> -1 then
        Result := I; //Successfully playing
      Exit;
    end;
end;


procedure TKMSoundPlayer.StopScriptSound(aIndex: Integer);
begin
  if not fIsSoundInitialized then Exit;
  if fScriptSoundIndex[aIndex] = -1 then Exit;

  //Stop previously playing sound and release buffer
  AlSourceStop(fSound[fScriptSoundIndex[aIndex]].ALSource);
  AlSourcei(fSound[fScriptSoundIndex[aIndex]].ALSource, AL_BUFFER, 0);

  fSound[fScriptSoundIndex[aIndex]].PlaySince := 0;
  fScriptSoundIndex[aIndex] := -1;
end;


procedure TKMSoundPlayer.AbortAllScriptSounds;
var I: Integer;
begin
  for I := Low(fScriptSoundIndex) to High(fScriptSoundIndex) do
    StopScriptSound(I);
end;


function TKMSoundPlayer.IsSoundPlaying(aIndex: Integer): Boolean;
begin
  Result := (fSound[aIndex].PlaySince <> 0)
      and ((GetTimeSince(fSound[aIndex].PlaySince) < fSound[aIndex].Duration) or fSound[aIndex].Looped)
end;


function TKMSoundPlayer.ActiveCount: Byte;
var I: Integer;
begin
  Result := 0;
  for I := 1 to MAX_SOUNDS do
    if IsSoundPlaying(I) then
      Inc(Result)
    else
      fSound[I].PlaySince := 0;
end;


procedure TKMSoundPlayer.Paint;
var I: Integer;
begin
  gRenderAux.CircleOnTerrain(fListener.Pos[1], fListener.Pos[2], MAX_DISTANCE, $00000000, $FFFFFFFF);
  for I := 1 to MAX_SOUNDS do
    if IsSoundPlaying(I) then
    begin
      gRenderAux.CircleOnTerrain(fSound[I].Position.X, fSound[I].Position.Y, 5, $4000FFFF, $FFFFFFFF);
      gRenderAux.Text(Round(fSound[I].Position.X), Round(fSound[I].Position.Y), fSound[I].Name, $FFFFFFFF);
    end else
      fSound[I].PlaySince := 0;
end;


procedure TKMSoundPlayer.UpdateStateIdle;
var
  I: Integer;
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
    fOnUnfadeMusic(False);
end;


{ TKMLoopSoundsManager }
destructor TKMScriptSoundsManager.Destroy;
begin
  gSoundPlayer.AbortAllScriptSounds;

  inherited;
end;


procedure TKMScriptSoundsManager.UpdateState;
var
  I: Integer;
begin
  //Check whether a sound needs starting or stopping
  for I := 0 to fCount - 1 do
    if fSounds[I].Looped then
    begin
      if (fSounds[I].PlayingIndex = -1) and CanPlay(I) then
        StartSound(I)
      else
        if (fSounds[I].PlayingIndex <> -1) and not CanPlay(I) then
          StopSound(I);
    end;
end;


function TKMScriptSoundsManager.CanPlay(aIndex: Integer): Boolean;
var
  DistanceSqr: Single;
begin
  Result := ((fSounds[aIndex].HandIndex = gMySpectator.HandIndex) or (fSounds[aIndex].HandIndex = PLAYER_NONE))
             and (not fSounds[aIndex].Attenuate or (gMySpectator.FogOfWar.CheckTileRevelation(fSounds[aIndex].Loc.X, fSounds[aIndex].Loc.Y) > 0));
  if not Result then Exit;

  if fSounds[aIndex].Attenuate then
  begin
    DistanceSqr := KMDistanceSqr(KMPointF(fSounds[aIndex].Loc), fListener);
    Result := Result and (DistanceSqr < Sqr(fSounds[aIndex].Radius));
  end;
end;


procedure TKMScriptSoundsManager.StartSound(aIndex: Integer);
var S: UnicodeString;
begin
  S := ExeDir + gGame.GetScriptSoundFile(fSounds[aIndex].SoundName, fSounds[aIndex].AudioFormat);

  //Silently ignore missing files
  if not FileExists(S) or not CanPlay(aIndex) then
    Exit;

  fSounds[aIndex].PlayingIndex := gSoundPlayer.PlayScriptSound(S, KMPointF(fSounds[aIndex].Loc), fSounds[aIndex].Attenuate,
                                                               fSounds[aIndex].Volume, fSounds[aIndex].Radius,
                                                               fSounds[aIndex].FadeMusic, fSounds[aIndex].Looped);
end;


procedure TKMScriptSoundsManager.StopSound(aIndex: Integer);
begin
  if fSounds[aIndex].PlayingIndex = -1 then Exit;

  gSoundPlayer.StopScriptSound(fSounds[aIndex].PlayingIndex);
  fSounds[aIndex].PlayingIndex := -1;
end;


function TKMScriptSoundsManager.AddSound(aHandIndex: TKMHandIndex; const aSoundName: AnsiString; aSoundFormat: TKMAudioFormat;
                                           aLoc: TKMPoint; aAttenuate: Boolean; aVolume: Single; aRadius: Single; aFadeMusic, aLooped: Boolean): Integer;
var
  NewIndex: Integer;
begin
  Inc(fCount);
  Inc(fLastScriptIndex);
  NewIndex := fCount-1;
  if Length(fSounds) < fCount then
    SetLength(fSounds, fCount+8);

  fSounds[NewIndex].ScriptIndex := fLastScriptIndex;
  fSounds[NewIndex].PlayingIndex := -1;
  fSounds[NewIndex].Looped := aLooped;
  fSounds[NewIndex].FadeMusic := aFadeMusic;
  fSounds[NewIndex].SoundName := aSoundName;
  fSounds[NewIndex].AudioFormat := aSoundFormat;
  fSounds[NewIndex].Loc := aLoc;
  fSounds[NewIndex].Attenuate := aAttenuate;
  fSounds[NewIndex].Volume := aVolume;
  fSounds[NewIndex].Radius := aRadius;
  fSounds[NewIndex].HandIndex := aHandIndex;

  StartSound(NewIndex);
  Result := fSounds[NewIndex].ScriptIndex;
end;


procedure TKMScriptSoundsManager.RemoveLoopSound(aScriptIndex: Integer);
begin
  RemoveSound(aScriptIndex, True);
end;


procedure TKMScriptSoundsManager.RemoveSound(aScriptIndex: Integer; aLoopedOnly: Boolean = False);
var
  I: Integer;
begin
  for I := 0 to fCount - 1 do
    if (not aLoopedOnly or fSounds[I].Looped) and (fSounds[I].ScriptIndex = aScriptIndex) then
    begin
      StopSound(I);

      if I <> fCount - 1 then
        Move(fSounds[I+1], fSounds[I], (fCount - 1 - I) * SizeOf(fSounds[I]));

      Dec(fCount);
      Exit; //Only one sound will have this aScriptIndex
    end;
end;


procedure TKMScriptSoundsManager.UpdateListener(X,Y: Single);
begin
  fListener := KMPointF(X, Y);
end;


procedure TKMScriptSoundsManager.Save(SaveStream: TKMemoryStream);
var
  I: Integer;
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


procedure TKMScriptSoundsManager.Load(LoadStream: TKMemoryStream);
var
  I: Integer;
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
