unit KM_LoadSFX;
interface
uses Windows, Classes, SysUtils, KromUtils, OpenAL, KM_Defaults;

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
    Waves: array[1..MaxWaves] of record
      Head: TWAVHeaderEx;
      Data: array of char;
      Foot: array of char;
    end;
    Props: array[1..MaxWaves] of record
      SampleRate,Volume:integer;
      a,b:integer;
      i,j,k,l,Index:word;
    end;
    Listener:record
      Pos: array [1..3] of TALfloat;
      Vel: array [1..3] of TALfloat;
      Ori: array [1..6] of TALfloat;
    end;
    //Buffer used to store the wave data, Source is sound position in space
    ALSource,ALBuffer: array [1..64] of TALuint;
    procedure LoadSoundsDAT();
  public
    constructor Create;
    procedure ExportSounds();
    procedure UpdateListener(Pos:TKMPoint);
    procedure Play(SoundID:TSoundFX; Loc:TKMPoint; Attenuated:boolean; const TimesToPlay:byte=1);
end;

var
  fSoundLib: TSoundLib;

implementation

constructor TSoundLib.Create;
var
  argv: array of PalByte;
begin
  Inherited Create;
  InitOpenAL;
  AlutInit(nil,argv);
  alDistanceModel(AL_LINEAR_DISTANCE_CLAMPED);
  LoadSoundsDAT();
  AlGenBuffers(32, @ALBuffer); //64 looks like the limit
  AlGenSources(32, @ALSource);
end;

procedure TSoundLib.LoadSoundsDAT();
var
  f:file;
  Head:record Size,Count:word; end;
  Tab1:array[1..200]of integer;
  Tab2:array[1..200]of smallint;
  i,Tmp:integer;
  c: array[1..100000] of char;
begin
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
  for i:=1 to Head.Count do
    BlockRead(f,Props[i],26);

  CloseFile(f);
end;


procedure TSoundLib.ExportSounds();
var f:file; i:integer;
begin
  CreateDir(ExeDir+'Sounds.dat\');
  for i:=1 to MaxWaves do if length(Waves[i].Data)>0 then begin
    assignfile(f,ExeDir+'Sounds.dat\sound_'+int2fix(i,3)+'.wav'); rewrite(f,1);
    blockwrite(f,Waves[i].Head,SizeOf(Waves[i].Head));
    blockwrite(f,Waves[i].Data[0],length(Waves[i].Data));
    blockwrite(f,Waves[i].Foot[0],length(Waves[i].Foot));
    closefile(f);
  end;
end;

procedure TSoundLib.UpdateListener(Pos:TKMPoint);
begin
  Listener.Pos[1]:=Pos.X;
  Listener.Pos[2]:=Pos.Y;
  Listener.Pos[3]:=0;
  AlListenerfv ( AL_POSITION, @Listener.Pos);

  Listener.Ori[1]:=0; Listener.Ori[2]:=1; Listener.Ori[3]:=0; //Look-at vector
  Listener.Ori[4]:=0; Listener.Ori[5]:=0; Listener.Ori[6]:=1; //Up vector
  AlListenerfv ( AL_ORIENTATION, @Listener.Ori);
end;


{Call to this procedure will find a right spot and start to play sound immediately}
{Will need to make another one for unit sounds, which will take WAV file path as parameter}
procedure TSoundLib.Play(SoundID:TSoundFX; Loc:TKMPoint; Attenuated:boolean; const TimesToPlay:byte=1);
var Dif:array[1..3]of single; FreeBuf,ID:integer; i:integer; ALState:TALint;
begin
  //Find free buffer and use it
  FreeBuf:=1;
  for i:=1 to MaxSourceCount do begin
    alGetSourcei(ALSource[i], AL_SOURCE_STATE, @ALState);
    if ALState<>AL_PLAYING then begin
      FreeBuf:=i;
      break;
    end;
  end;

  if i>=MaxSourceCount then exit;//FreeBuf:=MaxSourceCount;

  ID:=word(SoundID);

  //Stope previously playing sound and release buffer
  AlSourceStop(ALSource[FreeBuf]);
  AlSourcei ( ALSource[FreeBuf], AL_BUFFER, 0);

  //Assign new data to buffer and assign it to source
  AlBufferData(ALBuffer[FreeBuf], AL_FORMAT_MONO8, @Waves[ID].Data[0], Waves[ID].Head.DataSize, Waves[ID].Head.SampleRate);

  //Set source properties
  AlSourcei ( ALSource[FreeBuf], AL_BUFFER, ALBuffer[FreeBuf]);
  AlSourcef ( ALSource[FreeBuf], AL_PITCH, 1.0 );
  AlSourcef ( ALSource[FreeBuf], AL_GAIN, 1.0 );
  if Attenuated then begin
    Dif[1]:=Loc.X; Dif[2]:=Loc.Y; Dif[3]:=0;
    AlSourcefv( ALSource[FreeBuf], AL_POSITION, @Dif[1]);
  end else
    AlSourcefv( ALSource[FreeBuf], AL_POSITION, @Listener.Pos);
  AlSourcef ( ALSource[FreeBuf], AL_REFERENCE_DISTANCE, 5.0 );
  AlSourcef ( ALSource[FreeBuf], AL_MAX_DISTANCE, 15.0 );
  AlSourcef ( ALSource[FreeBuf], AL_ROLLOFF_FACTOR, 1.0 );
  AlSourcei ( ALSource[FreeBuf], AL_LOOPING, AL_FALSE);

  //Start playing
  AlSourcePlay(ALSource[FreeBuf]);
end;


end.
