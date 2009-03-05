unit KM_LoadSFX;
interface
uses Windows, Classes, SysUtils, KromUtils, OpenAL;

const MaxWaves = 200;
var
  ALBuffer : array [1..64] of TALuint;
  ALSource : array [1..64] of TALuint;
//  SourcePos: array [1..64,1..3] of TALfloat;
  SourceVel: array [1..64,1..3] of TALfloat;
  Listener:record
    Pos: array [1..3] of TALfloat;
    Vel: array [1..3] of TALfloat;
    Ori: array [1..6] of TALfloat;
  end;


type
  TSoundLibrary = class(TObject)
  private
    Waves: array[1..MaxWaves] of array of char;
    Props: array[1..MaxWaves] of record
      SampleRate,Volume:integer;
      a,b:integer;
      i,j,k,l,Index:word;
    end;
    procedure LoadSoundsDAT();
  public
    constructor Create;
    procedure ExportSounds();
    procedure UpdateListener(Pos:TKMPoint);
    procedure Play(ID:integer; Loc:TKMPoint; const TimesToPlay:byte=1);
end;

var
  fSoundLibrary: TSoundLibrary;

implementation
  uses KM_Defaults;

constructor TSoundLibrary.Create;
var
  argv: array of PalByte;
begin
  Inherited Create;
  InitOpenAL;
  AlutInit(nil,argv);
  LoadSoundsDAT();
  AlGenBuffers(32, @ALBuffer); //64 looks like the limit
  AlGenSources(32, @ALSource);
end;

procedure TSoundLibrary.LoadSoundsDAT();
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
      setlength(Waves[i],Tab1[i]+1);
      BlockRead(f,Waves[i,1],Tab1[i]);
    end;
  end;

  BlockRead(f,c,20);
  for i:=1 to Head.Count do
    BlockRead(f,Props[i],26);

  CloseFile(f);
end;


procedure TSoundLibrary.ExportSounds();
var f:file; i:integer;
begin
  CreateDir(ExeDir+'Sounds.dat\');
  for i:=1 to MaxWaves do if length(Waves[i])>0 then begin
    assignfile(f,ExeDir+'Sounds.dat\sound_'+int2fix(i,3)+'.wav'); rewrite(f,1);
    blockwrite(f,Waves[i,1],length(Waves[i]));
    closefile(f);
  end;
end;

procedure TSoundLibrary.UpdateListener(Pos:TKMPoint);
begin
  Listener.Pos[1]:=Pos.X;
  Listener.Pos[2]:=Pos.Y;
  Listener.Pos[3]:=0;
  AlListenerfv ( AL_POSITION, @Listener.Pos);
end;

procedure TSoundLibrary.Play(ID:integer; Loc:TKMPoint; const TimesToPlay:byte=1);
var Dif:array[1..3]of single; FreeBuf:integer;
  WAVHeader : record
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
  end;
begin
//Find free buffer and use it
FreeBuf:=1;

    AlBufferData(ALBuffer[FreeBuf], AL_FORMAT_MONO8, @Waves[ID,1], length(Waves[ID])-1, 11025);
          AlSourcei ( ALSource[FreeBuf], AL_BUFFER, ALBuffer[FreeBuf]);
//          AlSourcef ( ALSource[FreeBuf], AL_PITCH, 1.0 );
//          AlSourcef ( ALSource[FreeBuf], AL_GAIN, Props[ID].Volume );
          Dif[1]:=Loc.X; Dif[2]:=Loc.Y; Dif[3]:=0;
          AlSourcefv( ALSource[FreeBuf], AL_POSITION, @Dif[1]);
          AlSourcef ( ALSource[FreeBuf], AL_REFERENCE_DISTANCE, 10.0 );
//          AlSourcef ( ALSource[FreeBuf], AL_ROLLOFF_FACTOR, 0.85 );
          AlSourcei ( ALSource[FreeBuf], AL_LOOPING, AL_FALSE);
          AlSourcePlay(ALSource[FreeBuf]);
end;


end.
