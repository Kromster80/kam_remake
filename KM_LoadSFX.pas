unit KM_LoadSFX;
interface
uses Windows, Classes, SysUtils, KromUtils;

const MaxWaves = 200;

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
end;

var
  fSoundLibrary: TSoundLibrary;

implementation
uses KM_Global_Data;

constructor TSoundLibrary.Create;
begin
  Inherited Create;
  LoadSoundsDAT();
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


end.
