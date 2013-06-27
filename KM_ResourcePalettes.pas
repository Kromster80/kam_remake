unit KM_ResourcePalettes;
{$I KaM_Remake.inc}
interface
uses
  Classes, SysUtils,
  KM_CommonClasses, KM_Defaults;


type
  //There are 9 palette files: Map, Pal0-5, Setup, Setup2, gradient, 2lbm palettes
  TKMPal = (
    pal_map,
    pal_0, //pal_1, pal_2, pal_3, pal_4, pal_5, unused since we change brightness with OpenGL overlay
    pal_set,
    pal_set2,
    pal_bw,
    pal_lin,
    pal2_mapgold,
    pal2_setup);

  //Individual palette
  TKMPalData = class
    fData: array [0..255, 1..3] of Byte;
  public
    procedure GenerateBW;
    procedure GenerateLinear;
    procedure LoadFromFile(const aFileName: string);
    function Color32(aIdx: Byte): Cardinal;
  end;

  //All the palettes
  TKMPalettes = class
  private
    fPalData: array [TKMPal] of TKMPalData;
    function GetPalData(aIndex: TKMPal): TKMPalData;
  public
    constructor Create;
    destructor Destroy; override;

    procedure LoadPalettes(aPath: string);
    property PalData[aIndex: TKMPal]: TKMPalData read GetPalData; default;
    function DefDal: TKMPalData; //Default palette for the game
    function PalFile(aIndex: TKMPal):string;
  end;


implementation


const
  //Palette filenames, except pal_lin which is generated proceduraly
  PalFiles: array [TKMPal] of string = (
    'map.bbm',
    'pal0.bbm', //'pal1.bbm', 'pal2.bbm', 'pal3.bbm', 'pal4.bbm', 'pal5.bbm', unused
    'setup.bbm',
    'setup2.bbm',
    '', //Black`n`white
    '', //Linear
    'mapgold.lbm',
    'setup.lbm');


{ TKMPalData }
function TKMPalData.Color32(aIdx: Byte): Cardinal;
begin
  //Index 0 means that pixel is transparent
  if aIdx = 0 then
    Result := $00000000
  else
    Result := fData[aIdx,1] + fData[aIdx,2] shl 8 + fData[aIdx,3] shl 16 or $FF000000;
end;


//Black-and-white palette for fonts
procedure TKMPalData.GenerateBW;
begin
  FillChar(fData, SizeOf(fData), #255);
  fData[0, 1] := 0;
  fData[0, 2] := 0;
  fData[0, 3] := 0;
end;


//Gradient palette for missing files (used by pal_lin)
procedure TKMPalData.GenerateLinear;
var
  I: Byte;
begin
  for I := 0 to 255 do
  begin
    fData[I, 1] := I;
    fData[I, 2] := I;
    fData[I, 3] := I;
  end;
end;


procedure TKMPalData.LoadFromFile(const aFileName: string);
var
  S: TKMemoryStream;
begin
  if FileExists(aFileName) then
  begin
    S := TKMemoryStream.Create;
    S.LoadFromFile(aFileName);
    S.Seek(48, soFromBeginning);
    S.Read(fData, SizeOf(fData)); //768bytes
    S.Free;
  end else
    GenerateLinear;
end;


{ TKMPalettes }
constructor TKMPalettes.Create;
var
  I: TKMPal;
begin
  inherited Create;

  for I := Low(TKMPal) to High(TKMPal) do
    fPalData[I] := TKMPalData.Create;
end;


destructor TKMPalettes.Destroy;
var
  I: TKMPal;
begin
  for I := Low(TKMPal) to High(TKMPal) do
    fPalData[I].Free;

  inherited;
end;


function TKMPalettes.DefDal: TKMPalData;
begin
  //Default palette to use when generating full-color RGB textures
  Result := fPalData[pal_0];
end;


function TKMPalettes.GetPalData(aIndex: TKMPal): TKMPalData;
begin
  Result := fPalData[aIndex];
end;


procedure TKMPalettes.LoadPalettes(aPath: string);
var
  I: TKMPal;
begin
  for I := Low(TKMPal) to High(TKMPal) do
  case I of
    pal_bw:   fPalData[I].GenerateBW;
    pal_lin:  fPalData[I].GenerateLinear;
    else      fPalData[I].LoadFromFile(aPath + PalFiles[I]);
  end;
end;


function TKMPalettes.PalFile(aIndex: TKMPal): string;
begin
  Result := PalFiles[aIndex];
end;


end.
