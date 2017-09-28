unit KM_ResPalettes;
{$I KaM_Remake.inc}
interface
uses
  Classes, SysUtils,
  KM_CommonClasses;


type
  TKMPal = (
    pal_map,
    pal_0, //pal_1, pal_2, pal_3, pal_4, pal_5, unused since we change brightness with OpenGL overlay
    pal_set,
    pal_set2,
    pal_bw,
    pal_lin,
    pal2_mapgold,
    pal2_setup);

  // Individual palette info
  TKMPaletteInfo = class
  private
    fData: array [0..255, 1..3] of Byte;
    procedure GenerateBW;
    procedure GenerateLinear;
  public
    procedure LoadFromFile(const aFileName: UnicodeString);
    function Color32(aIdx: Byte): Cardinal;
  end;

  // All the palettes
  TKMResPalettes = class
  private
    fPalettes: array [TKMPal] of TKMPaletteInfo;
    function GetPalette(aIndex: TKMPal): TKMPaletteInfo;
  public
    constructor Create;
    destructor Destroy; override;

    procedure LoadPalettes(const aPath: UnicodeString);
    procedure LoadDefaultPalette(const aPath: UnicodeString);
    property Palettes[aIndex: TKMPal]: TKMPaletteInfo read GetPalette; default;
    function DefaultPalette: TKMPaletteInfo; //Default palette for the game
    function PalFile(aIndex: TKMPal): UnicodeString;
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


{ TKMPaletteInfo }
function TKMPaletteInfo.Color32(aIdx: Byte): Cardinal;
begin
  //Index 0 means that pixel is transparent
  if aIdx = 0 then
    Result := fData[aIdx,1] + fData[aIdx,2] shl 8 + fData[aIdx,3] shl 16 //$00000000
  else
    Result := fData[aIdx,1] + fData[aIdx,2] shl 8 + fData[aIdx,3] shl 16 or $FF000000;
end;


//Black-and-white palette for fonts
procedure TKMPaletteInfo.GenerateBW;
begin
  FillChar(fData, SizeOf(fData), #255);
  fData[0, 1] := 0;
  fData[0, 2] := 0;
  fData[0, 3] := 0;
end;


//Gradient palette for missing files (used by pal_lin)
procedure TKMPaletteInfo.GenerateLinear;
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


procedure TKMPaletteInfo.LoadFromFile(const aFileName: UnicodeString);
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


{ TKMResPalettes }
constructor TKMResPalettes.Create;
var
  I: TKMPal;
begin
  inherited Create;

  for I := Low(TKMPal) to High(TKMPal) do
    fPalettes[I] := TKMPaletteInfo.Create;
end;


destructor TKMResPalettes.Destroy;
var
  I: TKMPal;
begin
  for I := Low(TKMPal) to High(TKMPal) do
    fPalettes[I].Free;

  inherited;
end;


function TKMResPalettes.DefaultPalette: TKMPaletteInfo;
begin
  // Default palette to use when generating full-color RGB textures
  Result := fPalettes[pal_0];
end;


function TKMResPalettes.GetPalette(aIndex: TKMPal): TKMPaletteInfo;
begin
  Result := fPalettes[aIndex];
end;


procedure TKMResPalettes.LoadPalettes(const aPath: UnicodeString);
var
  I: TKMPal;
begin
  for I := Low(TKMPal) to High(TKMPal) do
  case I of
    pal_bw:   fPalettes[I].GenerateBW;
    pal_lin:  fPalettes[I].GenerateLinear;
    else      fPalettes[I].LoadFromFile(aPath + PalFiles[I]);
  end;
end;


//Load only Default Palette
procedure TKMResPalettes.LoadDefaultPalette(const aPath: UnicodeString);
begin
  fPalettes[pal_0].LoadFromFile(aPath + PalFiles[pal_0]);
end;


function TKMResPalettes.PalFile(aIndex: TKMPal): UnicodeString;
begin
  Result := PalFiles[aIndex];
end;


end.
