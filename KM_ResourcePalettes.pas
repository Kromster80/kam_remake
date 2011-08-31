unit KM_ResourcePalettes;
{$I KaM_Remake.inc}
interface
uses
  Classes, SysUtils,
  KM_CommonTypes, KM_Defaults;


type
  TKMPalData = class
    fData: array [0..255,1..3] of byte;
  public
    procedure LoadFromFile(const aFileName: string);
    function Color32(aIdx:byte):Cardinal;
  end;

  TKMPalettes = class
  private
    fPalData:array [TKMPal] of TKMPalData;
    function GetPalData(aIndex: TKMPal): TKMPalData;
  public
    constructor Create;
    destructor Destroy; override;

    procedure LoadPalettes;
    property PalData[aIndex: TKMPal]:TKMPalData read GetPalData; default;
    function PalFile(aIndex: TKMPal):string;
  end;


implementation


const
  //Palette filenames, except pal_lin which is generated proceduraly
  PalFiles:array[TKMPal]of string = (
    'map.bbm',
    'pal0.bbm', //'pal1.bbm', 'pal2.bbm', 'pal3.bbm', 'pal4.bbm', 'pal5.bbm', unused
    'setup.bbm',
    'setup2.bbm',
    'linear',
    'mapgold.lbm',
    'setup.lbm');


{ TKMPalData }
function TKMPalData.Color32(aIdx:byte): Cardinal;
begin
  Result := fData[aIdx,1] + fData[aIdx,2] shl 8 + fData[aIdx,3] shl 16 + (byte(aIdx<>0)*255 shl 24);
end;


procedure TKMPalData.LoadFromFile(const aFileName: string);
var
  i:integer;
  S:TKMemoryStream;
begin
  if FileExists(aFileName) then
  begin
    S := TKMemoryStream.Create;
    S.LoadFromFile(aFileName);
    S.Seek(48, soFromBeginning);
    S.Read(fData, SizeOf(fData)); //768bytes
    S.Free;
  end else
    for i:=0 to 255 do //Gradiant palette for missing files (used by pal_lin)
    begin
      fData[i,1] := i;
      fData[i,2] := i;
      fData[i,3] := i;
    end;
end;


{ TKMPalettes }
constructor TKMPalettes.Create;
var i:TKMPal;
begin
  Inherited Create;

  for i:=Low(TKMPal) to High(TKMPal) do
    fPalData[i] := TKMPalData.Create;
end;


destructor TKMPalettes.Destroy;
var i:TKMPal;
begin
  for i:=Low(TKMPal) to High(TKMPal) do
    fPalData[i].Free;

  inherited;
end;


function TKMPalettes.GetPalData(aIndex: TKMPal): TKMPalData;
begin
  Result := fPalData[aIndex];
end;


procedure TKMPalettes.LoadPalettes;
var i:TKMPal;
begin
  for i:=Low(TKMPal) to High(TKMPal) do
    fPalData[i].LoadFromFile(ExeDir+'data\gfx\'+PalFiles[i]);
end;


function TKMPalettes.PalFile(aIndex: TKMPal): string;
begin
  Result := PalFiles[aIndex];
end;


end.
