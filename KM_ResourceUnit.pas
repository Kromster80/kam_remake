unit KM_ResourceUnit;
{$I KaM_Remake.inc}
interface
uses
  Classes, Math, SysUtils, KM_CommonTypes, KM_Points, KM_Defaults;


type
  TKMUnitsAnim = packed record
    Step:array[1..30]of smallint;
    Count:smallint;
    MoveX,MoveY:integer;
  end;


  TKMUnitDatClass = class
  private
    fUnitType: TUnitType;
  public
    constructor Create(aType:TUnitType);
  end;


  TKMUnitDatCollection = class
  private
    fCRC:cardinal;
    fItems: array[TUnitType] of TKMUnitDatClass;
    fSerfCarry: array[rt_Trunk..rt_Fish, dir_N..dir_NW] of TKMUnitsAnim;
    function LoadUnitsDat(aPath: string):Cardinal;
    function GetUnitDat(aType:TUnitType):TKMUnitDatClass;
    function GetSerfCarry(aType:TResourceType; aDir:TKMDirection):TKMUnitsAnim;
  public
    constructor Create;
    destructor Destroy; override;

    property UnitsDat[aType:TUnitType]:TKMUnitDatClass read GetUnitDat; default;
    property SerfCarry[aType:TResourceType; aDir:TKMDirection]:TKMUnitsAnim read GetSerfCarry;
    property CRC:cardinal read fCRC; //Return hash of all values

    procedure ExportCSV(aPath: string);
  end;



implementation
uses KromUtils, KM_Render, KM_TGATexture, KM_Log, KM_Utils;


{ TKMUnitsDatClass }
constructor TKMUnitDatClass.Create(aType: TUnitType);
begin
  Inherited Create;
  fUnitType := aType;
end;


{ TKMUnitsDatCollection }
constructor TKMUnitDatCollection.Create;
var U:TUnitType;
begin
  Inherited;

  for U := Low(TUnitType) to High(TUnitType) do
    fItems[U] := TKMUnitDatClass.Create(U);

  fCRC := LoadUnitsDat(ExeDir+'data\defines\unit.dat');
  //ExportCSV(ExeDir+'Houses.csv');
end;


destructor TKMUnitDatCollection.Destroy;
var U:TUnitType;
begin
  for U := Low(TUnitType) to High(TUnitType) do
    fItems[U].Free;

  inherited;
end;


procedure TKMUnitDatCollection.ExportCSV(aPath: string);
begin
  //
end;


function TKMUnitDatCollection.GetSerfCarry(aType: TResourceType; aDir: TKMDirection): TKMUnitsAnim;
begin
  //Assert(aType in )
  Result := fSerfCarry[aType, aDir];
end;


function TKMUnitDatCollection.GetUnitDat(aType: TUnitType): TKMUnitDatClass;
begin
  Result := fItems[aType];
end;


function TKMUnitDatCollection.LoadUnitsDat(aPath: string): Cardinal;
var
  S:TKMemoryStream;
  i:integer;
begin
  Assert(FileExists(aPath));

  S := TKMemoryStream.Create;
  try
    S.LoadFromFile(aPath);

    S.Read(fSerfCarry, SizeOf(fSerfCarry){28*8*70});

    Result := Adler32CRC(S);
  finally
    S.Free;
  end;
end;


end.
