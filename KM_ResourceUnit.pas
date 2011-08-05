unit KM_ResourceUnit;
{$I KaM_Remake.inc}
interface
uses
  Classes, Math, SysUtils, KM_CommonTypes, KM_Points, KM_Defaults;


//Used to separate close-combat units from archers (they use different fighting logic)
type
  TFightType = (ft_Melee, ft_Ranged);


type
  TKMUnitsAnim = packed record
    Step:array[1..30]of smallint;
    Count:smallint;
    MoveX,MoveY:integer;
  end;

  TKMUnitDat = packed record
    HitPoints,Attack,AttackHorseBonus,x4,Defence,Speed,x7,Sight:smallint;
    x9,x10:shortint;
    CanWalkOut,x11:smallint;
  end;

  TKMUnitSprite = packed record
    Act:array[TUnitActionType]of packed record
      Dir:array[dir_N..dir_NW]of TKMUnitsAnim;
    end;
  end;

  TKMUnitSprite2 = array[1..18]of smallint; //Sound indices vs sprite ID

  TKMUnitDatClass = class
  private
    fUnitType: TUnitType;
    fUnitDat: TKMUnitDat;
    fUnitSprite:TKMUnitSprite;
    fUnitSprite2:TKMUnitSprite2;
    function GetGUIIcon:word;
    function GetGUIScroll:word;
    function GetSpeed:single;
    function GetUnitAnim(aAction:TUnitActionType; aDir:TKMDirection):TKMUnitsAnim;
    function GetUnitDescription: string;
    function GetUnitName: string;
    function GetFightType: TFightType;
  public
    constructor Create(aType:TUnitType);
    function IsValid:boolean;
    function IsAnimal: boolean;
    procedure LoadFromStream(Stream:TMemoryStream);
    //Derived from KaM
    property HitPoints:smallint read fUnitDat.HitPoints;
    property Attack:smallint read fUnitDat.Attack;
    property AttackHorseBonus:smallint read fUnitDat.AttackHorseBonus;
    property Defence:smallint read fUnitDat.Defence;
    property Sight:smallint read fUnitDat.Sight;
    //Additional properties added by Remake
    property FightType:TFightType read GetFightType;
    property GUIIcon:word read GetGUIIcon;
    property GUIScroll:word read GetGUIScroll;
    property Speed:single read GetSpeed;
    //todo: Replace Bytes with native Types
    property UnitAnim[aAction:TUnitActionType; aDir:TKMDirection]:TKMUnitsAnim read GetUnitAnim;
    property UnitDescription:string read GetUnitDescription;
    property UnitName:string read GetUnitName;
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


const
  School_Order:array[0..13] of TUnitType = (
    ut_Serf, ut_Worker, ut_StoneCutter, ut_Woodcutter, ut_Lamberjack,
    ut_Fisher, ut_Farmer, ut_Baker, ut_AnimalBreeder, ut_Butcher,
    ut_Miner, ut_Metallurgist, ut_Smith, ut_Recruit);

  Barracks_Order:array[0..8] of TUnitType = (
    ut_Militia, ut_AxeFighter, ut_Swordsman, ut_Bowman, ut_Arbaletman,
    ut_Pikeman, ut_Hallebardman, ut_HorseScout, ut_Cavalry);

  UnitDatCount = 41;
  UnitKaMType: array[0..UnitDatCount-1] of TUnitType = (
  {0..13}
  ut_Serf, ut_Woodcutter, ut_Miner, ut_AnimalBreeder, ut_Farmer,
  ut_Lamberjack, ut_Baker, ut_Butcher, ut_Fisher, ut_Worker,
  ut_StoneCutter, ut_Smith, ut_Metallurgist, ut_Recruit,
  {14..23}
  ut_Militia, ut_AxeFighter, ut_Swordsman, ut_Bowman, ut_Arbaletman,
  ut_Pikeman, ut_Hallebardman, ut_HorseScout, ut_Cavalry, ut_Barbarian,
  {24..29}
  ut_None, ut_None, ut_None, ut_None, ut_None,
  ut_None,
  //ut_Peasant, ut_Slingshot, ut_MetalBarbarian, ut_Horseman, ut_Catapult, ut_Ballista,
  {30..37}
  ut_Wolf, ut_Fish, ut_Watersnake, ut_Seastar, ut_Crab,
  ut_Waterflower, ut_Waterleaf, ut_Duck,
  {38..40}
  ut_None, ut_None, ut_None);

  UnitKaMOrder: array[TUnitType] of byte = (0, 0,
  1, 2, 3, 4, 5, 6, 7, 8, 9,
  10, 11, 12, 13, 14, 15, 16, 17, 18, 19,
  20, 21, 22, 23, 24, {25, 26, 27, 28, 29,
  30,} 31, 32, 33, 34, 35, 36, 37, 38);

  
implementation
uses KromUtils, KM_Render, KM_TGATexture, KM_Log, KM_Utils, KM_TextLibrary;


{ TKMUnitsDatClass }
constructor TKMUnitDatClass.Create(aType: TUnitType);
begin
  Inherited Create;
  fUnitType := aType;
end;


function TKMUnitDatClass.IsValid: boolean;
begin
  Result := not (fUnitType in [ut_None, ut_Any]);
end;


function TKMUnitDatClass.IsAnimal: boolean;
begin
  Result := fUnitType in [ut_Wolf..ut_Duck];
end;


procedure TKMUnitDatClass.LoadFromStream(Stream: TMemoryStream);
begin
  Stream.Read(fUnitDat, SizeOf(TKMUnitDat));
  Stream.Read(fUnitSprite, SizeOf(TKMUnitSprite));
  Stream.Read(fUnitSprite2, SizeOf(TKMUnitSprite2));
end;


function TKMUnitDatClass.GetGUIIcon: word;
begin
  if IsValid then
    Result := 140 + UnitKaMOrder[fUnitType]
  else
    Result := 0;
end;


function TKMUnitDatClass.GetGUIScroll: word;
begin
  if IsValid then
    Result := 520 + UnitKaMOrder[fUnitType]
  else
    Result := 0;
end;


function TKMUnitDatClass.GetSpeed: single;
begin
  Result := fUnitDat.Speed / 240;
end;


function TKMUnitDatClass.GetUnitAnim(aAction: TUnitActionType; aDir: TKMDirection): TKMUnitsAnim;
begin
  Assert(aDir <> dir_NA);
  Assert(aAction in [Low(TUnitActionType)..High(TUnitActionType)]);
  Result := fUnitSprite.Act[aAction].Dir[aDir];
end;


function TKMUnitDatClass.GetUnitName: string;
begin
  if IsValid then
    case fUnitType of
      ut_Wolf:        Result := 'Wolf';
      ut_Fish:        Result := 'Fish';
      ut_Watersnake:  Result := 'Watersnake';
      ut_Seastar:     Result := 'Seastar';
      ut_Crab:        Result := 'Crab';
      ut_Waterflower: Result := 'Waterflower';
      ut_Waterleaf:   Result := 'Waterleaf';
      ut_Duck:        Result := 'Duck';
      else            Result := fTextLibrary.GetTextString(siUnitNames + UnitKaMOrder[fUnitType]);
    end
  else
    Result := 'N/A';
end;


function TKMUnitDatClass.GetUnitDescription: string;
begin
  if IsValid and not IsAnimal then
    Result := fTextLibrary.GetTextString(siUnitDescriptions + UnitKaMOrder[fUnitType])
  else
    Result := 'N/A';
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

    for i:=0 to UnitDatCount-1 do
    if UnitKaMType[i] <> ut_None then
      fItems[UnitKaMType[i]].LoadFromStream(S)
    else
      S.Seek(SizeOf(TKMUnitDat) + SizeOf(TKMUnitSprite) + SizeOf(TKMUnitSprite2), soFromCurrent);


    Result := Adler32CRC(S);
  finally
    S.Free;
  end;
end;


function TKMUnitDatClass.GetFightType: TFightType;
const WarriorFightType: array[ut_Militia..ut_Barbarian] of TFightType = (
    ft_Melee,ft_Melee,ft_Melee, //Militia, AxeFighter, Swordsman
    ft_Ranged,ft_Ranged,        //Bowman, Arbaletman
    ft_Melee,ft_Melee,          //Pikeman, Hallebardman,
    ft_Melee,ft_Melee,          //HorseScout, Cavalry,
    ft_Melee                    //Barbarian
    {ft_Melee,           //Peasant
    ft_Ranged,           //ut_Slingshot
    ft_Melee,            //ut_MetalBarbarian
    ft_Melee,            //ut_Horseman
    ft_Ranged,ft_Ranged, //ut_Catapult, ut_Ballista,}
    );
begin
  Assert(fUnitType in [Low(WarriorFightType)..High(WarriorFightType)]);
  Result := WarriorFightType[fUnitType];
end;


end.
