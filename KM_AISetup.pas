unit KM_AISetup;
{$I KaM_Remake.inc}
interface
uses Classes, KromUtils, Math, SysUtils,
    KM_CommonClasses, KM_Points, KM_Defaults;


type
  TKMPlayerAISetup = class
  public
    Aggressiveness: Integer; //-1 means not used or default
    AutoBuild: Boolean;
    AutoDefend: Boolean;
    EquipRateLeather, EquipRateIron: Word; //Number of ticks between soldiers being equipped. Seperated into Leather/Iron to keep KaM compatibility.
    MaxSoldiers: Integer; //-1 means not used or default
    RecruitDelay: Cardinal; //Recruits (for barracks) can only be trained after this many ticks
    RecruitCount: Byte;
    SerfsPerHouse: Single;
    StartPosition: TKMPoint; //Defines roughly where to defend and build around
    TownDefence: Integer; //-1 means not used or default
    WorkerCount: Byte;

    constructor Create;
    function GetEquipRate(aUnit: TUnitType): Word;
    function WarriorsPerMinute(aArmy: TArmyType): Single;

    procedure Save(SaveStream: TKMemoryStream);
    procedure Load(LoadStream: TKMemoryStream);
  end;


implementation


{ TKMPlayerAISetup }
constructor TKMPlayerAISetup.Create;
begin
  inherited;

  //TSK/TPR properties
  Aggressiveness := 100; //No idea what the default for this is, it's barely used
  AutoBuild := True; //In KaM it is On by default, and most missions turn it off
  AutoDefend := False; //It did not exist in KaM, we add it, Off by default

  EquipRateIron := 500; //Measured in KaM: AI equips 1 iron soldier every ~50 seconds
  EquipRateLeather := 1000; //Measured in KaM: AI equips 1 leather soldier every ~100 seconds (if no iron one was already equipped)
  MaxSoldiers := -1; //No limit by default
  WorkerCount := 6;
  RecruitCount := 5; //This means the number in the barracks, watchtowers are counted seperately
  RecruitDelay := 0; //Can train at start
  SerfsPerHouse := 1;
  StartPosition := KMPoint(1,1);
  TownDefence := 100; //In KaM 100 is standard, although we don't completely understand this command
end;


function TKMPlayerAISetup.GetEquipRate(aUnit: TUnitType): Word;
begin
  if aUnit in WARRIORS_IRON then
    Result := EquipRateIron
  else
    Result := EquipRateLeather;
end;


function TKMPlayerAISetup.WarriorsPerMinute(aArmy: TArmyType): Single;
var
  EquipRate: Single;
begin
  case aArmy of
    atLeather:      EquipRate := EquipRateLeather;
    atIron:         EquipRate := EquipRateIron;
    atLeatherIron:  EquipRate := Max(EquipRateLeather, EquipRateIron);
    else            EquipRate := 0;
  end;

  //How many warriors we would need to equip per-minute
  Result := EnsureRange(600 / Max(EquipRate, 1), 0.1, 6);
end;


procedure TKMPlayerAISetup.Save(SaveStream: TKMemoryStream);
begin
  SaveStream.Write(Aggressiveness);
  SaveStream.Write(AutoBuild);
  SaveStream.Write(AutoDefend);
  SaveStream.Write(EquipRateLeather);
  SaveStream.Write(EquipRateIron);
  SaveStream.Write(MaxSoldiers);
  SaveStream.Write(RecruitDelay);
  SaveStream.Write(RecruitCount);
  SaveStream.Write(SerfsPerHouse);
  SaveStream.Write(StartPosition);
  SaveStream.Write(TownDefence);
  SaveStream.Write(WorkerCount);
end;


procedure TKMPlayerAISetup.Load(LoadStream: TKMemoryStream);
begin
  LoadStream.Read(Aggressiveness);
  LoadStream.Read(AutoBuild);
  LoadStream.Read(AutoDefend);
  LoadStream.Read(EquipRateLeather);
  LoadStream.Read(EquipRateIron);
  LoadStream.Read(MaxSoldiers);
  LoadStream.Read(RecruitDelay);
  LoadStream.Read(RecruitCount);
  LoadStream.Read(SerfsPerHouse);
  LoadStream.Read(StartPosition);
  LoadStream.Read(TownDefence);
  LoadStream.Read(WorkerCount);
end;


end.
