unit KM_AISetup;
{$I KaM_Remake.inc}
interface
uses
  KM_CommonClasses, KM_Defaults, KM_Points;


type
  // Common AI settings
  // Could be a record, but we want to have default values initialization in constructor
  TKMHandAISetup = class
  public
    Aggressiveness: Integer; //-1 means not used or default
    AutoAttack: Boolean;
    AutoRepair: Boolean;
    AutoBuild: Boolean;
    AutoDefend: Boolean;
    DefendAllies: Boolean;
    UnlimitedEquip: Boolean;
    ArmyType: TArmyType;
    EquipRateLeather, EquipRateIron: Word; //Number of ticks between soldiers being equipped. Seperated into Leather/Iron to keep KaM compatibility.
    MaxSoldiers: Integer; //-1 means not used or default
    RecruitDelay: Cardinal; //Recruits (for barracks) can only be trained after this many ticks
    RecruitCount: Byte;
    SerfsPerHouse: Single;
    StartPosition: TKMPoint; //Defines roughly where to defend and build around
    TownDefence: Integer; //-1 means not used or default
    WorkerCount: Byte;
    AutoAttackRange: Byte;

    constructor Create;
    function GetEquipRate(aUnit: TUnitType): Word;
    function WarriorsPerMinute(aArmy: TArmyType): Single; overload;
    function WarriorsPerMinute: Single; overload;

    procedure ApplyAgressiveBuilderSetup;

    procedure Save(SaveStream: TKMemoryStream);
    procedure Load(LoadStream: TKMemoryStream);
  end;


implementation
uses
  Math;


{ TKMHandAISetup }
constructor TKMHandAISetup.Create;
begin
  inherited;

  //TSK/TPR properties
  Aggressiveness := 100; //No idea what the default for this is, it's barely used
  AutoAttack := False; //It did not exist in KaM, we add it, Off by default
  AutoBuild := True; //In KaM it is On by default, and most missions turn it off
  AutoDefend := False; //It did not exist in KaM, we add it, Off by default
  DefendAllies := False; //It did not exist in KaM, we add it, Off by default (tested by Lewin, AI doesn't defend allies in TPR)

  ArmyType := atIronThenLeather; //By default make iron soldiers, and if that fails make leather (same as TPR)
  EquipRateIron := 500; //Measured in KaM: AI equips 1 iron soldier every ~50 seconds
  EquipRateLeather := 1000; //Measured in KaM: AI equips 1 leather soldier every ~100 seconds (if no iron one was already equipped)
  MaxSoldiers := -1; //No limit by default
  WorkerCount := 6;
  RecruitCount := 5; //This means the number in the barracks, watchtowers are counted seperately
  RecruitDelay := 0; //Can train at start
  SerfsPerHouse := 1;
  StartPosition := KMPoint(1,1);
  TownDefence := 100; //In KaM 100 is standard, although we don't completely understand this command
  AutoAttackRange := 4; //Measured in TPR
end;


function TKMHandAISetup.GetEquipRate(aUnit: TUnitType): Word;
begin
  if aUnit in WARRIORS_IRON then
    Result := EquipRateIron
  else
    Result := EquipRateLeather;
end;


function TKMHandAISetup.WarriorsPerMinute(aArmy: TArmyType): Single;

  function EquipRateToPerMin(EquipRate: Cardinal): Single;
  begin
    Result := EnsureRange(600 / Max(EquipRate, 1), 0.1, 6);
  end;

begin
  case aArmy of
    atIronThenLeather: Result := Max(EquipRateToPerMin(EquipRateLeather), EquipRateToPerMin(EquipRateIron));
    atLeather:         Result := EquipRateToPerMin(EquipRateLeather);
    atIron:            Result := EquipRateToPerMin(EquipRateIron);
    atIronAndLeather:  Result := EquipRateToPerMin(EquipRateLeather) + EquipRateToPerMin(EquipRateIron);
    else               Result := 0;
  end;
end;


function TKMHandAISetup.WarriorsPerMinute: Single;
begin
  Result := WarriorsPerMinute(ArmyType);
end;


//Used from MapEd to give multiplayer building maps an AI builder config
procedure TKMHandAISetup.ApplyAgressiveBuilderSetup;
begin
  SerfsPerHouse := 1;
  WorkerCount := 20;
  ArmyType := atIronAndLeather; //Mixed army
  EquipRateLeather := 500;
  EquipRateIron := 500;
  AutoBuild := True;
  AutoRepair := True;
  AutoAttack := True;
  AutoDefend := True;
  DefendAllies := True;
  UnlimitedEquip := True;
  StartPosition := KMPoint(1,1); //So it is overridden by auto attack
  MaxSoldiers := -1;
  RecruitDelay := 0;
  RecruitCount := 10;
  AutoAttackRange := 6;
end;


procedure TKMHandAISetup.Save(SaveStream: TKMemoryStream);
begin
  SaveStream.WriteA('AISetup');
  SaveStream.Write(Aggressiveness);
  SaveStream.Write(AutoAttack);
  SaveStream.Write(AutoBuild);
  SaveStream.Write(AutoRepair);
  SaveStream.Write(AutoDefend);
  SaveStream.Write(DefendAllies);
  SaveStream.Write(UnlimitedEquip);
  SaveStream.Write(ArmyType, SizeOf(ArmyType));
  SaveStream.Write(EquipRateLeather);
  SaveStream.Write(EquipRateIron);
  SaveStream.Write(MaxSoldiers);
  SaveStream.Write(RecruitDelay);
  SaveStream.Write(RecruitCount);
  SaveStream.Write(SerfsPerHouse);
  SaveStream.Write(StartPosition);
  SaveStream.Write(TownDefence);
  SaveStream.Write(WorkerCount);
  SaveStream.Write(AutoAttackRange);
end;


procedure TKMHandAISetup.Load(LoadStream: TKMemoryStream);
begin
  LoadStream.ReadAssert('AISetup');
  LoadStream.Read(Aggressiveness);
  LoadStream.Read(AutoAttack);
  LoadStream.Read(AutoBuild);
  LoadStream.Read(AutoRepair);
  LoadStream.Read(AutoDefend);
  LoadStream.Read(DefendAllies);
  LoadStream.Read(UnlimitedEquip);
  LoadStream.Read(ArmyType, SizeOf(ArmyType));
  LoadStream.Read(EquipRateLeather);
  LoadStream.Read(EquipRateIron);
  LoadStream.Read(MaxSoldiers);
  LoadStream.Read(RecruitDelay);
  LoadStream.Read(RecruitCount);
  LoadStream.Read(SerfsPerHouse);
  LoadStream.Read(StartPosition);
  LoadStream.Read(TownDefence);
  LoadStream.Read(WorkerCount);
  LoadStream.Read(AutoAttackRange);
end;


end.
