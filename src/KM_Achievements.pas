unit KM_Achievements;
{$I KaM_Remake.inc}
interface
uses
  Classes, KromUtils,
  KM_CommonClasses, KM_Defaults, KM_ResourceHouse, KM_ResourceUnit;


type
  //Achievements represent the behavior we want to encourage, they are to
  //help player to explore the game and try out all its possibilities.
  //These are the points we need to ask ourselves before adding an achievement:
  // - Does it encourages advancing into the game (beat campaigns, play MP, use all troops, focus on different town specializations)
  // - Is it player controlled (good: train 100 knights in one game. bad: have single archer kill 10 militia with 10 shots)
  // - Could it be proud of (good: win without losses. bad: get defeated 10 times)

  //Can be split into basic mechanic types:
  // A. during single mission
  // B. accumulating all the time
  // 1. statistical (collect some stats, e.g. build 5 Inns)
  // 2. conditional (have something combined with something else)
  TKMAchievements = class;

  TKMAchievementEvent = (aeHouseBuilt, aeMissionEnd, aeMissionStart, aeUnitTrained);

  TKMAchievement = class
  private
    fIsAwarded: Boolean;
  public
    procedure Process(aEvent: TKMAchievementEvent; aParams: array of const); virtual; abstract;

    //Progress towards receiving the achievement
    property IsAwarded: Boolean read fIsAwarded;
    function Progress: Single; virtual;

    //Save load
    procedure LoadFromStream(aStream: TKMemoryStream); virtual;
    procedure SaveToStream(aStream: TKMemoryStream); virtual;
  end;

  //Build one of each houses and train one of each units
  TKMAchievementVariety = class(TKMAchievement)
  private
    fHousesBuilt: array [HOUSE_MIN..HOUSE_MAX] of Boolean;
    fCitizenTrained: array [CITIZEN_MIN..CITIZEN_MAX] of Boolean;
    fWarriorsTrained: array [WARRIOR_EQUIPABLE_MIN..WARRIOR_EQUIPABLE_MAX] of Boolean;
  public
    procedure Process(aEvent: TKMAchievementEvent; aParams: array of const); override;
    function Progress: Single; override;
    procedure LoadFromStream(aStream: TKMemoryStream); override;
    procedure SaveToStream(aStream: TKMemoryStream); override;
  end;

  //Easy:
  // - Win with just starting troops
  // - Build one of each houses and train one of each units

  //Medium:
  // - Win MP game with only one type of warriors
  // - Beat the campaign
  // - Have certain amount of warfare made and equip soldiers with it till some time

  //Hard:
  // -

  TKMAchievements = class
  private
    fList: TList;
    fOwner: TPlayerIndex;
    procedure ProcessEvent(aEvent: TKMAchievementEvent; aParams: array of const);
  public
    constructor Create(aOwner: TPlayerIndex);
    destructor Destroy; override;

    //Just like Stats or GIP or Scripting we receive a handful of events from the game
    procedure HouseBuilt(aHouseType: THouseType);
    procedure MissionEnd(aGameResult: TGameResultMsg; aMissionName: string);
  end;


implementation
uses
  Math, KM_PlayersCollection, KM_Resource;


{ TKMAchievement }
function TKMAchievement.Progress: Single;
begin
  //As simple as that incase child class does not implements Progress
  Result := Byte(fIsAwarded);
end;


procedure TKMAchievement.LoadFromStream(aStream: TKMemoryStream);
begin
  //
end;


procedure TKMAchievement.SaveToStream(aStream: TKMemoryStream);
begin
  //
end;


{ TKMAchievementVariety }
procedure TKMAchievementVariety.Process(aEvent: TKMAchievementEvent; aParams: array of const);
var
  ut: TUnitType;
begin
  if aEvent = aeMissionStart then
  begin
    //Reset achievement on each new mission
    FillChar(fHousesBuilt, SizeOf(fHousesBuilt), #0);
    FillChar(fCitizenTrained, SizeOf(fHousesBuilt), #0);
    FillChar(fWarriorsTrained, SizeOf(fHousesBuilt), #0);
  end;
  if aEvent = aeHouseBuilt then
  begin
    Assert((Length(aParams) = 1) and (aParams[0].VType = vtInteger));
    fHousesBuilt[THouseType(aParams[0].VInteger)] := True;
  end;
  if aEvent = aeUnitTrained then
  begin
    Assert((Length(aParams) = 1) and (aParams[0].VType = vtInteger));
    ut := TUnitType(aParams[0].VInteger);
    if ut in [CITIZEN_MIN..CITIZEN_MAX] then
      fCitizenTrained[ut] := True
    else
    if ut in [WARRIOR_EQUIPABLE_MIN..WARRIOR_EQUIPABLE_MAX] then
      fWarriorsTrained[ut] := True;
  end;
end;


function TKMAchievementVariety.Progress: Single;
begin
  //Result :=
end;


procedure TKMAchievementVariety.LoadFromStream(aStream: TKMemoryStream);
begin
  inherited;

end;


procedure TKMAchievementVariety.SaveToStream(aStream: TKMemoryStream);
begin
  inherited;

end;


{ TKMAchievements }
constructor TKMAchievements.Create(aOwner: TPlayerIndex);
begin
  inherited Create;

  fOwner := aOwner;

  fList := TList.Create;
end;


destructor TKMAchievements.Destroy;
begin
  fList.Free;

  inherited;
end;


procedure TKMAchievements.HouseBuilt(aHouseType: THouseType);
begin
  //Inform all interested achievements
end;



procedure TKMAchievements.MissionEnd(aGameResult: TGameResultMsg; aMissionName: string);
begin
  ProcessEvent(aeMissionEnd, [Byte(aGameResult), aMissionName]);
end;


procedure TKMAchievements.ProcessEvent(aEvent: TKMAchievementEvent; aParams: array of const);
var
  I: Integer;
begin
  for I := 0 to fList.Count - 1 do
    TKMAchievement(fList[I]).Process(aEvent, aParams);
end;


end.
