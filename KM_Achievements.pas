unit KM_Achievements;
{$I KaM_Remake.inc}
interface
uses
  Classes, KromUtils,
  KM_CommonClasses, KM_Defaults, KM_ResourceHouse, KM_ResourceUnit;


type
  //Achievements represent the behavior we want to encourage, they are to
  //help player to explore the game and try out all its possibilities. E.g.:
  // - encourage beating the game (beat campaigns, play MP, use all troops, focus on different town specializations)
  // - player controlled (good: train 200 knights in one game. bad: have single archer kill 10 militia with 10 shots)
  // - could be proud of (good: win without losses. bad: get defeated 10 times)
  // - special (meet some rare event and feeling good about it, e.g. ???)

  //Can be split into basic mechanic types:
  // A. during single mission
  // B. accumulating all the time
  // 1. statistical (collect some stats, e.g. build 5 Inns)
  // 2. conditional (have something combined with something else)
  TKMAchievement = class
    IsAwarded: Boolean;

    //Progress towards receiving the achievement
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
    procedure Subscribe(); //Attach self to relevant events
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
  public
    constructor Create(aOwner: TPlayerIndex);
    destructor Destroy; override;

    //Just like Stats or GIP or Scripting we receive a handful of events from the game
    procedure HouseBuilt(aHouseType: THouseType);

  end;


implementation
uses Math, KM_PlayersCollection, KM_Resource;


{ TKMAchievement }
function TKMAchievement.Progress: Single;
begin
  //As simple as that incase child class does not implements Progress
  Result := Byte(IsAwarded);
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
function TKMAchievementVariety.Progress: Single;
begin
  //Result :=
end;


procedure TKMAchievementVariety.Subscribe;
begin
  //fParent.Subscribe()
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



end.
