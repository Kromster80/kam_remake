unit KM_Users;

interface
uses
  classes, KromUtils, KM_Global_Data, KM_Classes, KM_Units, KM_Houses, KM_DeliverQueue, KM_Defaults, windows;
type
  TUserControlType = (uct_User, uct_Computer);

  TKMUser = class(TObject)
  private
    fUserName: string;
  public
    constructor Create(const aUserName: string);
//    function GetMoney(aPrice: TMoney): Boolean;
  end;

  TKMUserControl = class(TObject)
  private
    fUser: TKMUser;
  public
    constructor Create(const aUserName: string);
    destructor Destroy; override;
  end;

  TKMUserUserControl = class(TKMUserControl)

  end;

  TKMUserComputerControl = class(TKMUserControl)

  end;

  TKMUserControlList = class(TKMList)
  private
    fUnits: TKMUnitsCollection;
    fHouses: TKMHousesCollection;
    fDeliverList: TKMDeliverQueue;
    fBuildList: TKMBuildingQueue;
    function GetCtrl(Index: Integer): TKMUserControl;
    function UserByName(const aName: string): TKMUser;
  public
    constructor Create();
    destructor Destroy; override;
    function Add(const aUserName: string; aControlType: TUserControlType): TKMUserControl;
    property Ctrl[Index: Integer]: TKMUserControl read GetCtrl;
  public
    function AddUnit(const aUserName: string; aUnitType: TUnitType; Position: TKMPoint): Boolean;
    procedure AddHouse(aHouseType: THouseType; Position: TKMPoint);
    procedure AddRoadPlan(aLoc: TKMPoint; aRoadType:TRoadType);
    procedure AddHousePlan(aLoc: TKMPoint; aHouseType: THouseType);
    procedure RemUnit(Position: TKMPoint);
    procedure RemHouse(Position: TKMPoint);
    function FindEmptyHouse(aUnitType:TUnitType): TKMHouse;
    function UnitsHitTest(X, Y: Integer): TKMUnit;
    function HousesHitTest(X, Y: Integer): TKMHouse;
    function UnitsSelectedUnit: TKMUnit;
    property DeliverList:TKMDeliverQueue read fDeliverList;
    property BuildList:TKMBuildingQueue read fBuildList;
  public
    procedure UpdateState;
    procedure Paint;
  end;

implementation

uses
  SysUtils;

{ TKMUserList }

function TKMUserControlList.Add(const aUserName: string; aControlType: TUserControlType): TKMUserControl;
begin
  case aControlType of
    uct_User:
      Result:= TKMUserUserControl.Create(aUserName);
    uct_Computer:
      Result:= TKMUserComputerControl.Create(aUserName);
  else
    Result:= nil;
  end;
  if Result <> nil then
    Inherited Add(Result);
end;

function TKMUserControlList.AddUnit(const aUserName: string; aUnitType: TUnitType; Position: TKMPoint): Boolean;
begin
    fUnits.Add(aUserName, aUnitType, Position.X, Position.Y);
    Result:=true;
end;

procedure TKMUserControlList.AddHouse(aHouseType: THouseType; Position: TKMPoint);
begin
  fHouses.Add(aHouseType, Position.X, Position.Y)
end;

procedure TKMUserControlList.AddRoadPlan(aLoc: TKMPoint; aRoadType:TRoadType);
begin
  fTerrain.SetRoadPlan(aLoc, aRoadType);
  BuildList.AddNewRoadToBuild(aLoc);
end;

procedure TKMUserControlList.AddHousePlan(aLoc: TKMPoint; aHouseType: THouseType);
begin
  fTerrain.SetHousePlan(aLoc, aHouseType);
  BuildList.AddNewHouseToBuild(aLoc, aHouseType);
end;

procedure TKMUserControlList.RemUnit(Position: TKMPoint);
begin
  fUnits.Rem(Position.X, Position.Y)
end;

procedure TKMUserControlList.RemHouse(Position: TKMPoint);
begin
  fHouses.Rem(Position.X, Position.Y)
end;

function TKMUserControlList.FindEmptyHouse(aUnitType:TUnitType): TKMHouse;
begin
Result:=fHouses.FindEmptyHouse(aUnitType);
end;

constructor TKMUserControlList.Create();
begin
  fUnits:= TKMUnitsCollection.Create;
  fHouses:= TKMHousesCollection.Create;
  fDeliverList:= TKMDeliverQueue.Create;
  fBuildList:= TKMbuildingQueue.Create;
end;

destructor TKMUserControlList.Destroy;
begin
  fUnits.Free;
  fHouses.Free;
  fDeliverList.Free;
  fBuildList.Free;
  inherited;
end;

function TKMUserControlList.GetCtrl(Index: Integer): TKMUserControl;
begin
  Result:= TKMUserControl(Items[Index]);
end;

procedure TKMUserControlList.Paint;
begin
  fUnits.Paint;
  fHouses.Paint;
end;

function TKMUserControlList.UnitsHitTest(X, Y: Integer): TKMUnit;
begin
  Result:= fUnits.HitTest(X, Y);
end;

function TKMUserControlList.HousesHitTest(X, Y: Integer): TKMHouse;
begin
  Result:= fHouses.HitTest(X, Y);
end;

function TKMUserControlList.UnitsSelectedUnit: TKMUnit;
begin
  Result:= fUnits.SelectedUnit;
end;

procedure TKMUserControlList.UpdateState;
begin
  fUnits.UpdateState;
  fHouses.UpdateState;
end;

function TKMUserControlList.UserByName(const aName: string): TKMUser;
var
  I: Integer;
begin
  Result:= nil;
  for I := 0 to Count - 1 do
    if SameText(Ctrl[I].fUser.fUserName, aName) then
    begin
      Result:= Ctrl[I].fUser;
      Exit;
    end;    
end;

{ TKMUser }

constructor TKMUser.Create(const aUserName: string);
begin
  Inherited Create;
  fUserName:= aUserName;
//  fMoney.Gold:= 1000;
end;

{function TKMUser.GetMoney(aPrice: TMoney): Boolean;
begin
  Result:= fMoney.Gold >= aPrice.Gold;
  if Result then
    fMoney.Gold:= fMoney.Gold - aPrice.Gold;
end;}

{ TKMUserControl }

constructor TKMUserControl.Create(const aUserName: string);
begin
  Inherited Create;
  fUser:= TKMUser.Create(aUserName);
end;

destructor TKMUserControl.Destroy;
begin
  fUser.Free;
  inherited;
end;

end.
