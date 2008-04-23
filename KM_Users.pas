unit KM_Users;

interface
uses
  classes, KM_Classes, KM_Units, KM_Houses, windows;
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
    function GetCtrl(Index: Integer): TKMUserControl;
    function UserByName(const aName: string): TKMUser;
  public
    constructor Create();
    destructor Destroy; override;
    function Add(const aUserName: string; aControlType: TUserControlType): TKMUserControl;
    property Ctrl[Index: Integer]: TKMUserControl read GetCtrl;
  public
    function AddUnit(const aUserName: string; aUnitType: TUnitType; Position: TPoint): Boolean;
    procedure AddHouse(aHouseType: THouseType; Position: TPoint);
    procedure RemUnit(Position: TPoint);
    procedure RemHouse(Position: TPoint);
    function UnitsHitTest(X, Y: Integer): TKMUnit;
    function HousesHitTest(X, Y: Integer): TKMHouse;
    function UnitsSelectedUnit: TKMUnit;
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

function TKMUserControlList.AddUnit(const aUserName: string; aUnitType: TUnitType; Position: TPoint): Boolean;
begin
//  if UserByName(aUserName).GetMoney(fUnits.GetPrice(aUnitType)) then
    fUnits.Add(aUserName, aUnitType, Position.X, Position.Y)
end;

procedure TKMUserControlList.AddHouse(aHouseType: THouseType; Position: TPoint);
begin
  fHouses.Add(aHouseType, Position.X, Position.Y)
end;

procedure TKMUserControlList.RemUnit(Position: TPoint);
begin
  fUnits.Rem(Position.X, Position.Y)
end;

procedure TKMUserControlList.RemHouse(Position: TPoint);
begin
  fHouses.Rem(Position.X, Position.Y)
end;

constructor TKMUserControlList.Create();
begin
  fUnits:= TKMUnitsCollection.Create;
  fHouses:= TKMHousesCollection.Create;
end;

destructor TKMUserControlList.Destroy;
begin
  fUnits.Free;
  fHouses.Free;
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
