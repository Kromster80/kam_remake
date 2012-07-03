unit KM_Alerts;
{$I KaM_Remake.inc}
interface
uses Classes, Math, SysUtils,
  KM_CommonClasses, KM_Defaults, KM_Points;


type
  TAlertType = (atBeacon, atFight);

  TKMAlert = class
  public
    AlertType: TAlertType;
    Loc: TKMPoint;
    Owner: TPlayerIndex;
    Expiration: Cardinal;
    constructor Create(aLoc: TKMPoint; aAlertType: TAlertType; aOwner: TPlayerIndex; aTick: Cardinal);
    function IsExpired(aTick: Cardinal): Boolean;
  end;

  //Alerts collection
  //Alerts are quick signals in game typically showing for 3-5sec
  //Because of that they don't need to be saved between game sessions
  TKMAlerts = class
  private
    fTickCounter: PCardinal;
    fList: TList;
    function GetAlert(aIndex: Integer): TKMAlert;
    function GetCount: Integer;
  public
    constructor Create(aTickCounter: PCardinal);
    destructor Destroy; override;
    procedure AddAlert(aLoc: TKMPoint; aAlertType: TAlertType; aOwner: TPlayerIndex);
    property Count: Integer read GetCount;
    property Items[aIndex: Integer]: TKMAlert read GetAlert; default;
    procedure UpdateState(aTick: Cardinal);
  end;


implementation
uses KM_PlayersCollection;


{ TKMAlert }
constructor TKMAlert.Create(aLoc: TKMPoint; aAlertType: TAlertType; aOwner: TPlayerIndex; aTick: Cardinal);
begin
  inherited Create;

  AlertType := aAlertType;
  Loc := aLoc;
  Owner := aOwner;
  Expiration := aTick + 30;
end;


function TKMAlert.IsExpired(aTick: Cardinal): Boolean;
begin
  Result := aTick >= Expiration;
end;


{ TKMAlerts }
constructor TKMAlerts.Create(aTickCounter: PCardinal);
begin
  inherited Create;

  fTickCounter := aTickCounter;
  fList := TList.Create;
end;


destructor TKMAlerts.Destroy;
var
  I: Integer;
begin
  for I := 0 to fList.Count - 1 do
    Items[I].Free;

  fList.Free;
  inherited;
end;


function TKMAlerts.GetAlert(aIndex: Integer): TKMAlert;
begin
  Result := fList[aIndex];
end;


function TKMAlerts.GetCount: Integer;
begin
  Result := fList.Count;
end;


procedure TKMAlerts.AddAlert(aLoc: TKMPoint; aAlertType: TAlertType; aOwner: TPlayerIndex);
begin
  fList.Add(TKMAlert.Create(aLoc, aAlertType, aOwner, fTickCounter^));
end;


procedure TKMAlerts.UpdateState(aTick: Cardinal);
var
  I: Integer;
begin
  //Remove expired alerts
  for I := fList.Count - 1 downto 0 do
  if Items[I].IsExpired(aTick) then
  begin
    Items[I].Free;
    fList.Delete(I);
  end;
end;


end.
