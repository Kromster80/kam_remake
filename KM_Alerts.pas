unit KM_Alerts;
{$I KaM_Remake.inc}
interface
uses Classes, Math, SysUtils,
  KM_Defaults, KM_Points;


type
  TAlertType = (atBeacon, atFight);

  TKMAlert = class
  public
    AlertType: TAlertType;
    Loc: TKMPointF;
    Owner: TPlayerIndex;
    Expiration: Cardinal;
    constructor Create(aAlertType: TAlertType; aLoc: TKMPointF; aOwner: TPlayerIndex; aTick: Cardinal);
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
    procedure AddAlert(aAlertType: TAlertType; aLoc: TKMPointF; aOwner: TPlayerIndex);
    property Count: Integer read GetCount;
    property Items[aIndex: Integer]: TKMAlert read GetAlert; default;
    procedure UpdateState;
  end;


implementation


const
  BEACON_DURATION = 35; //Typical beacon duration after which it will be gone


{ TKMAlert }
constructor TKMAlert.Create(aAlertType: TAlertType; aLoc: TKMPointF; aOwner: TPlayerIndex; aTick: Cardinal);
begin
  inherited Create;

  AlertType := aAlertType;
  Loc := aLoc;
  Owner := aOwner;
  Expiration := aTick + BEACON_DURATION;
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


procedure TKMAlerts.AddAlert(aAlertType: TAlertType; aLoc: TKMPointF; aOwner: TPlayerIndex);
begin
  fList.Add(TKMAlert.Create(aAlertType, aLoc, aOwner, fTickCounter^));
end;


procedure TKMAlerts.UpdateState;
var
  I: Integer;
begin
  //Remove expired alerts
  for I := fList.Count - 1 downto 0 do
  if Items[I].IsExpired(fTickCounter^) then
  begin
    Items[I].Free;
    fList.Delete(I);
  end;
end;


end.
