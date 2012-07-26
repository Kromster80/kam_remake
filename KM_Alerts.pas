unit KM_Alerts;
{$I KaM_Remake.inc}
interface
uses Classes, Math, SysUtils,
  KM_Defaults, KM_Points, KM_Sound, KM_Viewport;


type
  TAlertType = (atBeacon, atFight);

  TKMAlert = class
  private
    fAlertType: TAlertType;
    fExpiration: Cardinal;
    fLoc: TKMPointF;
    fOwner: TPlayerIndex;
  protected
    function GetText: string; virtual; abstract;
    function GetVisible: Boolean; virtual; abstract;
  public
    constructor Create(aAlertType: TAlertType; aLoc: TKMPointF; aOwner: TPlayerIndex; aTick: Cardinal);

    property AlertType: TAlertType read fAlertType;
    property Loc: TKMPointF read fLoc;
    property Owner: TPlayerIndex read fOwner;
    property Text: string read GetText;
    property Visible: Boolean read GetVisible;

    function IsExpired(aTick: Cardinal): Boolean;
    procedure Update(const aView: TKMRect); virtual;
  end;

  TKMAlertBeacon = class(TKMAlert)
  protected
    function GetText: string; override;
    function GetVisible: Boolean; override;
  public
    constructor Create(aLoc: TKMPointF; aOwner: TPlayerIndex; aTick: Cardinal);
  end;

  TKMAlertFight = class(TKMAlert)
  private
    fAsset: TAttackNotification; //What was attacked?
    fLastLookedAt: Byte;
    fLastReadAbout: Byte;
  protected
    function GetText: string; override;
    function GetVisible: Boolean; override;
  public
    constructor Create(aLoc: TKMPointF; aOwner: TPlayerIndex; aAsset: TAttackNotification; aTick: Cardinal);
    property Asset: TAttackNotification read fAsset;
    procedure Refresh(aTick: Cardinal);
    procedure Update(const aView: TKMRect); override;
  end;

  //Alerts collection
  //Alerts are quick signals in game typically showing for 3-5sec
  //Because of that they don't need to be saved between game sessions
  TKMAlerts = class
  private
    fTickCounter: PCardinal;
    fViewport: TViewport;
    fList: TList;
    function GetAlert(aIndex: Integer): TKMAlert;
    function GetCount: Integer;
  public
    constructor Create(aTickCounter: PCardinal; aViewport: TViewport);
    destructor Destroy; override;
    procedure AddBeacon(aLoc: TKMPointF; aOwner: TPlayerIndex);
    procedure AddFight(aLoc: TKMPointF; aPlayer: TPlayerIndex; aAsset: TAttackNotification);
    property Count: Integer read GetCount;
    property Items[aIndex: Integer]: TKMAlert read GetAlert; default;
    procedure Paint;
    procedure UpdateState;
  end;


implementation
uses KM_PlayersCollection, KM_RenderPool;

const
  BEACON_DURATION: array [TAlertType] of Byte = (35, 30); //Typical beacon duration after which it will be gone
  FIGHT_DISTANCE = 24; //Fights this far apart are treated as separate
  FIGHT_REFRESHRATE = 30;


{ TKMAlert }
constructor TKMAlert.Create(aAlertType: TAlertType; aLoc: TKMPointF; aOwner: TPlayerIndex; aTick: Cardinal);
begin
  inherited Create;

  fAlertType := aAlertType;
  fLoc := aLoc;
  fOwner := aOwner;
  fExpiration := aTick + BEACON_DURATION[fAlertType];
end;


function TKMAlert.IsExpired(aTick: Cardinal): Boolean;
begin
  Result := aTick >= fExpiration;
end;


procedure TKMAlert.Update(const aView: TKMRect);
begin
  //
end;


{ TKMAlertBeacon }
constructor TKMAlertBeacon.Create(aLoc: TKMPointF; aOwner: TPlayerIndex; aTick: Cardinal);
begin
  inherited Create(atBeacon, aLoc, aOwner, aTick);
end;


function TKMAlertBeacon.GetText: string;
begin
  Result := '!';
end;


function TKMAlertBeacon.GetVisible: Boolean;
begin
  //Beacons placed by player are always visible until expired
  Result := True;
end;


{ TKMAlertFight }
constructor TKMAlertFight.Create(aLoc: TKMPointF; aOwner: TPlayerIndex; aAsset: TAttackNotification; aTick: Cardinal);
begin
  inherited Create(atFight, aLoc, aOwner, aTick);

  fAsset := aAsset;
  fLastLookedAt := High(Byte);
  fLastReadAbout := High(Byte);
end;


function TKMAlertFight.GetText: string;
begin
  Result := 'x';
end;


function TKMAlertFight.GetVisible: Boolean;
begin
  //UnderAttack alerts are visible only when not looked at
  //When looked at alert is muted for the next 20sec
  Result := fLastLookedAt > FIGHT_REFRESHRATE;
end;


procedure TKMAlertFight.Refresh(aTick: Cardinal);
begin
  fExpiration := aTick + BEACON_DURATION[fAlertType];
end;


procedure TKMAlertFight.Update(const aView: TKMRect);
begin
  inherited;

  //If alerts gets into view - mute it. Alert will be unmuted when going out of view
  if KMInRect(fLoc, aView) then
  begin
    fLastLookedAt := 0;
    fLastReadAbout := 0;
  end
  else
  begin
    fLastLookedAt := Min(fLastLookedAt + 1, High(Byte));
    fLastReadAbout := Min(fLastReadAbout + 1, High(Byte));
  end;
end;


{ TKMAlerts }
constructor TKMAlerts.Create(aTickCounter: PCardinal; aViewport: TViewport);
begin
  inherited Create;

  fTickCounter := aTickCounter;
  fViewport := aViewport;
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


procedure TKMAlerts.AddBeacon(aLoc: TKMPointF; aOwner: TPlayerIndex);
begin
  fList.Add(TKMAlertBeacon.Create(aLoc, aOwner, fTickCounter^));
end;


//Player signals that he is under attack
procedure TKMAlerts.AddFight(aLoc: TKMPointF; aPlayer: TPlayerIndex; aAsset: TAttackNotification);
var
  I: Integer;
begin
  //Check previous alerts and see if there's one like that already
  for I := 0 to fList.Count - 1 do
    if Items[I] is TKMAlertFight then
      with TKMAlertFight(Items[I]) do
        if (Owner = aPlayer) and (Asset = aAsset)
        and (GetLength(Loc, aLoc) < FIGHT_DISTANCE) then
        begin
          Refresh(fTickCounter^);
          Exit;
        end;

  //Otherwise create a new alert
  fList.Add(TKMAlertFight.Create(aLoc, aPlayer, aAsset, fTickCounter^));
end;


procedure TKMAlerts.Paint;
var
  I: Integer;
  R: TKMRect;
begin
  R := fViewport.GetMinimapClip;

  //Only player placed beacons are shown on map
  //Fight alerts are shown only on minimap
  for I := 0 to fList.Count - 1 do
    if KMInRect(Items[I].Loc, R)
    and (Items[I].AlertType = atBeacon)
    and (fPlayers.CheckAlliance(Items[I].Owner, MyPlayer.PlayerIndex) = at_Ally) then
      fRenderPool.AddAlert(Items[I].Loc, 340);
end;


procedure TKMAlerts.UpdateState;
var
  I: Integer;
begin
  //Update alerts visibility
  if (fTickCounter^ mod 10 = 0) then
  for I := fList.Count - 1 downto 0 do
    Items[I].Update(fViewport.GetMinimapClip);

  //Remove expired alerts
  for I := fList.Count - 1 downto 0 do
    if Items[I].IsExpired(fTickCounter^) then
    begin
      Items[I].Free;
      fList.Delete(I);
    end;
end;


end.
