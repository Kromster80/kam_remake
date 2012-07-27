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
    function GetTexID: Word; virtual; abstract;
    function GetTexOffset: TKMPointI; virtual; abstract;
    function GetVisibleMinimap: Boolean; virtual; abstract;
  public
    constructor Create(aAlertType: TAlertType; aLoc: TKMPointF; aOwner: TPlayerIndex; aTick: Cardinal);

    property AlertType: TAlertType read fAlertType;
    property Loc: TKMPointF read fLoc;
    property Owner: TPlayerIndex read fOwner;
    property TexID: Word read GetTexID;
    property TexOffset: TKMPointI read GetTexOffset;
    property VisibleMinimap: Boolean read GetVisibleMinimap;

    function IsExpired(aTick: Cardinal): Boolean;
    procedure Update(const aView: TKMRect); virtual;
  end;

  //Alerts are signals in game that draw Players attention to something,
  //Unlike simple messages that are fired-and-forget, alerts do have a lifespan
  //and some interaction with e.g. Viewport
  //Alerts are not saved between game sessions because by nature they are short
  //lived and last only a few seconds
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


type
  TKMAlertBeacon = class(TKMAlert)
  protected
    function GetTexID: Word; override;
    function GetTexOffset: TKMPointI; override;
    function GetVisibleMinimap: Boolean; override;
  public
    constructor Create(aLoc: TKMPointF; aOwner: TPlayerIndex; aTick: Cardinal);
  end;

  TKMAlertAttacked = class(TKMAlert)
  private
    fAsset: TAttackNotification; //What was attacked?
    fLastLookedAt: Byte;
  protected
    function GetTexID: Word; override;
    function GetTexOffset: TKMPointI; override;
    function GetVisibleMinimap: Boolean; override;
  public
    constructor Create(aLoc: TKMPointF; aOwner: TPlayerIndex; aAsset: TAttackNotification; aTick: Cardinal);
    property Asset: TAttackNotification read fAsset;
    procedure Refresh(aTick: Cardinal);
    procedure Update(const aView: TKMRect); override;
  end;


const
  ALERT_DURATION: array [TAlertType] of Byte = (35, 60); //Typical beacon duration after which it will be gone
  FIGHT_DISTANCE = 24; //Fights this far apart are treated as separate
  INTERVAL_ATTACKED_MSG = 20; //Time between audio messages saying you are being attacked


{ TKMAlert }
constructor TKMAlert.Create(aAlertType: TAlertType; aLoc: TKMPointF; aOwner: TPlayerIndex; aTick: Cardinal);
begin
  inherited Create;

  fAlertType := aAlertType;
  fLoc := aLoc;
  fOwner := aOwner;
  fExpiration := aTick + ALERT_DURATION[fAlertType];
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


function TKMAlertBeacon.GetTexOffset: TKMPointI;
begin
  Result := KMPointI(0,0); //Temporary until we get a proper one
end;


function TKMAlertBeacon.GetTexID: Word;
begin
  Result := 32; //Temporary until we get a proper one
end;


function TKMAlertBeacon.GetVisibleMinimap: Boolean;
begin
  //Beacons placed by player are always visible until expired
  Result := True;
end;


{ TKMAlertFight }
constructor TKMAlertAttacked.Create(aLoc: TKMPointF; aOwner: TPlayerIndex; aAsset: TAttackNotification; aTick: Cardinal);
begin
  inherited Create(atFight, aLoc, aOwner, aTick);

  fAsset := aAsset;
  fLastLookedAt := High(Byte) - 1;
end;


function TKMAlertAttacked.GetTexID: Word;
begin
  Result := 69;
end;


function TKMAlertAttacked.GetTexOffset: TKMPointI;
begin
  Result := KMPointI(-11,-7);
end;


function TKMAlertAttacked.GetVisibleMinimap: Boolean;
begin
  //UnderAttack alerts are visible only when not looked at
  //When looked at alert is muted for the next 20sec
  Result := fLastLookedAt >= INTERVAL_ATTACKED_MSG;
end;


procedure TKMAlertAttacked.Refresh(aTick: Cardinal);
begin
  fExpiration := aTick + ALERT_DURATION[fAlertType];
end;


procedure TKMAlertAttacked.Update(const aView: TKMRect);
begin
  inherited;

  //If alerts gets into view - mute it.
  //Alert will be unmuted when going out of view
  if KMInRect(fLoc, aView) then
    fLastLookedAt := 0
  else
  begin
    Inc(fLastLookedAt);

    //Alert is automatically repeated
    if fLastLookedAt >= INTERVAL_ATTACKED_MSG * 2 then
      fLastLookedAt := INTERVAL_ATTACKED_MSG;

    //Make the sound
    if (fOwner = MyPlayer.PlayerIndex)
    and (fLastLookedAt = INTERVAL_ATTACKED_MSG) then
      fSoundLib.PlayNotification(fAsset);
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
    if Items[I] is TKMAlertAttacked then
      with TKMAlertAttacked(Items[I]) do
        if (Owner = aPlayer) and (Asset = aAsset)
        and (GetLength(Loc, aLoc) < FIGHT_DISTANCE) then
        begin
          Refresh(fTickCounter^);
          Exit;
        end;

  //Otherwise create a new alert
  fList.Add(TKMAlertAttacked.Create(aLoc, aPlayer, aAsset, fTickCounter^));
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
