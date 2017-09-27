unit KM_Alerts;
{$I KaM_Remake.inc}
interface
uses
  Classes,
  KM_Pics, KM_Viewport, KM_ResSound,
  KM_Defaults, KM_Points;

type
  TAlertType = (atBeacon, atFight);

const
  ALERT_DURATION: array [TAlertType] of Byte = (80, 60); //Typical beacon duration after which it will be gone

type
  TKMAlert = class
  private
    fAlertType: TAlertType;
    fExpiration: Cardinal;
    fLoc: TKMPointF;
    fOwner: TKMHandIndex;
  protected
    function GetTexMinimap: TKMPic; virtual; abstract;
    function GetTexTerrain: TKMPic; virtual; abstract;
    function GetTeamColor: Cardinal; virtual; abstract;
    function GetVisibleMinimap: Boolean; virtual; abstract;
    function GetVisibleTerrain: Boolean; virtual; abstract;
  public
    constructor Create(aAlertType: TAlertType; aLoc: TKMPointF; aOwner: TKMHandIndex; aExpiry: Cardinal);

    property AlertType: TAlertType read fAlertType;
    property Loc: TKMPointF read fLoc;
    property Owner: TKMHandIndex read fOwner;
    property Expiration: Cardinal read fExpiration;
    property TexMinimap: TKMPic read GetTexMinimap;
    property TexTerrain: TKMPic read GetTexTerrain;
    property TeamColor: Cardinal read GetTeamColor;
    property VisibleMinimap: Boolean read GetVisibleMinimap;
    property VisibleTerrain: Boolean read GetVisibleTerrain;

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
    fViewport: TKMViewport;
    fList: TList;
    function GetAlert(aIndex: Integer): TKMAlert;
    function GetCount: Integer;
  public
    constructor Create(aViewport: TKMViewport);
    destructor Destroy; override;
    procedure ClearBeaconsExcept(aOwner: TKMHandIndex);
    procedure AddBeacon(aLoc: TKMPointF; aOwner: TKMHandIndex; aColor: Cardinal; aShowUntil: Cardinal);
    procedure AddFight(aLoc: TKMPointF; aPlayer: TKMHandIndex; aAsset: TAttackNotification; aShowUntil: Cardinal);
    function GetLatestAlert: TKMAlert;
    property Count: Integer read GetCount;
    property Items[aIndex: Integer]: TKMAlert read GetAlert; default;
    procedure Paint(aPass: Byte);
    procedure UpdateState(aTickCount: Cardinal);
  end;


implementation
uses
  KM_Hand, KM_RenderPool, KM_HandsCollection, KM_Sound, KM_FogOfWar;


type
  //These classes are only accessed locally, hence they aren't interfaced
  TKMAlertBeacon = class(TKMAlert)
  private
    fFlagColor: Cardinal; //Spectators don't have Hand so fOwner isn't enough
  protected
    function GetTexMinimap: TKMPic; override;
    function GetTexTerrain: TKMPic; override;
    function GetTeamColor: Cardinal; override;
    function GetVisibleMinimap: Boolean; override;
    function GetVisibleTerrain: Boolean; override;
  public
    constructor Create(aLoc: TKMPointF; aOwner: TKMHandIndex; aColor: Cardinal; aExpiry: Cardinal);
  end;

  TKMAlertAttacked = class(TKMAlert)
  private
    fAsset: TAttackNotification; // What was attacked
    fLastLookedAt: Byte;
  protected
    function GetTexMinimap: TKMPic; override;
    function GetTexTerrain: TKMPic; override;
    function GetTeamColor: Cardinal; override;
    function GetVisibleMinimap: Boolean; override;
    function GetVisibleTerrain: Boolean; override;
  public
    constructor Create(aLoc: TKMPointF; aOwner: TKMHandIndex; aAsset: TAttackNotification; aExpiry: Cardinal);
    property Asset: TAttackNotification read fAsset;
    procedure Refresh(aExpiry: Cardinal);
    procedure Update(const aView: TKMRect); override;
  end;


const
  FIGHT_DISTANCE = 24; //Fights this far apart are treated as separate
  INTERVAL_ATTACKED_MSG = 20; //Time between audio messages saying you are being attacked
  MAX_BEACONS = 5; //Maximum number of simultanious beacons per player


{ TKMAlert }
constructor TKMAlert.Create(aAlertType: TAlertType; aLoc: TKMPointF; aOwner: TKMHandIndex; aExpiry: Cardinal);
begin
  inherited Create;

  fAlertType := aAlertType;
  fLoc := aLoc;
  fOwner := aOwner;
  fExpiration := aExpiry;
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
constructor TKMAlertBeacon.Create(aLoc: TKMPointF; aOwner: TKMHandIndex; aColor: Cardinal; aExpiry: Cardinal);
begin
  inherited Create(atBeacon, aLoc, aOwner, aExpiry);

  // Owner and Color became separate because Spectators set beacons under PLAYER_NONE, but still in their color

  fFlagColor := aColor;
end;


function TKMAlertBeacon.GetTexMinimap: TKMPic;
begin
  Result.RX := rxGui;
  Result.ID := 54;
end;


function TKMAlertBeacon.GetTexTerrain: TKMPic;
begin
  Result.RX := rxGui;
  Result.ID := 250; //Located near the house tablets so we can include it in soft shadows range
end;


function TKMAlertBeacon.GetTeamColor: Cardinal;
begin
  Result := fFlagColor;
end;


function TKMAlertBeacon.GetVisibleMinimap: Boolean;
begin
  //Beacons placed by player are always visible until expired
  Result := True;
end;


function TKMAlertBeacon.GetVisibleTerrain: Boolean;
begin
  Result := True;
end;


{ TKMAlertFight }
constructor TKMAlertAttacked.Create(aLoc: TKMPointF; aOwner: TKMHandIndex; aAsset: TAttackNotification; aExpiry: Cardinal);
begin
  inherited Create(atFight, aLoc, aOwner, aExpiry);

  fAsset := aAsset;
  fLastLookedAt := High(Byte) - 1;
end;


function TKMAlertAttacked.GetTexMinimap: TKMPic;
begin
  Result.RX := rxGui;
  Result.ID := 53;
end;


function TKMAlertAttacked.GetTexTerrain: TKMPic;
begin
  //Has no sprite on terrain
  Result.RX := rxTrees;
  Result.ID := 0;
end;


function TKMAlertAttacked.GetTeamColor: Cardinal;
begin
  Result := gHands[fOwner].FlagColor;
end;


function TKMAlertAttacked.GetVisibleMinimap: Boolean;
begin
  //UnderAttack alerts are visible only when not looked at
  //When looked at alert is muted for the next 20sec
  Result := fLastLookedAt >= INTERVAL_ATTACKED_MSG;
end;


function TKMAlertAttacked.GetVisibleTerrain: Boolean;
begin
  Result := False;
end;


procedure TKMAlertAttacked.Refresh(aExpiry: Cardinal);
begin
  fExpiration := aExpiry;
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
    if (fOwner = gMySpectator.HandIndex)
    and (fLastLookedAt = INTERVAL_ATTACKED_MSG) then
      gSoundPlayer.PlayNotification(fAsset);
  end;
end;


{ TKMAlerts }
constructor TKMAlerts.Create(aViewport: TKMViewport);
begin
  inherited Create;

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


//Ally has placed a beacon for ue
procedure TKMAlerts.AddBeacon(aLoc: TKMPointF; aOwner: TKMHandIndex; aColor: Cardinal; aShowUntil: Cardinal);
  procedure RemoveExcessBeacons;
  var
    I, OldestID, qty: Integer;
    OldestExpiry: Cardinal;
  begin
    qty := 0;
    OldestID := -1;
    OldestExpiry := 0;
    for I := 0 to fList.Count - 1 do
      if (Items[I].AlertType = atBeacon)
      and (Items[I].Owner = aOwner) then
      begin
        Inc(qty);
        if (OldestID = -1) or (Items[I].fExpiration < OldestExpiry) then
        begin
          OldestExpiry := Items[I].fExpiration;
          OldestID := I;
        end;
      end;
    if (qty > MAX_BEACONS) and (OldestID <> -1) then
    begin
      Items[OldestID].Free;
      fList.Delete(OldestID);
    end;
  end;

begin
  //If this player has too many beacons remove his oldest one
  RemoveExcessBeacons;
  fList.Add(TKMAlertBeacon.Create(aLoc, aOwner, aColor, aShowUntil));
  gSoundPlayer.Play(sfxn_Beacon);
end;


//Clear all beacons except those, whose owner is aOwner
procedure TKMAlerts.ClearBeaconsExcept(aOwner: TKMHandIndex);
var
  I: Integer;
begin
  for I := fList.Count - 1 downto 0 do
    if (Items[I].AlertType = atBeacon)
      and (Items[I].Owner <> aOwner) then
    begin
      Items[I].Free;
      fList.Delete(I);
    end;
end;


//Player belongings signal that they are under attack
procedure TKMAlerts.AddFight(aLoc: TKMPointF; aPlayer: TKMHandIndex; aAsset: TAttackNotification; aShowUntil: Cardinal);
var
  I: Integer;
begin
  //Check previous alerts and see if there's one like that already
  for I := 0 to fList.Count - 1 do
    if Items[I] is TKMAlertAttacked then
      with TKMAlertAttacked(Items[I]) do
        if (Owner = aPlayer) and (Asset = aAsset)
        and (KMLength(Loc, aLoc) < FIGHT_DISTANCE) then
        begin
          Refresh(aShowUntil);
          Exit;
        end;

  //Otherwise create a new alert
  fList.Add(TKMAlertAttacked.Create(aLoc, aPlayer, aAsset, aShowUntil));
end;


function TKMAlerts.GetLatestAlert: TKMAlert;
var
  I: Integer;
  Best: Cardinal;
begin
  Result := nil;
  Best := 0; //Makes compiler happy
  for I := 0 to fList.Count - 1 do
    if TKMAlert(Items[I]).GetVisibleMinimap then
      if (Result = nil) or (TKMAlert(Items[I]).Expiration >= Best) then
      begin
        Result := TKMAlert(Items[I]);
        Best := TKMAlert(Items[I]).Expiration;
      end;
end;


//We want beacons to be Z sorted on explored area, but always on top in FOW areas
//so that beacon is not occluded by houses in FOW (which we need to render to show
//their rooftops from under the FOW) to block seeing enemy town silhouettes
//Rendered in 2 passes
//1 - beacon amid other sprites
//2 - if in FOW render always
procedure TKMAlerts.Paint(aPass: Byte);
var
  I: Integer;
  R: TKMRect;
begin
  R := KMRectGrow(fViewport.GetMinimapClip, 4); //Beacons may stick up over a few tiles

  for I := 0 to fList.Count - 1 do
  if Items[I].VisibleTerrain
  and KMInRect(Items[I].Loc, R) then
  begin
    case aPass of
      0:  if gMySpectator.FogOfWar.CheckRevelation(Items[I].Loc) > 0 then
            gRenderPool.AddAlert(Items[I].Loc, Items[I].TexTerrain.ID, Items[I].TeamColor);
      1:  if gMySpectator.FogOfWar.CheckRevelation(Items[I].Loc) < FOG_OF_WAR_MAX then
            gRenderPool.RenderSpriteOnTerrain(Items[I].Loc, Items[I].TexTerrain.ID, Items[I].TeamColor);
    end;
  end;
end;


procedure TKMAlerts.UpdateState(aTickCount: Cardinal);
var
  I: Integer;
begin
  //Update alerts visibility
  if (aTickCount mod 10 = 0) then
  for I := fList.Count - 1 downto 0 do
    Items[I].Update(fViewport.GetMinimapClip);

  //Remove expired alerts
  for I := fList.Count - 1 downto 0 do
    if Items[I].IsExpired(aTickCount) then
    begin
      Items[I].Free;
      fList.Delete(I);
    end;
end;


end.
