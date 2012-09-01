unit KM_AIGeneral;
{$I KaM_Remake.inc}
interface
uses
  Classes, KromUtils, Math, SysUtils,
  KM_Defaults, KM_CommonClasses, KM_Points, KM_AISetup;


type
  TKMGeneral = class
  private
    fOwner: TPlayerIndex;
    //fSetup: TKMPlayerAISetup;

    procedure CheckDefences;
  public
    constructor Create(aPlayer: TPlayerIndex; aSetup: TKMPlayerAISetup);
    destructor Destroy; override;

    procedure AfterMissionInit;
    procedure OwnerUpdate(aPlayer: TPlayerIndex);

    procedure UpdateState(aTick: Cardinal);
    procedure Save(SaveStream: TKMemoryStream);
    procedure Load(LoadStream: TKMemoryStream);
  end;


implementation
uses KM_Game, KM_Houses, KM_PlayersCollection, KM_Player, KM_Terrain,
  KM_Utils, KM_AIFields, KM_AIDefensePos;


{ TKMGeneral }
constructor TKMGeneral.Create(aPlayer: TPlayerIndex; aSetup: TKMPlayerAISetup);
begin
  inherited Create;

  fOwner := aPlayer;
end;


destructor TKMGeneral.Destroy;
begin

  inherited;
end;


procedure TKMGeneral.AfterMissionInit;
begin

end;


procedure TKMGeneral.Load(LoadStream: TKMemoryStream);
begin

end;


procedure TKMGeneral.OwnerUpdate(aPlayer: TPlayerIndex);
begin
  fOwner := aPlayer;
end;


procedure TKMGeneral.Save(SaveStream: TKMemoryStream);
begin

end;


procedure TKMGeneral.CheckDefences;
var
  Outline: TKMWeightSegments;
  I: Integer;
  DP: TAIDefencePositions;
  Loc: TKMPoint;
  LocI: TKMPointI;
  FaceDir: TKMDirection;
begin
  //Get defence outline with weights representing how important each segment is
  fAIFields.NavMesh.GetDefenceOutline(fOwner, Outline);

  DP := fPlayers[fOwner].AI.DefencePositions;
  DP.Clear;

  //Create missing defence positions
  for I := DP.Count to High(Outline) do
  begin
    FaceDir := KMGetDirection(KMPointF(Outline[I].A), KMPerpendecular(Outline[I].A, Outline[I].B));

    Loc := KMPointRound(KMLerp(Outline[I].A, Outline[I].B, 0.5));
    LocI := KMGetPointInDir(Loc, KMAddDirection(FaceDir, 4), 1);
    Loc := fTerrain.EnsureTileInMapCoords(LocI.X, LocI.Y, 3);
    DP.AddDefencePosition(KMPointDir(Loc, FaceDir), gt_Melee, 12, adt_FrontLine);

    Loc := KMPointRound(KMLerp(Outline[I].A, Outline[I].B, 0.5));
    LocI := KMGetPointInDir(Loc, KMAddDirection(FaceDir, 4), 4);
    Loc := fTerrain.EnsureTileInMapCoords(LocI.X, LocI.Y, 3);
    DP.AddDefencePosition(KMPointDir(Loc, FaceDir), gt_Ranged, 12, adt_FrontLine);
  end;

  //Compare existing defence positions with the sample
    //Get the ratio between sample and existing troops
    //Check all segments to have proportional troops count
    //Add or remove defence positions
end;


procedure TKMGeneral.UpdateState(aTick: Cardinal);
begin
  if (aTick + Byte(fOwner)) mod (MAX_PLAYERS * 120) <> 0 then Exit;

  //Manage defence positions
  CheckDefences;
end;


end.

