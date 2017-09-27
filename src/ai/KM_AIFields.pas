unit KM_AIFields;
{$I KaM_Remake.inc}
interface
uses
  KM_NavMesh, KM_AIInfluences,
  KM_CommonClasses, KM_Points;


type
  //Master class for Influence maps, NavMeshe and other terrain representations
  //that are helpful in decision making by Mayour/General
  TKMAIFields = class
  private
    fNavMesh: TKMNavMesh;
    fInfluences: TKMInfluences;
  public
    constructor Create;
    destructor Destroy; override;

    property NavMesh: TKMNavMesh read fNavMesh;
    property Influences: TKMInfluences read fInfluences;

    procedure AfterMissionInit;

    procedure Save(SaveStream: TKMemoryStream);
    procedure Load(LoadStream: TKMemoryStream);
    procedure UpdateState(aTick: Cardinal);
    procedure Paint(aRect: TKMRect);
  end;


var
  gAIFields: TKMAIFields;


implementation
uses
  SysUtils,
  KM_Defaults;


{ TKMAIFields }
constructor TKMAIFields.Create;
begin
  inherited;

  fInfluences := TKMInfluences.Create;
  fNavMesh := TKMNavMesh.Create(fInfluences);
end;


destructor TKMAIFields.Destroy;
begin
  FreeAndNil(fNavMesh);
  FreeAndNil(fInfluences);
  inherited;
end;


procedure TKMAIFields.AfterMissionInit;
begin
  fInfluences.Init;

  if AI_GEN_NAVMESH then
    fNavMesh.Init;
end;


procedure TKMAIFields.Save(SaveStream: TKMemoryStream);
begin
  fNavMesh.Save(SaveStream);
  fInfluences.Save(SaveStream);
end;


procedure TKMAIFields.Load(LoadStream: TKMemoryStream);
begin
  fNavMesh.Load(LoadStream);
  fInfluences.Load(LoadStream);
end;


procedure TKMAIFields.UpdateState(aTick: Cardinal);
begin
  fInfluences.UpdateState(aTick);
  fNavMesh.UpdateState(aTick);
end;


//Render debug symbols
procedure TKMAIFields.Paint(aRect: TKMRect);
begin
  if AI_GEN_INFLUENCE_MAPS then
    fInfluences.Paint(aRect);

  if AI_GEN_NAVMESH then
    fNavMesh.Paint(aRect);
end;


end.
