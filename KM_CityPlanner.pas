unit KM_CityPlanner;
{$I KaM_Remake.inc}
interface
uses
  Classes, KromUtils, Math, SysUtils,
  KM_Defaults, KM_Points;


type
  TKMCityPlanner = class
  private
    fOwner: TPlayerIndex;
    function NextToStore(aHouse: THouseType; out aLoc: TKMPoint): Boolean;
  public
    constructor Create(aPlayer: TPlayerIndex);
    function FindPlaceForHouse(aHouse: THouseType; out aLoc: TKMPoint): Boolean;
  end;


implementation
uses KM_Houses, KM_Terrain, KM_PlayersCollection;


{ TKMCityPlanner }
constructor TKMCityPlanner.Create(aPlayer: TPlayerIndex);
begin
  inherited Create;
  fOwner := aPlayer;
end;


function TKMCityPlanner.FindPlaceForHouse(aHouse: THouseType; out aLoc: TKMPoint): Boolean;
begin
  case aHouse of
    ht_School:  Result := NextToStore(aHouse, aLoc);//Close to Store
                //aLoc := KMPoint(4,8);
    ht_Inn:     Result := NextToStore(aHouse, aLoc);//Close to Store
                //aLoc := KMPoint(12,8);
  end;

  //Result := True;
end;


function TKMCityPlanner.NextToStore(aHouse: THouseType; out aLoc: TKMPoint): Boolean;
var
  S: TKMHouse;
  I, K: Integer;
  Bid, BestBid: Single;
  StoreLoc, TempLoc: TKMPoint;
begin
  Result := False;
  S := fPlayers[fOwner].Houses.FindHouse(ht_Store, 0, 0, 1, True);

  if S = nil then Exit;

  BestBid := MaxSingle;
  StoreLoc := S.GetEntrance;

  for I := Max(StoreLoc.Y - 10, 1) to Min(StoreLoc.Y + 10, fTerrain.MapY - 1) do
  for K := Max(StoreLoc.X - 10, 1) to Min(StoreLoc.X + 10, fTerrain.MapX - 1) do
    if fTerrain.CanPlaceHouse(KMPoint(K,I), aHouse) then
    begin
      Bid := GetLength(KMPoint(K,I), StoreLoc) + Random * 3;
      if Bid < BestBid then
      begin
        aLoc := KMPoint(K,I);
        BestBid := Bid;
        Result := True;
      end;
    end;
end;


end.

