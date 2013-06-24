unit KM_Terrain;
{$I KaM_Remake.inc}
interface
uses Types, Math, SysUtils,
  KM_Defaults, KM_Points;


const
  MAX_SIZE = 256;


type
  TKMTerrain = class
  public
    MapX: Word;
    MapY: Word;
    Land: array [1..MAX_SIZE, 1..MAX_SIZE] of record
      IsUnit: Pointer;
      Passability: TPassabilitySet; //Meant to be set of allowed actions on the tile
    end;
    function TileInMapCoords(X,Y:integer; Inset: Byte=0): Boolean;
    function CanWalkDiagonaly(const aFrom: TKMPoint; tx, ty: SmallInt): Boolean;
    function TileIsLocked(aLoc:TKMPoint): Boolean;
    function GetConnectID(aWalkConnect: TWalkConnect; Loc:TKMPoint): Byte;
    function CheckPassability(Loc:TKMPoint; aPass:TPassability): Boolean;
  end;


var
  gTerrain: TKMTerrain;


implementation


{ TKMTerrain }
function TKMTerrain.CanWalkDiagonaly(const aFrom: TKMPoint; tx, ty: SmallInt): Boolean;
begin
  Result := True;
end;


function TKMTerrain.CheckPassability(Loc: TKMPoint; aPass: TPassability): Boolean;
begin
  Result := TileInMapCoords(Loc.X,Loc.Y) and (aPass in Land[Loc.Y,Loc.X].Passability);
end;

function TKMTerrain.GetConnectID(aWalkConnect: TWalkConnect; Loc: TKMPoint): Byte;
begin
  Result := 1;
end;

function TKMTerrain.TileInMapCoords(X,Y:integer; Inset: Byte=0): Boolean;
begin
  Result := InRange(X,1+Inset,MapX-1-Inset) and InRange(Y,1+Inset,MapY-1-Inset);
end;


function TKMTerrain.TileIsLocked(aLoc: TKMPoint): Boolean;
begin
  Result := False;
end;


end.
