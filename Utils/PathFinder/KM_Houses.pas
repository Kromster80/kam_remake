unit KM_Houses;
{$I KaM_Remake.inc}
interface
uses
   Classes,  Math, SysUtils,
   KM_CommonClasses,  KM_Points;

type
  //Stub for Pathfinder
  TKMHouse = class
  public
    function InReach(aPos: TKMPoint; aDistance: Single): Boolean;
  end;

  TKMHouseSchool = class(TKMHouse);


implementation


{ TKMHouse }
function TKMHouse.InReach(aPos: TKMPoint; aDistance: Single): Boolean;
begin
  Result := True;
end;


end.
