unit KM_Houses;
{$I KaM_Remake.inc}
interface
uses
   Classes,  Math, SysUtils,
   KM_CommonClasses,  KM_Points;

type
  TKMHouse = class
  public
    function InReach(aPos: TKMPoint; aDistance: Single): Boolean;
  end;


implementation
uses
  KM_CommonTypes, KM_Resource, KM_ResourceWares, KM_ResourceHouse, KM_CommonUtils;


{ TKMHouse }
function TKMHouse.InReach(aPos: TKMPoint; aDistance: Single): Boolean;
begin
  Result := True;
end;


end.
