unit KM_Units;
{$I KaM_Remake.inc}
interface
uses
  Classes;

type
  TKMUnit = class
  public
    function PathfindingShouldAvoid: Boolean;
  end;


implementation


function TKMUnit.PathfindingShouldAvoid: Boolean;
begin
  Result := False;
end;


end.
