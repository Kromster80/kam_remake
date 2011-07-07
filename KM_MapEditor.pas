unit KM_MapEditor;
{$I KaM_Remake.inc}
interface
uses Classes, KromUtils, SysUtils,
      KM_Defaults, KM_Utils;


type
  { Designed to store MapEd specific data }
  TKMMapEditor = class
  private
  public
    constructor Create;
  end;


implementation
uses KM_Terrain, KM_Sound, KM_PathFinding, KM_PlayersCollection, KM_ResourceGFX;


{ TKMMapEditor }
constructor TKMMapEditor.Create;
begin
  Inherited Create;
end;


end.
