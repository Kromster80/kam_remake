unit KM_MapEditor;
{$I KaM_Remake.inc}
interface
uses Classes, KromUtils, SysUtils,
      KM_Defaults;


type
  { Designed to store MapEd specific data }
  TKMMapEditor = class
  public
    constructor Create;
    //Dummy
  end;


implementation


{ TKMMapEditor }
constructor TKMMapEditor.Create;
begin
  Inherited Create;
end;


end.
