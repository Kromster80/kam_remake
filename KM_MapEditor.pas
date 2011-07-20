unit KM_MapEditor;
{$I KaM_Remake.inc}
interface
uses Classes, KromUtils, SysUtils,
      KM_Defaults;


type
  { Designed to store MapEd specific data }
  TKMMapEditor = class
  private
  public
    constructor Create;
  end;


implementation


{ TKMMapEditor }
constructor TKMMapEditor.Create;
begin
  Inherited Create;
end;


end.
