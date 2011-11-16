unit KM_GameOptions;
{$I KaM_Remake.inc}
interface
uses
  KM_CommonTypes;

type
  TKMGameOptions = class
  private
  public
    Peacetime:word; //Peacetime in minutes (should it be in ticks?)
    procedure Save(SaveStream: TKMemoryStream);
    procedure Load(LoadStream: TKMemoryStream);
  end;


implementation


{ TKMGameOptions }
procedure TKMGameOptions.Load(LoadStream: TKMemoryStream);
begin
  LoadStream.Read(Peacetime);
end;


procedure TKMGameOptions.Save(SaveStream: TKMemoryStream);
begin
  SaveStream.Write(Peacetime);
end;


end.

