unit KM_GameOptions;
{$I KaM_Remake.inc}
interface
uses
  KM_CommonClasses;


type
  //Game options set in MP lobby
  //(maybe later we could use some of these for SP games too)
  TKMGameOptions = class
  public
    Peacetime: Word; //Peacetime in minutes
    SpeedPT: Single; //Game speed during peacetime
    SpeedAfterPT: Single; //Game speed after peacetime (usually slower)
    RandomSeed: Integer;
    constructor Create;
    procedure Reset;
    procedure Save(SaveStream: TKMemoryStream);
    procedure Load(LoadStream: TKMemoryStream);
  end;


implementation


{ TKMGameOptions }
constructor TKMGameOptions.Create;
begin
  inherited;

  //Default values are not always 0
  Reset;
end;


//Resets values to defaults
procedure TKMGameOptions.Reset;
begin
  Peacetime := 0;
  SpeedPT := 1;
  SpeedAfterPT := 1;
  RandomSeed := 0; //Must be init later on. 0 is an erroneous value for KaMSeed
end;


procedure TKMGameOptions.Load(LoadStream: TKMemoryStream);
begin
  LoadStream.Read(Peacetime);
  LoadStream.Read(SpeedPT);
  LoadStream.Read(SpeedAfterPT);
  LoadStream.Read(RandomSeed);
end;


procedure TKMGameOptions.Save(SaveStream: TKMemoryStream);
begin
  SaveStream.Write(Peacetime);
  SaveStream.Write(SpeedPT);
  SaveStream.Write(SpeedAfterPT);
  SaveStream.Write(RandomSeed);
end;


end.
