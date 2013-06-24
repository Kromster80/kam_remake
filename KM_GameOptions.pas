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
    procedure Save(SaveStream: TKMemoryStream);
    procedure Load(LoadStream: TKMemoryStream);

    procedure Reset;
    procedure SetAsText(const aText: string);
    function GetAsText: string;
    constructor Create;
  end;


implementation


{ TKMGameOptions }
constructor TKMGameOptions.Create;
begin
  inherited;

  //Default values are not always 0
  Reset;
end;


procedure TKMGameOptions.Load(LoadStream: TKMemoryStream);
begin
  LoadStream.Read(Peacetime);
  LoadStream.Read(SpeedPT);
  LoadStream.Read(SpeedAfterPT);
end;


procedure TKMGameOptions.Save(SaveStream: TKMemoryStream);
begin
  SaveStream.Write(Peacetime);
  SaveStream.Write(SpeedPT);
  SaveStream.Write(SpeedAfterPT);
end;


//Resets values to defaults
procedure TKMGameOptions.Reset;
begin
  Peacetime := 0;
  SpeedPT := 1;
  SpeedAfterPT := 1;
end;


procedure TKMGameOptions.SetAsText(const aText: string);
var M: TKMemoryStream;
begin
  M := TKMemoryStream.Create;
  try
    M.SetAsText(aText);
    Load(M);
  finally
    M.Free;
  end;
end;


function TKMGameOptions.GetAsText: string;
var M: TKMemoryStream;
begin
  M := TKMemoryStream.Create;
  Save(M);
  Result := M.GetAsText;
  M.Free;
end;


end.

