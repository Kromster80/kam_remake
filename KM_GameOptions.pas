unit KM_GameOptions;
{$I KaM_Remake.inc}
interface
uses
  KM_CommonClasses;


type
  TKMGameOptions = class
  public
    Peacetime: Word; //Peacetime in minutes
    SpeedPT, SpeedAfterPT: Byte;
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
    M.WriteAsText(aText);
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
  Result := M.ReadAsText;
  M.Free;
end;


end.

