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

    procedure SetAsText(const aText:string);
    function GetAsText:string;
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


procedure TKMGameOptions.SetAsText(const aText:string);
var M:TKMemoryStream;
begin
  M := TKMemoryStream.Create;
  try
    M.WriteAsText(aText);
    Load(M);
  finally
    M.Free;
  end;
end;


function TKMGameOptions.GetAsText:string;
var M:TKMemoryStream;
begin
  M := TKMemoryStream.Create;
  Save(M);
  Result := M.ReadAsText;
  M.Free;
end;

end.

