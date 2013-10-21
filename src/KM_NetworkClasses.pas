unit KM_NetworkClasses;
{$I KaM_Remake.inc}
interface
uses
  Classes, SysUtils, KM_CommonClasses, KM_NetworkTypes;

type
  //Stores information about a multiplayer game to be sent: host -> server -> queriers
  TMPGameInfo = class
  public
    GameState: TMPGameState;
    PasswordLocked: Boolean;
    PlayerCount: Byte;
    Players: UnicodeString;
    Description: UnicodeString;
    Map: UnicodeString;
    GameTime: TDateTime;
    function GetFormattedTime: UnicodeString;
    procedure LoadFromStream(aStream: TKMemoryStream);
    procedure SaveToStream(aStream: TKMemoryStream);
    function GetAsHTML: string;
  end;


implementation
uses KM_Utils;


{ TMPGameInfo }
procedure TMPGameInfo.LoadFromStream(aStream: TKMemoryStream);
begin
  aStream.Read(GameState, SizeOf(GameState));
  aStream.Read(PasswordLocked);
  aStream.Read(PlayerCount);
  aStream.ReadW(Players);
  aStream.ReadW(Description);
  aStream.ReadW(Map);
  aStream.Read(GameTime, SizeOf(GameTime));
end;


//Return string representation of games length
function TMPGameInfo.GetFormattedTime: UnicodeString;
begin
  if GameTime >= 0 then
    Result := TimeToString(GameTime)
  else
    Result := '';
end;


procedure TMPGameInfo.SaveToStream(aStream: TKMemoryStream);
begin
  aStream.Write(GameState, SizeOf(GameState));
  aStream.Write(PasswordLocked);
  aStream.Write(PlayerCount);
  aStream.WriteW(Players);
  aStream.WriteW(Description);
  aStream.WriteW(Map);
  aStream.Write(GameTime, SizeOf(GameTime));
end;


function TMPGameInfo.GetAsHTML: string;
begin
  Result := '';
  Result := Result + Map;
  Result := Result + '<BR>' + GetFormattedTime;
  Result := Result + '<BR>' + Players;
end;


end.
