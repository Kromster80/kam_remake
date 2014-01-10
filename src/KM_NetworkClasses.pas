unit KM_NetworkClasses;
{$I KaM_Remake.inc}
interface
uses
  Classes, SysUtils, StrUtils, KM_CommonClasses, KM_NetworkTypes, KM_Defaults;

type
  //Stores information about a multiplayer game to be sent: host -> server -> queriers
  TMPGameInfo = class
  public
    GameState: TMPGameState;
    PasswordLocked: Boolean;
    PlayerCount: Byte;
    Players: array[1..MAX_LOBBY_SLOTS] of record
                                            Name: AnsiString;
                                            Color: Cardinal;
                                            Connected: Boolean;
                                            PlayerType: TNetPlayerType;
                                          end;
    Description: UnicodeString;
    Map: UnicodeString;
    GameTime: TDateTime;
    function GetFormattedTime: UnicodeString;
    procedure LoadFromStream(aStream: TKMemoryStream);
    procedure SaveToStream(aStream: TKMemoryStream);
    function PlayersList: string;
    function ConnectedPlayerCount: Byte;
    function FormattedPlayerName(aIndex: Integer): UnicodeString;
  end;


implementation
uses KM_Utils, KM_ResTexts;


{ TMPGameInfo }
procedure TMPGameInfo.LoadFromStream(aStream: TKMemoryStream);
var I: Integer;
begin
  aStream.Read(GameState, SizeOf(GameState));
  aStream.Read(PasswordLocked);
  aStream.Read(PlayerCount);
  for I := 1 to PlayerCount do
  begin
    aStream.ReadA(Players[I].Name);
    aStream.Read(Players[I].Color);
    aStream.Read(Players[I].Connected);
    aStream.Read(Players[I].PlayerType, SizeOf(Players[I].PlayerType));
  end;
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
var I: Integer;
begin
  aStream.Write(GameState, SizeOf(GameState));
  aStream.Write(PasswordLocked);
  aStream.Write(PlayerCount);
  for I := 1 to PlayerCount do
  begin
    aStream.WriteA(Players[I].Name);
    aStream.Write(Players[I].Color);
    aStream.Write(Players[I].Connected);
    aStream.Write(Players[I].PlayerType, SizeOf(Players[I].PlayerType));
  end;
  aStream.WriteW(Description);
  aStream.WriteW(Map);
  aStream.Write(GameTime, SizeOf(GameTime));
end;


function TMPGameInfo.PlayersList: string;
var I: Integer;
begin
  Result := '';
  for I := 1 to PlayerCount do
    Result := Result + Players[I].Name + IfThen(I < PlayerCount, ', ');
end;


function TMPGameInfo.ConnectedPlayerCount: Byte;
var I: Integer;
begin
  Result := 0;
  for I := 1 to PlayerCount do
    if Players[I].Connected and (Players[I].PlayerType = nptHuman) then
      Inc(Result);
end;


function TMPGameInfo.FormattedPlayerName(aIndex: Integer): UnicodeString;
begin
  case Players[aIndex].PlayerType of
    nptHuman:    Result := UnicodeString(Players[aIndex].Name);
    nptComputer: Result := gResTexts[TX_LOBBY_SLOT_AI_PLAYER];
    nptClosed:   Result := gResTexts[TX_LOBBY_SLOT_CLOSED];
  end;
end;


end.
