unit KM_GameInfo;
{$I KaM_Remake.inc}
interface
uses
  KM_Hand, KM_CommonClasses, KM_Defaults;


type
  //Info that is relevant to any game, be it Save or a Mission
  TKMGameInfo = class
  private
    fParseError: UnicodeString;
  public
    Title: UnicodeString; //Used for campaigns and to store in savegames
    Version: AnsiString; //Savegame version, yet unused in maps, they always have actual version
    MapCRC: Cardinal; //CRC of entire map, used for reporting which map was played to master server
    DATCRC: Cardinal; //CRC of defines .dat files (data\defines)
    TickCount: Cardinal; //Current tick count of the game (unused for maps)
    SaveTimestamp: TDateTime; //UTC time when the save was created (unused for maps)
    MissionMode: TKMissionMode; //Fighting or Build-a-City map
    MapSizeX, MapSizeY: Integer;

    PlayerCount: Byte;
    Enabled: array [0..MAX_HANDS-1] of Boolean;
    CanBeHuman: array [0..MAX_HANDS-1] of Boolean;
    OwnerNikname: array [0..MAX_HANDS-1] of AnsiString; //Nikname of the player who plays this location
    HandTypes: array [0..MAX_HANDS-1] of THandType;
    ColorID: array [0..MAX_HANDS-1] of Integer;
    Team: array [0..MAX_HANDS-1] of Integer;

    //To be used in Savegames
    procedure Save(SaveStream: TKMemoryStream);
    procedure Load(LoadStream: TKMemoryStream);

    property ParseError: UnicodeString read fParseError;
    function IsValid(aCheckDATCRC: Boolean): Boolean;
    function AICount: Byte;
    function HumanCount: Byte;
    function HumanUsableLocations: TKMHandIndexArray;
    function GetTimeText: UnicodeString;
    function GetTitleWithTime: UnicodeString;
    function GetSaveTimestamp: UnicodeString;
    function ColorUsed(aColorID: Integer): Boolean;
  end;


implementation
uses
  SysUtils,
  KM_Resource, KM_ResTexts, KM_CommonUtils;


{ TKMGameInfo }
procedure TKMGameInfo.Load(LoadStream: TKMemoryStream);
var
  s: AnsiString;
  I: Integer;
begin
  LoadStream.ReadA(s);
  if s <> 'KaM_GameInfo' then
  begin
    fParseError := Format(gResTexts[TX_SAVE_UNSUPPORTED_FORMAT], [Copy(s, 1, 8)]);
    Exit;
  end;

  LoadStream.ReadA(Version);
  if Version <> GAME_REVISION then
  begin
    fParseError := Format(gResTexts[TX_SAVE_UNSUPPORTED_VERSION], [Version]);
    Exit;
  end;

  LoadStream.Read(DATCRC); //Don't check it here (maps don't care), if required somebody else will check it
  LoadStream.Read(MapCRC);

  LoadStream.ReadW(Title); //GameName
  LoadStream.Read(TickCount);
  LoadStream.Read(SaveTimestamp);
  LoadStream.Read(MissionMode, SizeOf(MissionMode));
  LoadStream.Read(MapSizeX);
  LoadStream.Read(MapSizeY);

  LoadStream.Read(PlayerCount);
  for I := 0 to PlayerCount - 1 do
  begin
    LoadStream.Read(CanBeHuman[I]);
    LoadStream.Read(Enabled[I]);
    LoadStream.ReadA(OwnerNikname[I]);
    LoadStream.Read(HandTypes[I], SizeOf(HandTypes[I]));
    LoadStream.Read(ColorID[I]);
    LoadStream.Read(Team[I]);
  end;
end;


procedure TKMGameInfo.Save(SaveStream: TKMemoryStream);
var I: Integer;
begin
  SaveStream.WriteA('KaM_GameInfo');
  SaveStream.WriteA(GAME_REVISION); //Save current revision
  SaveStream.Write(gRes.GetDATCRC);
  SaveStream.Write(MapCRC);

  SaveStream.WriteW(Title); //GameName
  SaveStream.Write(TickCount);
  SaveStream.Write(SaveTimestamp);
  SaveStream.Write(MissionMode, SizeOf(MissionMode));
  SaveStream.Write(MapSizeX);
  SaveStream.Write(MapSizeY);

  SaveStream.Write(PlayerCount);
  for I := 0 to PlayerCount - 1 do
  begin
    SaveStream.Write(CanBeHuman[I]);
    SaveStream.Write(Enabled[I]);
    SaveStream.WriteA(OwnerNikname[I]);
    SaveStream.Write(HandTypes[I], SizeOf(HandTypes[I]));
    SaveStream.Write(ColorID[I]);
    SaveStream.Write(Team[I]);
  end;
end;


function TKMGameInfo.IsValid(aCheckDATCRC: Boolean): Boolean;
begin
  Result := (PlayerCount > 0) and (not aCheckDATCRC or (DATCRC = gRes.GetDATCRC));
end;


//How many AI players are in this game,
//so that Lobby could automatically create this much AIs when the save is selected
function TKMGameInfo.AICount: Byte;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to PlayerCount - 1 do
    if HandTypes[I] = hndComputer then
      Inc(Result);
end;


function TKMGameInfo.HumanCount: Byte;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to PlayerCount - 1 do
    if Enabled[I] and (HandTypes[I] = hndHuman) then
      Inc(Result);
end;


function TKMGameInfo.HumanUsableLocations: TKMHandIndexArray;
var
  I: Integer;
begin
  SetLength(Result, 0);
  for I := 0 to MAX_HANDS - 1 do
    if CanBeHuman[I] then
    begin
      SetLength(Result, Length(Result)+1);
      Result[Length(Result)-1] := I;
    end;
end;


function TKMGameInfo.GetTimeText: UnicodeString;
begin
  Result := TimeToString(TickCount/24/60/60/10);
end;


function TKMGameInfo.GetTitleWithTime: UnicodeString;
begin
  if IsValid(True) then
    Result := Title + ' ' + TimeToString(TickCount/24/60/60/10)
  else
    Result := Title;
end;


function TKMGameInfo.GetSaveTimestamp: UnicodeString;
begin
  Result := FormatDateTime('ddddd t', UTCToLocal(SaveTimestamp));
end;


function TKMGameInfo.ColorUsed(aColorID: Integer): Boolean;
var I: Integer;
begin
  for I := 0 to MAX_HANDS - 1 do
    if Enabled[I] and (ColorID[I] = aColorID) then
    begin
      Result := True;
      Exit;
    end;
  Result := False;
end;


end.

