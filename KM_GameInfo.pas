unit KM_GameInfo;
{$I KaM_Remake.inc}
interface
uses
  Classes, SysUtils, KM_Player, KM_CommonClasses, KM_Defaults;


type
  //Info that is relevant to any game, be it Save or a Mission
  TKMGameInfo = class
  private
    fParseError: string;
  public
    Title: AnsiString; //Used for campaigns and to store in savegames
    Version: AnsiString; //Savegame version, yet unused in maps, they always have actual version
    DATCRC: Cardinal; //CRC of defines .dat files
    TickCount: Cardinal;
    MissionMode: TKMissionMode; //Fighting or Build-a-City map
    MapSizeX, MapSizeY: Integer;
    VictoryCondition: AnsiString;
    DefeatCondition: AnsiString;

    PlayerCount: Byte;
    //Location name is string because for savegames we store players name there
    Enabled: array [0..MAX_PLAYERS-1] of Boolean;
    CanBeHuman: array [0..MAX_PLAYERS-1] of Boolean;
    LocationName: array [0..MAX_PLAYERS-1] of AnsiString;
    PlayerTypes: array [0..MAX_PLAYERS-1] of TPlayerType;
    ColorID: array [0..MAX_PLAYERS-1] of Integer;
    Team: array [0..MAX_PLAYERS-1] of Integer;

    //To be used in Savegames
    procedure Save(SaveStream: TKMemoryStream);
    procedure Load(LoadStream: TKMemoryStream);

    property ParseError: string read fParseError;
    function IsValid(aCheckDATCRC: Boolean): Boolean;
    function AICount: Byte;
    function SizeText: string;
    function MissionModeText: string;
    function GetTimeText: string;
    function GetTitleWithTime: string;
  end;


implementation
uses KM_Resource, KM_TextLibrary, KM_Utils;


{ TKMGameInfo }
procedure TKMGameInfo.Load(LoadStream: TKMemoryStream);
var
  s: AnsiString;
  I: Integer;
begin
  LoadStream.Read(s);
  if s <> 'KaM_GameInfo' then begin
    fParseError := Format(fTextLibrary[TX_SAVE_UNSUPPORTED_FORMAT], [Copy(s, 1, 8)]);
    Exit;
  end;

  LoadStream.Read(Version);
  if Version <> GAME_REVISION then begin
    fParseError := Format(fTextLibrary[TX_SAVE_UNSUPPORTED_VERSION], [Version]);
    Exit;
  end;

  LoadStream.Read(DATCRC); //Don't check it here (maps don't care), if required somebody else will check it

  LoadStream.Read(Title); //GameName
  LoadStream.Read(TickCount); //TickCount
  LoadStream.Read(MissionMode, SizeOf(MissionMode));
  LoadStream.Read(MapSizeX);
  LoadStream.Read(MapSizeY);
  LoadStream.Read(VictoryCondition);
  LoadStream.Read(DefeatCondition);

  LoadStream.Read(PlayerCount);
  for I := 0 to PlayerCount - 1 do
  begin
    LoadStream.Read(CanBeHuman[I]);
    LoadStream.Read(Enabled[I]);
    LoadStream.Read(LocationName[I]);
    LoadStream.Read(PlayerTypes[I], SizeOf(PlayerTypes[I]));
    LoadStream.Read(ColorID[I]);
    LoadStream.Read(Team[I]);
  end;
end;


procedure TKMGameInfo.Save(SaveStream: TKMemoryStream);
var I: Integer;
begin
  SaveStream.Write('KaM_GameInfo');
  SaveStream.Write(AnsiString(GAME_REVISION)); //Save current revision
  SaveStream.Write(fResource.GetDATCRC);

  SaveStream.Write(Title); //GameName
  SaveStream.Write(TickCount);
  SaveStream.Write(MissionMode, SizeOf(MissionMode));
  SaveStream.Write(MapSizeX);
  SaveStream.Write(MapSizeY);
  SaveStream.Write(VictoryCondition);
  SaveStream.Write(DefeatCondition);

  SaveStream.Write(PlayerCount);
  for I := 0 to PlayerCount - 1 do
  begin
    SaveStream.Write(CanBeHuman[I]);
    SaveStream.Write(Enabled[I]);
    SaveStream.Write(LocationName[I]);
    SaveStream.Write(PlayerTypes[I], SizeOf(PlayerTypes[I]));
    SaveStream.Write(ColorID[I]);
    SaveStream.Write(Team[I]);
  end;
end;


function TKMGameInfo.IsValid(aCheckDATCRC: Boolean): Boolean;
begin
  Result := (PlayerCount > 0) and (not aCheckDATCRC or (DATCRC = fResource.GetDATCRC));
end;


//How many AI players are in this game,
//so that Lobby could automatically create this much AIs when the save is selected
function TKMGameInfo.AICount: Byte;
var I: Integer;
begin
  Result := 0;
  for I := 0 to PlayerCount - 1 do
    if PlayerTypes[I] = pt_Computer then
      Inc(Result);
end;


function TKMGameInfo.SizeText: string;
begin
  Result := MapSizeText(MapSizeX, MapSizeY);
end;


function TKMGameInfo.MissionModeText: string;
begin
  case MissionMode of
    mm_Normal: Result := fTextLibrary[TX_MODE_BUILD_FIGHT];
    mm_Tactic: Result := fTextLibrary[TX_MODE_FIGHTING]
    else       Result := 'Unknown';
  end;
end;


function TKMGameInfo.GetTimeText: string;
begin
  Result := TimeToString(TickCount/24/60/60/10);
end;


function TKMGameInfo.GetTitleWithTime: string;
begin
  if IsValid(True) then
    Result := Title + ' ' + TimeToString(TickCount/24/60/60/10)
  else
    Result := Title;
end;


end.

