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
    TickCount: cardinal;
    MissionMode: TKMissionMode; //Fighting or Build-a-City map
    MapSizeX, MapSizeY: Integer;
    VictoryCondition: AnsiString;
    DefeatCondition: AnsiString;

    PlayerCount: byte;
    LocationName: array[0..MAX_PLAYERS-1] of AnsiString;
    PlayerTypes: array[0..MAX_PLAYERS-1] of TPlayerType;
    ColorID: array[0..MAX_PLAYERS-1] of integer;
    Team: array[0..MAX_PLAYERS-1] of integer;

    //To be used in Savegames
    procedure Save(SaveStream: TKMemoryStream);
    procedure Load(LoadStream: TKMemoryStream);

    property ParseError: string read fParseError;
    function IsValid: Boolean;
    function AICount: Integer;
    function MapSizeText:string;
    function MissionModeText:string;
    function GetTitleWithTime:string;
  end;


implementation
uses KM_TextLibrary;


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

  LoadStream.Read(Title); //GameName
  LoadStream.Read(TickCount); //TickCount
  LoadStream.Read(MissionMode, SizeOf(MissionMode));
  LoadStream.Read(MapSizeX);
  LoadStream.Read(MapSizeY);
  LoadStream.Read(VictoryCondition);
  LoadStream.Read(DefeatCondition);

  LoadStream.Read(PlayerCount);
  for i:=0 to PlayerCount-1 do
  begin
    LoadStream.Read(LocationName[i]);
    LoadStream.Read(PlayerTypes[i], SizeOf(PlayerTypes[i]));
    LoadStream.Read(ColorID[i]);
    LoadStream.Read(Team[i]);
  end;
end;


procedure TKMGameInfo.Save(SaveStream: TKMemoryStream);
var i:integer;
begin
  SaveStream.Write('KaM_GameInfo');
  SaveStream.Write(AnsiString(GAME_REVISION)); //Save current revision

  SaveStream.Write(Title); //GameName
  SaveStream.Write(TickCount);
  SaveStream.Write(MissionMode, SizeOf(MissionMode));
  SaveStream.Write(MapSizeX);
  SaveStream.Write(MapSizeY);
  SaveStream.Write(VictoryCondition);
  SaveStream.Write(DefeatCondition);

  SaveStream.Write(PlayerCount);
  for i:=0 to PlayerCount-1 do
  begin
    SaveStream.Write(LocationName[i]);
    SaveStream.Write(PlayerTypes[i], SizeOf(PlayerTypes[i]));
    SaveStream.Write(ColorID[i]);
    SaveStream.Write(Team[i]);
  end;
end;


function TKMGameInfo.IsValid:boolean;
begin
  Result := PlayerCount > 0;
end;


//How many AI players are in this game,
//so that Lobby could automatically create this much AIs when the save is selected
function TKMGameInfo.AICount:integer;
var i:integer;
begin
  Result := 0;
  for i:=0 to PlayerCount-1 do
    if PlayerTypes[i] = pt_Computer then
      inc(Result);
end;


function TKMGameInfo.MapSizeText: string;
begin
  case MapSizeX * MapSizeY of
            1.. 48* 48: Result := 'XS';
     48* 48+1.. 72* 72: Result := 'S';
     72* 72+1..112*112: Result := 'M';
    112*112+1..176*176: Result := 'L';
    176*176+1..256*256: Result := 'XL';
    256*256+1..320*320: Result := 'XXL';
    else                Result := '???';
  end;
end;


function TKMGameInfo.MissionModeText:string;
begin
  case MissionMode of
    mm_Normal: Result := fTextLibrary[TX_MODE_BUILD_FIGHT];
    mm_Tactic: Result := fTextLibrary[TX_MODE_FIGHTING]
    else       Result := 'Unknown';
  end;
end;


function TKMGameInfo.GetTitleWithTime:string;
begin
  if IsValid then
    Result := Title + ' ' + FormatDateTime('hh:nn:ss', TickCount/24/60/60/10)
  else
    Result := Title;
end;


end.

