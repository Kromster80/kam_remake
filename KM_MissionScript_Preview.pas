unit KM_MissionScript_Preview;
{$I KaM_Remake.inc}
interface
uses
  Classes, KromUtils, SysUtils, Math,
  KM_CommonClasses, KM_Defaults, KM_Points, KM_MissionScript, KM_Utils;


type
  TTilePreviewInfo = record
                       TileID: Byte;
                       TileHeight: Byte; //Used for calculating light
                       TileOwner: TPlayerIndex;
                       Revealed: Boolean;
                     end;

  TPlayerPreviewInfo = record
                         Color: Cardinal;
                         StartingLoc: TKMPoint;
                         CanHuman, CanAI: Boolean;
                       end;

  //Specially optimized mission parser for map previews
  TMissionParserPreview = class(TMissionParserCommon)
  private
    fMapX: Integer;
    fMapY: Integer;
    fPlayerPreview: array [0 .. MAX_PLAYERS-1] of TPlayerPreviewInfo;
    fMapPreview: array [1 .. MAX_MAP_SIZE * MAX_MAP_SIZE] of TTilePreviewInfo;

    fRevealFor: array of TPlayerIndex;

    function GetTileInfo(X, Y: Integer): TTilePreviewInfo;
    function GetPlayerInfo(aIndex: Byte): TPlayerPreviewInfo;
    function LoadMapData(const aFileName: string): Boolean;
  protected
    function ProcessCommand(CommandType: TKMCommandType; P: array of Integer; TextParam: AnsiString = ''): Boolean; override;
  public
    property MapPreview[X, Y: Integer]: TTilePreviewInfo read GetTileInfo;
    property PlayerPreview[aIndex: Byte]: TPlayerPreviewInfo read GetPlayerInfo;
    property MapX: Integer read fMapX;
    property MapY: Integer read fMapY;
    function LoadMission(const aFileName: string; const aRevealFor: array of TPlayerIndex): Boolean; reintroduce;
  end;


implementation
uses
  KM_Resource, KM_ResourceHouse, KM_ResourceUnit;


{ TMissionParserPreview }
function TMissionParserPreview.GetTileInfo(X,Y: Integer): TTilePreviewInfo;
begin
  Result := fMapPreview[(Y-1)*fMapX + X];
end;


function TMissionParserPreview.GetPlayerInfo(aIndex: Byte): TPlayerPreviewInfo;
begin
  Result := fPlayerPreview[aIndex];
end;


//Load terrain data into liteweight structure, take only what we need for preview
function TMissionParserPreview.LoadMapData(const aFileName: string): Boolean;
var
  I: Integer;
  S: TKMemoryStream;
  NewX, NewY: Integer;
begin
  Result := False;

  if not FileExists(aFileName) then
    Exit;

  S := TKMemoryStream.Create;
  try
    S.LoadFromFile(aFileName);
    S.Read(NewX); //We read header to new variables to avoid damage to existing map if header is wrong
    S.Read(NewY);
    Assert((NewX <= MAX_MAP_SIZE) and (NewY <= MAX_MAP_SIZE), 'Can''t open the map cos it has too big dimensions');
    fMapX := NewX;
    fMapY := NewY;
    for I := 1 to fMapX * fMapY do
    begin
      S.Read(fMapPreview[I].TileID);
      S.Seek(1, soFromCurrent);
      S.Read(fMapPreview[I].TileHeight); //Height (for lighting)
      S.Seek(20, soFromCurrent);
    end;
  finally
    S.Free;
  end;

  Result := True;
end;


function TMissionParserPreview.ProcessCommand(CommandType: TKMCommandType; P: array of Integer; TextParam: AnsiString = ''): Boolean;

  procedure SetOwner(X,Y: Word);
  begin
    fMapPreview[X + Y*fMapX].TileOwner := fLastPlayer;
  end;

  function RevealForPlayer(aPlayerIndex: TPlayerIndex): Boolean;
  var
    I: Integer;
  begin
    Result := False;
    for I := 0 to Length(fRevealFor)-1 do
    if (fRevealFor[I] = aPlayerIndex) then
    begin
      Result := True;
      Exit;
    end;
  end;

  procedure RevealCircle(X,Y,Radius: Word);
  var
    I, K: Word;
  begin
    if not RevealForPlayer(fLastPlayer) then
      Exit;

    for I := Max(Y-Radius,1) to Min(Y+Radius,fMapY) do
    for K := Max(X-Radius,1) to Min(X+Radius,fMapX) do
    if (Sqr(X-K) + Sqr(Y-I)) <= Sqr(Radius) then
      fMapPreview[(I-1)*fMapX + K].Revealed := True;
  end;

var
  I, K: Integer;
  HA: THouseArea;
  Valid: Boolean;
  Loc: TKMPoint;
begin
  case CommandType of
    ct_SetCurrPlayer:   fLastPlayer := P[0];
    ct_SetHouse:        if InRange(P[0], Low(HouseIndexToType), High(HouseIndexToType)) then
                        begin
                          RevealCircle(P[1]+1, P[2]+1, fResource.HouseDat[HouseIndexToType[P[0]]].Sight);
                          HA := fResource.HouseDat[HouseIndexToType[P[0]]].BuildArea;
                          for i:=1 to 4 do for k:=1 to 4 do
                            if HA[i,k]<>0 then
                              if InRange(P[1]+1+k-3, 1, fMapX) and InRange(P[2]+1+i-4, 1, fMapY) then
                                SetOwner(P[1]+1+k-3, P[2]+1+i-4);
                        end;
    ct_SetMapColor:     if InRange(fLastPlayer, 0, MAX_PLAYERS-1) then
                          fPlayerPreview[fLastPlayer].Color := fResource.Palettes.DefDal.Color32(P[0]);
    ct_CenterScreen:    fPlayerPreview[fLastPlayer].StartingLoc := KMPoint(P[0]+1,P[1]+1);
    ct_HumanPlayer:     //Default human player can be human, obviously
                        fPlayerPreview[P[0]].CanHuman := True;
    ct_UserPlayer:      if P[0] = -1 then
                          fPlayerPreview[fLastPlayer].CanHuman := True
                        else
                          fPlayerPreview[P[0]].CanHuman := True;
    ct_AIPlayer:        if P[0] = -1 then
                          fPlayerPreview[fLastPlayer].CanAI := True
                        else
                          fPlayerPreview[P[0]].CanAI := True;
    ct_SetRoad,
    ct_SetField,
    ct_SetWinefield:    SetOwner(P[0]+1, P[1]+1);
    ct_SetUnit:         if not (UnitOldIndexToType[P[0]] in [ANIMAL_MIN..ANIMAL_MAX]) then //Skip animals
                        begin
                          SetOwner(P[1]+1, P[2]+1);
                          RevealCircle(P[1]+1, P[2]+1, fResource.UnitDat.UnitsDat[UnitOldIndexToType[P[0]]].Sight);
                        end;
    ct_SetStock:        begin
                          //Set Store and roads below
                          ProcessCommand(ct_SetHouse,[11,P[0]+1,P[1]+1]);
                          ProcessCommand(ct_SetRoad, [   P[0]-2,P[1]+1]);
                          ProcessCommand(ct_SetRoad, [   P[0]-1,P[1]+1]);
                          ProcessCommand(ct_SetRoad, [   P[0]  ,P[1]+1]);
                        end;
    ct_SetGroup:        if InRange(P[0], Low(UnitIndexToType), High(UnitIndexToType)) and (UnitIndexToType[P[0]] <> ut_None) then
                          for I := 0 to P[5] - 1 do
                          begin
                            Loc := GetPositionInGroup2(P[1]+1,P[2]+1,TKMDirection(P[3]+1), I, P[4],fMapX,fMapY,Valid);
                            if Valid then
                            begin
                              SetOwner(Loc.X,Loc.Y);
                              RevealCircle(P[1]+1, P[2]+1, fResource.UnitDat.UnitsDat[UnitOldIndexToType[P[0]]].Sight);
                            end;
                          end;
    ct_ClearUp:         begin
                          if (P[0] = 255) then
                          begin
                            if RevealForPlayer(fLastPlayer) then
                              for I := 1 to MAX_MAP_SIZE * MAX_MAP_SIZE do
                                fMapPreview[i].Revealed := True;
                          end
                          else
                            RevealCircle(P[0]+1, P[1]+1, P[2]);
                        end;
  end;

  Result := True;
end;


//We use custom mission loader for speed (compare only used commands)
function TMissionParserPreview.LoadMission(const aFileName: string; const aRevealFor: array of TPlayerIndex): Boolean;
const
  Commands: array [0..14] of AnsiString = (
    '!SET_MAP', '!SET_MAP_COLOR', '!SET_AI_PLAYER', '!CENTER_SCREEN',
    '!SET_CURR_PLAYER', '!SET_HUMAN_PLAYER', '!SET_USER_PLAYER',
    '!SET_STREET', '!SET_FIELD', '!SET_WINEFIELD', '!SET_STOCK',
    '!SET_HOUSE', '!CLEAR_UP', '!SET_UNIT', '!SET_GROUP');
var
  FileText: AnsiString;
  I: Integer;
begin
  inherited LoadMission(aFileName);

  Result := False;

  SetLength(fRevealFor, Length(aRevealFor));
  for I := 0 to Length(aRevealFor) - 1 do
    fRevealFor[I] := aRevealFor[I];

  FillChar(fMapPreview, SizeOf(fMapPreview), #0);
  FillChar(fPlayerPreview, SizeOf(fPlayerPreview), #0);

  for I := Low(fMapPreview) to High(fMapPreview) do
    fMapPreview[I].TileOwner := PLAYER_NONE;

  FileText := ReadMissionFile(aFileName);
  if FileText = '' then
    Exit;

  //We need to load map dimensions first, so that SetGroup could access map bounds
  if not LoadMapData(ChangeFileExt(fMissionFileName, '.map')) then
    Exit;

  if not TokenizeScript(FileText, 6, Commands) then
    Exit;

  Result := (fFatalErrors = '');
end;


end.
