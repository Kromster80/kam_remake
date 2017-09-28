unit KM_MissionScript_Preview;
{$I KaM_Remake.inc}
interface
uses
  KM_MissionScript,
  KM_Defaults, KM_Points;


type
  TKMTilePreview = record
                       TileID: Byte;
                       TileHeight: Byte; //Used for calculating light
                       TileOwner: TKMHandIndex;
                       Revealed: Boolean;
                     end;

  TKMHandPreview = record
                       Color: Cardinal;
                       StartingLoc: TKMPoint;
                       CanHuman, CanAI: Boolean;
                     end;

  //Specially optimized mission parser for map previews
  TMissionParserPreview = class(TMissionParserCommon)
  private
    fMapX: Integer;
    fMapY: Integer;
    fHandPreview: array [0 .. MAX_HANDS-1] of TKMHandPreview;
    fMapPreview: array of TKMTilePreview;

    fRevealFor: array of TKMHandIndex;

    function GetTileInfo(X, Y: Integer): TKMTilePreview;
    function GetPlayerInfo(aIndex: Byte): TKMHandPreview;
    function LoadMapData(const aFileName: string): Boolean;
  protected
    function ProcessCommand(CommandType: TKMCommandType; P: array of Integer; const TextParam: AnsiString = ''): Boolean; override;
  public
    property MapPreview[X, Y: Integer]: TKMTilePreview read GetTileInfo;
    property PlayerPreview[aIndex: Byte]: TKMHandPreview read GetPlayerInfo;
    property MapX: Integer read fMapX;
    property MapY: Integer read fMapY;
    function LoadMission(const aFileName: string; const aRevealFor: array of TKMHandIndex): Boolean; reintroduce;
  end;


implementation
uses
  Classes, SysUtils, Math,
  KM_Resource, KM_ResHouses, KM_ResUnits,
  KM_CommonClasses, KM_CommonUtils;


{ TMissionParserPreview }
function TMissionParserPreview.GetTileInfo(X,Y: Integer): TKMTilePreview;
begin
  Result := fMapPreview[(Y-1)*fMapX + X-1];
end;


function TMissionParserPreview.GetPlayerInfo(aIndex: Byte): TKMHandPreview;
begin
  Result := fHandPreview[aIndex];
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

    SetLength(fMapPreview, fMapX * fMapY);
    for I := 0 to fMapX * fMapY - 1 do
    begin
      S.Read(fMapPreview[I].TileID);
      S.Seek(1, soFromCurrent);
      S.Read(fMapPreview[I].TileHeight); //Height (for lighting)
      S.Seek(20, soFromCurrent);

      //Fill in blanks
      fMapPreview[I].TileOwner := PLAYER_NONE;
      fMapPreview[I].Revealed := False;
    end;
  finally
    S.Free;
  end;

  Result := True;
end;


function TMissionParserPreview.ProcessCommand(CommandType: TKMCommandType; P: array of Integer; const TextParam: AnsiString = ''): Boolean;

  function PointInMap(X, Y: Integer): Boolean;
  begin
    Result := InRange(X, 1, fMapX)
          and InRange(Y, 1, fMapY);
  end;

  procedure SetOwner(X,Y: Word);
  begin
    fMapPreview[X-1 + (Y-1)*fMapX].TileOwner := fLastHand;
  end;

  function RevealForPlayer(aPlayerIndex: TKMHandIndex): Boolean;
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
    if not RevealForPlayer(fLastHand) then
      Exit;

    for I := Max(Y-Radius,1) to Min(Y+Radius,fMapY) do
    for K := Max(X-Radius,1) to Min(X+Radius,fMapX) do
    if (Sqr(X-K) + Sqr(Y-I)) <= Sqr(Radius) then
      fMapPreview[(I-1)*fMapX + K-1].Revealed := True;
  end;

var
  I, K: Integer;
  HA: THouseArea;
  Valid: Boolean;
  Loc: TKMPoint;
begin
  case CommandType of
    ct_SetCurrPlayer:   fLastHand := P[0];
    ct_SetHouse:        if InRange(P[0], Low(HouseIndexToType), High(HouseIndexToType))
                          and PointInMap(P[1]+1, P[2]+1) then
                        begin
                          RevealCircle(P[1]+1, P[2]+1, gRes.Houses[HouseIndexToType[P[0]]].Sight);
                          HA := gRes.Houses[HouseIndexToType[P[0]]].BuildArea;
                          for i:=1 to 4 do for k:=1 to 4 do
                            if HA[i,k]<>0 then
                              if InRange(P[1]+1+k-3, 1, fMapX) and InRange(P[2]+1+i-4, 1, fMapY) then
                                SetOwner(P[1]+1+k-3, P[2]+1+i-4);
                        end;
    ct_SetMapColor:     if InRange(fLastHand, 0, MAX_HANDS-1) then
                          fHandPreview[fLastHand].Color := gRes.Palettes.DefaultPalette.Color32(P[0]);
    ct_SetRGBColor:     if InRange(fLastHand, 0, MAX_HANDS-1) then
                          fHandPreview[fLastHand].Color := P[0] or $FF000000;
    ct_CenterScreen:    if PointInMap(P[0]+1, P[1]+1) then
                          fHandPreview[fLastHand].StartingLoc := KMPoint(P[0]+1,P[1]+1);
    ct_HumanPlayer:     //Default human player can be human, obviously
                        fHandPreview[P[0]].CanHuman := True;
    ct_UserPlayer:      if P[0] = -1 then
                          fHandPreview[fLastHand].CanHuman := True
                        else
                          fHandPreview[P[0]].CanHuman := True;
    ct_AIPlayer:        if P[0] = -1 then
                          fHandPreview[fLastHand].CanAI := True
                        else
                          fHandPreview[P[0]].CanAI := True;
    ct_SetRoad,
    ct_SetField,
    ct_SetWinefield,
    ct_SetFieldStaged,
    ct_SetWinefieldStaged:
                        if PointInMap(P[0]+1, P[1]+1) then
                          SetOwner(P[0]+1, P[1]+1);
    ct_SetUnit:         if PointInMap(P[1]+1, P[2]+1) and
                          not (UnitOldIndexToType[P[0]] in [ANIMAL_MIN..ANIMAL_MAX]) then //Skip animals
                        begin
                          SetOwner(P[1]+1, P[2]+1);
                          RevealCircle(P[1]+1, P[2]+1, gRes.Units[UnitOldIndexToType[P[0]]].Sight);
                        end;
    ct_SetStock:        if PointInMap(P[1]+1, P[2]+1) then
                        begin
                          //Set Store and roads below
                          ProcessCommand(ct_SetHouse,[11,P[0]+1,P[1]+1]);
                          ProcessCommand(ct_SetRoad, [   P[0]-2,P[1]+1]);
                          ProcessCommand(ct_SetRoad, [   P[0]-1,P[1]+1]);
                          ProcessCommand(ct_SetRoad, [   P[0]  ,P[1]+1]);
                        end;
    ct_SetGroup:        if InRange(P[0], Low(UnitIndexToType), High(UnitIndexToType)) and (UnitIndexToType[P[0]] <> ut_None)
                          and PointInMap(P[1]+1, P[2]+1) then
                          for I := 0 to P[5] - 1 do
                          begin
                            Loc := GetPositionInGroup2(P[1]+1,P[2]+1,TKMDirection(P[3]+1), I, P[4],fMapX,fMapY,Valid);
                            if Valid then
                            begin
                              SetOwner(Loc.X,Loc.Y);
                              RevealCircle(P[1]+1, P[2]+1, gRes.Units[UnitOldIndexToType[P[0]]].Sight);
                            end;
                          end;
    ct_ClearUp:         begin
                          if (P[0] = 255) then
                          begin
                            if RevealForPlayer(fLastHand) then
                              for I := 0 to fMapX * fMapY - 1 do
                                fMapPreview[I].Revealed := True;
                          end
                          else if PointInMap(P[0]+1, P[1]+1) then
                            RevealCircle(P[0]+1, P[1]+1, P[2]);
                        end;
  end;

  Result := True;
end;


//We use custom mission loader for speed (compare only used commands)
function TMissionParserPreview.LoadMission(const aFileName: string; const aRevealFor: array of TKMHandIndex): Boolean;
const
  Commands: array [0..15] of AnsiString = (
    '!SET_MAP', '!SET_MAP_COLOR', '!SET_RGB_COLOR', '!SET_AI_PLAYER', '!CENTER_SCREEN',
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
  for I := Low(aRevealFor) to High(aRevealFor) do
    fRevealFor[I] := aRevealFor[I];

  FillChar(fHandPreview, SizeOf(fHandPreview), #0);
  for I := 0 to MAX_HANDS-1 do
    fHandPreview[I].Color := DefaultTeamColors[I];

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
