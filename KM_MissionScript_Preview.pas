unit KM_MissionScript_Preview;
{$I KaM_Remake.inc}
interface
uses
  Classes, KromUtils, SysUtils, Math,
  KM_Utils, KM_CommonClasses, KM_Defaults, KM_Points, KM_MissionScript,
  KM_Terrain, KM_UnitGroups;


type
  TTilePreviewInfo = record
                       TileID: Byte;
                       TileHeight: Byte; //Used for calculating light
                       TileOwner: Byte;
                       Revealed: Boolean;
                     end;

  TPlayerPreviewInfo = record
                         Color: Cardinal;
                         StartingLoc: TKMPoint;
                       end;

  //Specially optimized mission parser for map previews
  TMissionParserPreview = class(TMissionParserCommon)
  private
    fMapX: Integer;
    fMapY: Integer;
    fPlayerPreview: array [1 .. MAX_PLAYERS] of TPlayerPreviewInfo;
    fMapPreview: array [1 .. MAX_MAP_SIZE * MAX_MAP_SIZE] of TTilePreviewInfo;

    fLastPlayer: Integer;
    fHumanPlayer: Integer;

    function GetTileInfo(X, Y: Integer): TTilePreviewInfo;
    function GetPlayerInfo(aIndex: Byte): TPlayerPreviewInfo;
    procedure LoadMapData(const aFileName: string);
    procedure ProcessCommand(CommandType: TKMCommandType; const P: array of Integer);
  public
    property MapPreview[X, Y: Integer]: TTilePreviewInfo read GetTileInfo;
    property PlayerPreview[aIndex: Byte]: TPlayerPreviewInfo read GetPlayerInfo;
    property MapX: Integer read fMapX;
    property MapY: Integer read fMapY;
    function LoadMission(const aFileName: string): Boolean; override;
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
procedure TMissionParserPreview.LoadMapData(const aFileName: string);
var
  I: Integer;
  S: TKMemoryStream;
  NewX, NewY: Integer;
begin
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
end;


procedure TMissionParserPreview.ProcessCommand(CommandType: TKMCommandType; const P: array of integer);

  procedure SetOwner(X,Y: Word);
  begin
    fMapPreview[X + Y*fMapX].TileOwner := fLastPlayer;
  end;

  procedure RevealCircle(X,Y,Radius: Word);
  var i,k:Word;
  begin
    if (fHumanPlayer = 0) or (fHumanPlayer <> fLastPlayer) then exit;
    for i:=max(Y-Radius,1) to min(Y+Radius,fMapY) do
    for k:=max(X-Radius,1) to min(X+Radius,fMapX) do
       if (sqr(X-k) + sqr(Y-i)) <= sqr(Radius) then
         fMapPreview[(i-1)*fMapX + k].Revealed := True;
  end;

var i,k:integer; HA:THouseArea; Valid: Boolean; Loc: TKMPoint;
begin
  case CommandType of
    ct_SetCurrPlayer:  fLastPlayer := P[0]+1;
    ct_SetHumanPlayer: fHumanPlayer := P[0]+1;
    ct_SetHouse:       if InRange(P[0], Low(HouseIndexToType), High(HouseIndexToType)) then
                       begin
                         RevealCircle(P[1]+1, P[2]+1, fResource.HouseDat[HouseIndexToType[P[0]]].Sight);
                         HA := fResource.HouseDat[HouseIndexToType[P[0]]].BuildArea;
                         for i:=1 to 4 do for k:=1 to 4 do
                           if HA[i,k]<>0 then
                             if InRange(P[1]+1+k-3, 1, fMapX) and InRange(P[2]+1+i-4, 1, fMapY) then
                               SetOwner(P[1]+1+k-3, P[2]+1+i-4);
                       end;
    ct_SetMapColor:    if InRange(fLastPlayer, 1, MAX_PLAYERS) then
                         fPlayerPreview[fLastPlayer].Color := fResource.Palettes.DefDal.Color32(P[0]);
    ct_CenterScreen:   fPlayerPreview[fLastPlayer].StartingLoc := KMPoint(P[0]+1,P[1]+1);
    ct_SetRoad,
    ct_SetField,
    ct_SetWinefield:   SetOwner(P[0]+1, P[1]+1);
    ct_SetUnit:        if not (UnitOldIndexToType[P[0]] in [ANIMAL_MIN..ANIMAL_MAX]) then //Skip animals
                       begin
                         SetOwner(P[1]+1, P[2]+1);
                         RevealCircle(P[1]+1, P[2]+1, fResource.UnitDat.UnitsDat[UnitOldIndexToType[P[0]]].Sight);
                       end;
    ct_SetStock:       begin
                         //Set Store and roads below
                         ProcessCommand(ct_SetHouse,[11,P[0]+1,P[1]+1]);
                         ProcessCommand(ct_SetRoad, [   P[0]-2,P[1]+1]);
                         ProcessCommand(ct_SetRoad, [   P[0]-1,P[1]+1]);
                         ProcessCommand(ct_SetRoad, [   P[0]  ,P[1]+1]);
                       end;
    ct_SetGroup:       if InRange(P[0], Low(UnitIndexToType), High(UnitIndexToType)) and (UnitIndexToType[P[0]] <> ut_None) then
                         for I := 0 to P[5] - 1 do
                         begin
                           Loc := GetPositionInGroup2(P[1]+1,P[2]+1,TKMDirection(P[3]+1), I, P[4],fMapX,fMapY,Valid);
                           if Valid then
                           begin
                             SetOwner(Loc.X,Loc.Y);
                             RevealCircle(P[1]+1, P[2]+1, fResource.UnitDat.UnitsDat[UnitOldIndexToType[P[0]]].Sight);
                           end;
                         end;
    ct_ClearUp:        if (fHumanPlayer <> 0) and (fHumanPlayer = fLastPlayer) then
                       begin
                         if P[0] = 255 then
                           for i:=1 to MAX_MAP_SIZE*MAX_MAP_SIZE do
                             fMapPreview[i].Revealed := True
                         else
                           RevealCircle(P[0]+1,P[1]+1,P[2]);
                       end;
  end;
end;


//We use custom mission loader for speed (compare only used commands)
function TMissionParserPreview.LoadMission(const aFileName: string): Boolean;
const
  MAX_CMD = 6;
var
  FileText: AnsiString;
  CommandText, Param: AnsiString;
  ParamList: array[1..MAX_CMD] of Integer;
  k, l, IntParam: Integer;
  CommandType: TKMCommandType;
begin
  inherited LoadMission(aFileName);

  fLastPlayer := 0;
  fHumanPlayer := 0;
  FillChar(fMapPreview, SizeOf(fMapPreview), #0);
  FillChar(fPlayerPreview, SizeOf(fPlayerPreview), #0);

  LoadMapData(ChangeFileExt(fMissionFileName,'.map'));
  Result := false;

  FileText := ReadMissionFile(aFileName);
  if FileText = '' then Exit;

  //FileText should now be formatted nicely with 1 space between each parameter/command
  k := 1;
  repeat
    if FileText[k]='!' then
    begin
      for l:=1 to MAX_CMD do
        ParamList[l]:=-1;
      CommandText:='';
      //Extract command until a space
      repeat
        CommandText:=CommandText+FileText[k];
        inc(k);
      until((FileText[k]=#32)or(k>=length(FileText)));

      //Try to make it faster by only processing commands used
      if (CommandText='!SET_CURR_PLAYER')or(CommandText='!SET_HUMAN_PLAYER')or
         (CommandText='!SET_MAP_COLOR')or(CommandText='!CENTER_SCREEN')or
         (CommandText='!SET_STREET')or(CommandText='!SET_FIELD')or
         (CommandText='!SET_WINEFIELD')or(CommandText='!SET_STOCK')or
         (CommandText='!SET_HOUSE')or(CommandText='!CLEAR_UP')or
         (CommandText='!SET_UNIT')or(CommandText='!SET_GROUP') then
      begin
        //Now convert command into type
        CommandType := TextToCommandType(CommandText);
        inc(k);
        //Extract parameters
        for l:=1 to MAX_CMD do
          if (k<length(FileText)) and (FileText[k]<>'!') then
          begin
            Param := '';
            repeat
              Param := Param + FileText[k];
              inc(k);
            until((k >= Length(FileText)) or (FileText[k]='!') or (FileText[k]=#32)); //Until we find another ! OR we run out of data

            //Convert to an integer, if possible
            if TryStrToInt(String(Param), IntParam) then
              ParamList[l] := IntParam;

            if FileText[k]=#32 then inc(k);
          end;
        //We now have command text and parameters, so process them
        ProcessCommand(CommandType,ParamList);
      end;
    end
    else
      inc(k);
  until (k>=length(FileText));
  //Apparently it's faster to parse till file end than check if all details are filled

  Result := (fFatalErrors = '');
end;


end.
