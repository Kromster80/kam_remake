unit KM_MissionScript;
{$I KaM_Remake.inc}
interface
uses
  {$IFDEF MSWindows} Windows, {$ENDIF}
  Classes, KromUtils, SysUtils, Dialogs, Math,
  KM_Utils, KM_CommonClasses, KM_Defaults, KM_Points,
  KM_AIAttacks, KM_Houses, KM_Units, KM_Terrain, KM_UnitGroups, KM_Units_Warrior;


type
  TMissionParsingMode = (
                          mpm_Single,
                          mpm_Multi,  //Skip players
                          mpm_Editor  //Ignore errors, load armies differently
                        );

  TKMCommandType = (ct_Unknown=0,ct_SetMap,ct_SetMaxPlayer,ct_SetCurrPlayer,ct_SetHumanPlayer,ct_SetHouse,
                    ct_SetTactic,ct_AIPlayer,ct_EnablePlayer,ct_SetNewRemap,ct_SetMapColor,ct_CenterScreen,
                    ct_ClearUp,ct_BlockTrade,ct_BlockHouse,ct_ReleaseHouse,ct_ReleaseAllHouses,ct_AddGoal,ct_AddLostGoal,
                    ct_SetUnit,ct_SetRoad,ct_SetField,ct_SetWinefield,ct_SetStock,ct_AddWare,ct_SetAlliance,
                    ct_SetHouseDamage,ct_SetUnitByStock,ct_SetGroup,ct_SetGroupFood,ct_SendGroup,
                    ct_AttackPosition,ct_AddWareToSecond,ct_AddWareTo,ct_AddWareToAll,ct_AddWeapon,ct_AICharacter,
                    ct_AINoBuild,ct_AIStartPosition,ct_AIDefence,ct_AIAttack,ct_CopyAIAttack);

  TKMCommandParamType = (cpt_Unknown=0,cpt_Recruits,cpt_Constructors,cpt_WorkerFactor,cpt_RecruitCount,cpt_TownDefence,
                         cpt_MaxSoldier,cpt_EquipRate,cpt_AttackFactor,cpt_TroopParam);

  TAIAttackParamType = (cpt_Type, cpt_TotalAmount, cpt_Counter, cpt_Range, cpt_TroopAmount, cpt_Target, cpt_Position, cpt_TakeAll);


type
  TKMMissionInfo = record
    MapPath: string;
    MapSizeX, MapSizeY: Integer;
    MissionMode: TKMissionMode;
    PlayerCount: ShortInt;
    HumanPlayerID: TPlayerIndex;
    VictoryCond: string;
    DefeatCond: string;
  end;

  TKMAttackPosition = record
    Group: TKMUnitGroup;
    Target: TKMPoint;
  end;


  TMissionParserCommon = class
  protected
    fStrictParsing: Boolean; //Report non-fatal script errors such as SEND_GROUP without defining a group first
    fMissionFileName: string;
    fFatalErrors: string; //Fatal errors descriptions accumulate here
    fMinorErrors: string; //Minor error descriptions accumulate here
    fMissionInfo: TKMMissionInfo;
    function TextToCommandType(const ACommandText: AnsiString): TKMCommandType;
    function ReadMissionFile(const aFileName: string): AnsiString;
    procedure AddError(const ErrorMsg: string; aFatal: Boolean = False);
  public
    constructor Create(aStrictParsing: Boolean);
    property FatalErrors: string read fFatalErrors;
    property MinorErrors: string read fMinorErrors;
    property MissionInfo: TKMMissionInfo read fMissionInfo;
    function LoadMission(const aFileName: string): Boolean; overload; virtual;
  end;


  TMissionParserInfo = class(TMissionParserCommon)
  private
    function LoadMapInfo(const aFileName: string): Boolean;
    procedure ProcessCommand(CommandType: TKMCommandType; const P: array of integer; TextParam:AnsiString);
  public
    function LoadMission(const aFileName: string): Boolean; override;
  end;


  TMissionParserStandard = class(TMissionParserCommon)
  private
    fParsingMode: TMissionParsingMode; //Data gets sent to Game differently depending on Game/Editor mode
    fPlayerEnabled: TPlayerEnabledArray;
    fLastPlayer: TPlayerIndex;
    fLastHouse: TKMHouse;
    fLastTroop: TKMUnitGroup;
    fAIAttack: TAIAttack;
    fAttackPositions: array of TKMAttackPosition;
    fAttackPositionsCount: Integer;

    function ProcessCommand(CommandType: TKMCommandType; P: array of Integer; TextParam: AnsiString): Boolean;
    procedure ProcessAttackPositions;
  public
    constructor Create(aMode: TMissionParsingMode; aStrictParsing: Boolean); overload;
    constructor Create(aMode: TMissionParsingMode; aPlayersEnabled: TPlayerEnabledArray; aStrictParsing: Boolean); overload;
    function LoadMission(const aFileName: string): Boolean; overload; override;

    procedure SaveDATFile(const aFileName: string);
  end;


implementation
uses KM_PlayersCollection, KM_Player, KM_AI, KM_AIDefensePos,
  KM_Resource, KM_ResourceHouse, KM_ResourceUnit, KM_ResourceResource, KM_Game;


const
  COMMANDVALUES: array [TKMCommandType] of AnsiString = (
    '','SET_MAP','SET_MAX_PLAYER','SET_CURR_PLAYER','SET_HUMAN_PLAYER','SET_HOUSE',
    'SET_TACTIC','SET_AI_PLAYER','ENABLE_PLAYER','SET_NEW_REMAP','SET_MAP_COLOR',
    'CENTER_SCREEN','CLEAR_UP','BLOCK_TRADE','BLOCK_HOUSE','RELEASE_HOUSE','RELEASE_ALL_HOUSES',
    'ADD_GOAL','ADD_LOST_GOAL','SET_UNIT','SET_STREET','SET_FIELD','SET_WINEFIELD',
    'SET_STOCK','ADD_WARE','SET_ALLIANCE','SET_HOUSE_DAMAGE','SET_UNIT_BY_STOCK',
    'SET_GROUP','SET_GROUP_FOOD','SEND_GROUP','ATTACK_POSITION','ADD_WARE_TO_SECOND',
    'ADD_WARE_TO','ADD_WARE_TO_ALL','ADD_WEAPON','SET_AI_CHARACTER',
    'SET_AI_NO_BUILD','SET_AI_START_POSITION','SET_AI_DEFENSE','SET_AI_ATTACK',
    'COPY_AI_ATTACK');

  PARAMVALUES: array [TKMCommandParamType] of AnsiString = (
    '','RECRUTS','CONSTRUCTORS','WORKER_FACTOR','RECRUT_COUNT','TOWN_DEFENSE',
    'MAX_SOLDIER','EQUIP_RATE','ATTACK_FACTOR','TROUP_PARAM');

  AI_ATTACK_PARAMS: array [TAIAttackParamType] of AnsiString = (
    'TYPE', 'TOTAL_AMOUNT', 'COUNTER', 'RANGE', 'TROUP_AMOUNT', 'TARGET', 'POSITION', 'TAKEALL');

  MAX_PARAMS = 8;

{ TMissionParserGeneric }
constructor TMissionParserCommon.Create(aStrictParsing: boolean);
begin
  inherited Create;
  fStrictParsing := aStrictParsing;
end;


function TMissionParserCommon.LoadMission(const aFileName: string):boolean;
begin
  fMissionFileName := aFileName;

  //Set default values
  fMissionInfo.MapPath := '';
  fMissionInfo.MapSizeX := 0;
  fMissionInfo.MapSizeY := 0;
  fMissionInfo.MissionMode := mm_Normal;
  fMissionInfo.PlayerCount := 0;
  fMissionInfo.HumanPlayerID := PLAYER_NONE;
  fMissionInfo.VictoryCond := '';
  fMissionInfo.DefeatCond := '';

  Result := true;
end;


function TMissionParserCommon.TextToCommandType(const ACommandText: AnsiString): TKMCommandType;
var
  i: TKMCommandType;
begin
  Result := ct_Unknown;
  for i:=low(TKMCommandType) to high(TKMCommandType) do
  begin
    if ACommandText = '!' + COMMANDVALUES[i] then
    begin
      Result := i;
      break;
    end;
  end;
  //Commented out because it slows down mission scanning
  //if Result = ct_Unknown then fLog.AddToLog(String(ACommandText));
end;


//Read mission file to a string and if necessary - decode it
function TMissionParserCommon.ReadMissionFile(const aFileName: string): AnsiString;
var
  I,Num: Cardinal;
  F: TMemoryStream;
begin
  if not FileExists(aFileName) then
  begin
    AddError(Format('Mission file %s could not be found', [aFileName]), True);
    Result := '';
    Exit;
  end;

  //Load and decode .DAT file into FileText
  F := TMemoryStream.Create;
  try
    F.LoadFromFile(aFileName);

    if F.Size = 0 then
    begin
      AddError(Format('Mission file %s is empty', [aFileName]), True);
      Result := '';
      Exit;
    end;

    //Detect whether mission is encoded so we can support decoded/encoded .DAT files
    //We can't test 1st char, it can be any. Instead see how often common chracters meet
    Num := 0;
    for I:=0 to F.Size-1 do               //tab, eol, 0..9, space, !
      if PByte(Cardinal(F.Memory)+I)^ in [9,10,13,ord('0')..ord('9'),$20,$21] then
        inc(Num);

    //Usually 30-50% is numerals/spaces, tested on typical KaM maps, take half of that as margin
    if (Num/F.Size < 0.20) then
    for I := 0 to F.Size - 1 do
      PByte(Cardinal(F.Memory)+I)^ := PByte(Cardinal(F.Memory)+I)^ xor 239;

    //Save text after decoding but before cleaning
    if WRITE_DECODED_MISSION then
      F.SaveToFile(aFileName+'.txt');

    for I := 0 to F.Size - 1 do
      if PByte(Cardinal(F.Memory)+I)^ in [9, 10, 13] then //tab, eol
        PByte(Cardinal(F.Memory)+I)^ := $20; //Space

    Num := 0;
    for I := 0 to F.Size - 1 do
    begin
      PByte(Cardinal(F.Memory)+Num)^ := PByte(Cardinal(F.Memory)+I)^;
      if (Num <= 0) or (
        (PWord(Cardinal(F.Memory)+Num-1)^ <> $2020) //Skip double spaces and !!
        and (PWord(Cardinal(F.Memory)+Num-1)^ <> $2121)) then
        inc(Num);
    end;

    SetLength(Result, Num); //Because some extra characters were removed
    F.Position := 0;
    F.ReadBuffer(Result[1], Num);
  finally
    F.Free;
  end;
end;


//A nice way of debugging script errors.
//Shows the error to the user so they know exactly what they did wrong.
procedure TMissionParserCommon.AddError(const ErrorMsg: string; aFatal: Boolean = False);
begin
  if fStrictParsing or aFatal then
    fFatalErrors := fFatalErrors + ErrorMsg + '|';

  if not aFatal then
    fMinorErrors := fMinorErrors + ErrorMsg + '|';
end;


{ TMissionParserInfo }
function TMissionParserInfo.LoadMission(const aFileName: string):boolean;
const
  Max_Cmd=2;
var
  FileText: AnsiString;
  CommandText, Param, TextParam: AnsiString;
  ParamList: array[1..Max_Cmd] of Integer;
  k, l, IntParam: integer;
  CommandType: TKMCommandType;
begin
  inherited LoadMission(aFileName);

  Result := false;

  FileText := ReadMissionFile(aFileName);
  if FileText = '' then Exit;

  //We need only these 6 commands
  //!SET_MAP, !SET_MAX_PLAYER, !SET_TACTIC, !SET_HUMAN_PLAYER, !ADD_GOAL, !ADD_LOST_GOAL

  //FileText should now be formatted nicely with 1 space between each parameter/command
  k := 1;
  repeat
    if FileText[k]='!' then
    begin
      for l:=1 to Max_Cmd do
        ParamList[l]:=-1;
      TextParam:='';
      CommandText:='';
      //Extract command until a space
      repeat
        CommandText:=CommandText+FileText[k];
        inc(k);
      until((FileText[k]=#32)or(k>=length(FileText)));

      //Try to make it faster by only processing commands used
      if (CommandText='!SET_MAP')or(CommandText='!SET_MAX_PLAYER')or
         (CommandText='!SET_TACTIC')or(CommandText='!SET_HUMAN_PLAYER')or
         (CommandText='!ADD_GOAL')or(CommandText='!ADD_LOST_GOAL') then
      begin
        //Now convert command into type
        CommandType := TextToCommandType(CommandText);
        inc(k);
        //Extract parameters
        for l:=1 to Max_Cmd do
          if (k<length(FileText)) and (FileText[k]<>'!') then
          begin
            Param := '';
            repeat
              Param := Param + FileText[k];
              inc(k);
            until((k >= Length(FileText)) or (FileText[k]='!') or (FileText[k]=#32)); //Until we find another ! OR we run out of data

            //Convert to an integer, if possible
            if TryStrToInt(String(Param), IntParam) then
              ParamList[l] := IntParam
            else
              if l = 1 then
                TextParam := Param; //Accept text for first parameter

            if FileText[k]=#32 then inc(k);
          end;
        //We now have command text and parameters, so process them
        ProcessCommand(CommandType,ParamList,TextParam);
      end;
    end
    else
      inc(k);
  until (k>=length(FileText));
  //Apparently it's faster to parse till file end than check if all details are filled

  Result := LoadMapInfo(ChangeFileExt(fMissionFileName,'.map')) and (fFatalErrors='');
end;


procedure TMissionParserInfo.ProcessCommand(CommandType: TKMCommandType; const P: array of integer; TextParam:AnsiString);
begin
  with fMissionInfo do
  case CommandType of
    ct_SetMap:         MapPath       := RemoveQuotes(String(TextParam));
    ct_SetMaxPlayer:   PlayerCount   := P[0];
    ct_SetTactic:      MissionMode   := mm_Tactic;
    ct_SetHumanPlayer: HumanPlayerID := P[0];
{                       if TGoalCondition(P[0]) = gc_Time then
                         VictoryCond := VictoryCond + fPlayers[fLastPlayer].AddGoal(glt_Victory,TGoalCondition(P[0]),TGoalStatus(P[1]),P[3],P[2],play_none)
                       else
                         fPlayers[fLastPlayer].AddGoal(glt_Victory,TGoalCondition(P[0]),TGoalStatus(P[1]),0,P[2],TPlayerID(P[3]));
}
    ct_AddGoal:        VictoryCond   := VictoryCond
                                        + GoalConditionStr[TGoalCondition(P[0])] + ' '
                                        + GoalStatusStr[TGoalStatus(P[1])]+', ';
    ct_AddLostGoal:    DefeatCond    := DefeatCond
                                        + GoalConditionStr[TGoalCondition(P[0])] + ' '
                                        + GoalStatusStr[TGoalStatus(P[1])]+', ';
  end;
end;


{Acquire specific map details in a fast way}
function TMissionParserInfo.LoadMapInfo(const aFileName:string):boolean;
var F:TKMemoryStream; sx,sy:integer;
begin
  Result := false;
  if not FileExists(aFileName) then Exit;

  F := TKMemoryStream.Create;
  try
    F.LoadFromFile(aFileName);
    F.Read(sx);
    F.Read(sy);
  finally
    F.Free;
  end;

  if (sx > MAX_MAP_SIZE) or (sy > MAX_MAP_SIZE) then
  begin
    AddError('MissionParser can''t open the map because it''s too big.',true);
    Result := false;
    Exit;
  end;

  fMissionInfo.MapSizeX := sx;
  fMissionInfo.MapSizeY := sy;
  Result := true;
end;


{ TMissionParserStandard }
//Mode affect how certain parameters are loaded a bit differently
constructor TMissionParserStandard.Create(aMode: TMissionParsingMode; aStrictParsing: boolean);
var I: Integer;
begin
  inherited Create(aStrictParsing);
  fParsingMode := aMode;

  for I := 0 to High(fPlayerEnabled) do
    fPlayerEnabled[I] := True;
end;


constructor TMissionParserStandard.Create(aMode: TMissionParsingMode; aPlayersEnabled: TPlayerEnabledArray; aStrictParsing: Boolean);
var I: Integer;
begin
  inherited Create(aStrictParsing);
  fParsingMode := aMode;

  //Tells us which player should be enabled and which ignored/skipped
  fPlayerEnabled := aPlayersEnabled;
end;


function TMissionParserStandard.LoadMission(const aFileName: string): Boolean;
var
  FileText, CommandText, Param, TextParam: AnsiString;
  ParamList: array [1..MAX_PARAMS] of integer;
  k, l, IntParam: integer;
  CommandType: TKMCommandType;
begin
  inherited LoadMission(aFileName);

  Assert((fTerrain <> nil) and (fPlayers <> nil));

  Result := false; //Set it right from the start

  //Reset fPlayers and other stuff
  fLastPlayer := -1;

  //Read the mission file into FileText
  FileText := ReadMissionFile(aFileName);
  if FileText = '' then Exit;

  //FileText should now be formatted nicely with 1 space between each parameter/command
  k := 1;
  repeat
    if FileText[k]='!' then
    begin
      for l:=1 to MAX_PARAMS do
        ParamList[l]:=-1;
      TextParam:='';
      CommandText:='';
      //Extract command until a space
      repeat
        CommandText:=CommandText+FileText[k];
        inc(k);
      until((FileText[k]=#32)or(k>=length(FileText)));
      //Now convert command into type
      CommandType := TextToCommandType(CommandText);
      inc(k);
      //Extract parameters
      for l:=1 to MAX_PARAMS do
        if (k<=length(FileText)) and (FileText[k]<>'!') then
        begin
          Param := '';
          repeat
            Param:=Param+FileText[k];
            inc(k);
          until((k>=length(FileText))or(FileText[k]='!')or(FileText[k]=#32)); //Until we find another ! OR we run out of data

          //Convert to an integer, if possible
          if TryStrToInt(String(Param), IntParam) then
            ParamList[l] := IntParam
          else
            if l = 1 then
              TextParam := Param; //Accept text for first parameter

          if (k<=length(FileText)) and (FileText[k]=#32) then inc(k);
        end;
      //We now have command text and parameters, so process them

      if not ProcessCommand(CommandType, ParamList, TextParam) then //A returned value of false indicates an error has occoured and we should exit
      begin
        Result := false;
        Exit;
      end;
    end
    else
      inc(k);
  until (k>=length(FileText));

  //Post-processing of ct_Attack_Position commands which must be done after mission has been loaded
  ProcessAttackPositions;

  //SinglePlayer needs a player
  if (fMissionInfo.HumanPlayerID = PLAYER_NONE) and (fParsingMode = mpm_Single) then
    if ALLOW_NO_HUMAN_IN_SP then
      fMissionInfo.HumanPlayerID := 0 //We need to choose some player to look at
    else
      AddError('No human player detected - ''ct_SetHumanPlayer''', True);

  //If we have reach here without exiting then loading was successful if no errors were reported
  Result := (fFatalErrors = '');
end;


function TMissionParserStandard.ProcessCommand(CommandType: TKMCommandType; P: array of integer; TextParam: AnsiString):boolean;
var
  MapFileName: string;
  i: integer;
  Qty: integer;
  H: TKMHouse;
  HT: THouseType;
  iPlayerAI: TKMPlayerAI;
begin
  Result := False; //Set it right from the start. There are several Exit points below

  case CommandType of
    ct_SetMap:          begin
                          MapFileName := RemoveQuotes(String(TextParam));
                          //Check for same filename.map in same folder first - Remake format
                          if FileExists(ChangeFileExt(fMissionFileName, '.map')) then
                            fTerrain.LoadFromFile(ChangeFileExt(fMissionFileName, '.map'), fParsingMode = mpm_Editor)
                          else
                          //Check for KaM format map path
                          if FileExists(ExeDir + MapFileName) then
                            fTerrain.LoadFromFile(ExeDir+MapFileName, fParsingMode = mpm_Editor)
                          else
                          begin
                            //Else abort loading and fail
                            AddError('Map file couldn''t be found', True);
                            Exit;
                          end;
                        end;
    ct_SetMaxPlayer:    begin
                          fPlayers.AddPlayers(P[0]);
                        end;
    ct_SetTactic:       begin
                          fMissionInfo.MissionMode := mm_Tactic;
                        end;
    ct_SetCurrPlayer:   if InRange(P[0], 0, MAX_PLAYERS - 1) then
                        begin
                          if fPlayerEnabled[P[0]] then
                            fLastPlayer := P[0]
                          else
                            fLastPlayer := -1; //Lets us skip this player
                          fLastHouse := nil;
                          fLastTroop := nil;
                        end;
    ct_SetHumanPlayer:  if (fParsingMode <> mpm_Multi) and (fPlayers <> nil) then
                          if InRange(P[0], 0, fPlayers.Count-1) then
                          begin
                            fMissionInfo.HumanPlayerID := P[0];
                            fPlayers[P[0]].PlayerType := pt_Human;
                          end;
                        //Multiplayer will set Human player itself after loading
    ct_AIPlayer:        if (fParsingMode <> mpm_Multi) and (fPlayers <> nil) then
                          if InRange(P[0], 0, fPlayers.Count - 1) then
                            fPlayers[P[0]].PlayerType := pt_Computer
                          else //This command doesn't require an ID, just use the current player
                            fPlayers[fLastPlayer].PlayerType := pt_Computer;
                        //Multiplayer will set AI players itself after loading
    ct_CenterScreen:    if fLastPlayer >= 0 then
                          fPlayers[fLastPlayer].CenterScreen := KMPoint(P[0]+1, P[1]+1);
    ct_ClearUp:         if fLastPlayer >= 0 then
                        begin
                          if fParsingMode = mpm_Editor then
                            if P[0] = 255 then
                              fGame.MapEditor.Revealers[fLastPlayer].AddEntry(KMPoint(0,0), 255)
                            else
                              fGame.MapEditor.Revealers[fLastPlayer].AddEntry(KMPoint(P[0]+1,P[1]+1), P[2])
                          else
                            if P[0] = 255 then
                              fPlayers[fLastPlayer].FogOfWar.RevealEverything
                            else
                              fPlayers[fLastPlayer].FogOfWar.RevealCircle(KMPoint(P[0]+1,P[1]+1), P[2], 255);
                        end;
    ct_SetHouse:        if fLastPlayer >= 0 then
                          if InRange(P[0], Low(HouseIndexToType), High(HouseIndexToType)) then
                            if fTerrain.CanPlaceHouseFromScript(HouseIndexToType[P[0]], KMPoint(P[1]+1, P[2]+1)) then
                              fLastHouse := fPlayers[fLastPlayer].AddHouse(
                                HouseIndexToType[P[0]], P[1]+1, P[2]+1, false)
                            else
                              AddError('ct_SetHouse failed, can not place house at ' + TypeToString(KMPoint(P[1]+1, P[2]+1)));
    ct_SetHouseDamage:  if fLastPlayer >= 0 then //Skip false-positives for skipped players
                          if fLastHouse <> nil then
                            fLastHouse.AddDamage(min(P[0],high(word)), fParsingMode = mpm_Editor)
                          else
                            AddError('ct_SetHouseDamage without prior declaration of House');
    ct_SetUnit:         begin
                          //Animals should be added regardless of current player
                          if UnitOldIndexToType[P[0]] in [ANIMAL_MIN..ANIMAL_MAX] then
                            fPlayers.PlayerAnimals.AddUnit(UnitOldIndexToType[P[0]], KMPoint(P[1]+1, P[2]+1))
                          else
                          if (fLastPlayer >= 0) and (UnitOldIndexToType[P[0]] in [HUMANS_MIN..HUMANS_MAX]) then
                            fPlayers[fLastPlayer].AddUnit(UnitOldIndexToType[P[0]], KMPoint(P[1]+1, P[2]+1));
                        end;

    ct_SetUnitByStock:  if fLastPlayer >= 0 then
                          if UnitOldIndexToType[P[0]] in [HUMANS_MIN..HUMANS_MAX] then
                          begin
                            H := fPlayers[fLastPlayer].FindHouse(ht_Store, 1);
                            if H <> nil then
                              fPlayers[fLastPlayer].AddUnit(UnitOldIndexToType[P[0]], KMPoint(H.GetEntrance.X, H.GetEntrance.Y+1));
                          end;
    ct_SetRoad:         if fLastPlayer >= 0 then
                          fPlayers[fLastPlayer].AddRoadToList(KMPoint(P[0]+1,P[1]+1));
    ct_SetField:        if fLastPlayer >= 0 then
                          fPlayers[fLastPlayer].AddField(KMPoint(P[0]+1,P[1]+1),ft_Corn);
    ct_SetWinefield:    if fLastPlayer >= 0 then
                          fPlayers[fLastPlayer].AddField(KMPoint(P[0]+1,P[1]+1),ft_Wine);
    ct_SetStock:        if fLastPlayer >= 0 then
                        begin //This command basically means: Put a SH here with road bellow it
                          fLastHouse := fPlayers[fLastPlayer].AddHouse(ht_Store, P[0]+1,P[1]+1, false);
                          fPlayers[fLastPlayer].AddRoadToList(KMPoint(P[0]+1,P[1]+2));
                          fPlayers[fLastPlayer].AddRoadToList(KMPoint(P[0],P[1]+2));
                          fPlayers[fLastPlayer].AddRoadToList(KMPoint(P[0]-1,P[1]+2));
                        end;
    ct_AddWare:         if fLastPlayer >= 0 then
                        begin
                          Qty := EnsureRange(P[1], -1, High(Word)); //Sometimes user can define it to be 999999
                          if Qty = -1 then Qty := High(Word); //-1 means maximum resources
                          H := fPlayers[fLastPlayer].FindHouse(ht_Store,1);
                          if (H <> nil) and (ResourceKaMIndex[P[0]] in [WARE_MIN..WARE_MAX]) then
                          begin
                            H.ResAddToIn(ResourceKaMIndex[P[0]], Qty, True);
                            fPlayers[fLastPlayer].Stats.GoodInitial(ResourceKaMIndex[P[0]], Qty);
                          end;

                        end;
    ct_AddWareToAll:    begin
                          Qty := EnsureRange(P[1], -1, High(Word)); //Sometimes user can define it to be 999999
                          if Qty = -1 then Qty := High(Word); //-1 means maximum resources
                          for i:=0 to fPlayers.Count-1 do
                          begin
                            H := fPlayers[i].FindHouse(ht_Store,1);
                            if (H<>nil) and (ResourceKaMIndex[P[0]] in [WARE_MIN..WARE_MAX]) then
                            begin
                              H.ResAddToIn(ResourceKaMIndex[P[0]], Qty, True);
                              fPlayers[i].Stats.GoodInitial(ResourceKaMIndex[P[0]], Qty);
                            end;
                          end;
                        end;
    ct_AddWareToSecond: if fLastPlayer >= 0 then
                        begin
                          Qty := EnsureRange(P[1], -1, High(Word)); //Sometimes user can define it to be 999999
                          if Qty = -1 then Qty := High(Word); //-1 means maximum resources

                          H := TKMHouseStore(fPlayers[fLastPlayer].FindHouse(ht_Store, 2));
                          if (H <> nil) and (ResourceKaMIndex[P[0]] in [WARE_MIN..WARE_MAX]) then
                          begin
                            H.ResAddToIn(ResourceKaMIndex[P[0]], Qty, True);
                            fPlayers[fLastPlayer].Stats.GoodInitial(ResourceKaMIndex[P[0]], Qty);
                          end;
                        end;
    ct_AddWareTo:       if fLastPlayer >= 0 then
                        begin //HouseType, House Order, Ware Type, Count
                          Qty := EnsureRange(P[3], -1, High(Word)); //Sometimes user can define it to be 999999
                          if Qty = -1 then Qty := High(Word); //-1 means maximum resources

                          H := fPlayers[fLastPlayer].FindHouse(HouseIndexToType[P[0]], P[1]);
                          if (H <> nil) and (ResourceKaMIndex[P[2]] in [WARE_MIN..WARE_MAX]) then
                          begin
                            H.ResAddToIn(ResourceKaMIndex[P[2]], Qty, True);
                            fPlayers[fLastPlayer].Stats.GoodInitial(ResourceKaMIndex[P[2]], Qty);
                          end;
                        end;
    ct_AddWeapon:       if fLastPlayer >= 0 then
                        begin
                          Qty := EnsureRange(P[1], -1, High(Word)); //Sometimes user can define it to be 999999
                          if Qty = -1 then Qty := High(Word); //-1 means maximum weapons
                          H := TKMHouseBarracks(fPlayers[fLastPlayer].FindHouse(ht_Barracks, 1));
                          if (H <> nil) and (ResourceKaMIndex[P[0]] in [WARFARE_MIN..WARFARE_MAX]) then
                          begin
                            H.ResAddToIn(ResourceKaMIndex[P[0]], Qty, True);
                            fPlayers[fLastPlayer].Stats.GoodInitial(ResourceKaMIndex[P[0]], Qty);
                          end;
                        end;
    ct_BlockTrade:      if fLastPlayer >= 0 then
                        begin
                          if ResourceKaMIndex[P[0]] in [WARE_MIN..WARE_MAX] then
                            fPlayers[fLastPlayer].Stats.AllowToTrade[ResourceKaMIndex[P[0]]] := false;
                        end;
    ct_BlockHouse:      if fLastPlayer >= 0 then
                        begin
                          if InRange(P[0], Low(HouseIndexToType), High(HouseIndexToType)) then
                            fPlayers[fLastPlayer].Stats.HouseBlocked[HouseIndexToType[P[0]]] := True;
                        end;
    ct_ReleaseHouse:    if fLastPlayer >= 0 then
                        begin
                          if InRange(P[0], Low(HouseIndexToType), High(HouseIndexToType)) then
                            fPlayers[fLastPlayer].Stats.HouseGranted[HouseIndexToType[P[0]]] := True;
                        end;
    ct_ReleaseAllHouses:if fLastPlayer >= 0 then
                          for HT:=Low(THouseType) to High(THouseType) do
                            fPlayers[fLastPlayer].Stats.HouseGranted[HT] := True;
    ct_SetGroup:        if fLastPlayer >= 0 then
                          if InRange(P[0], Low(UnitIndexToType), High(UnitIndexToType)) and (UnitIndexToType[P[0]] <> ut_None) then
                            fLastTroop := fPlayers[fLastPlayer].AddUnitGroup(
                              UnitIndexToType[P[0]],
                              KMPoint(P[1]+1, P[2]+1),
                              TKMDirection(P[3]+1),
                              P[4],
                              P[5]
                              );
    ct_SendGroup:       if fLastPlayer >= 0 then
                        begin
                          if fLastTroop <> nil then
                            if fParsingMode = mpm_Editor then
                            begin
                              fLastTroop.MapEdOrder.Order := goWalkTo;
                              fLastTroop.MapEdOrder.Pos := KMPointDir(P[0]+1, P[1]+1, TKMDirection(P[2]+1));
                            end
                            else
                              fLastTroop.OrderWalk(KMPoint(P[0]+1, P[1]+1), TKMDirection(P[2]+1))
                          else
                            AddError('ct_SendGroup without prior declaration of Troop');
                        end;
    ct_SetGroupFood:    if fLastPlayer >= 0 then
                        begin
                          if fLastTroop <> nil then
                            fLastTroop.Condition := UNIT_MAX_CONDITION
                          else
                            AddError('ct_SetGroupFood without prior declaration of Troop');
                        end;
    ct_AICharacter:     if fLastPlayer >= 0 then
                        begin
                          if fPlayers[fLastPlayer].PlayerType <> pt_Computer then Exit;
                          iPlayerAI := fPlayers[fLastPlayer].AI; //Setup the AI's character
                          if TextParam = PARAMVALUES[cpt_Recruits]     then iPlayerAI.Setup.RecruitFactor := P[1];
                          if TextParam = PARAMVALUES[cpt_Constructors] then iPlayerAI.Setup.WorkerFactor  := P[1];
                          if TextParam = PARAMVALUES[cpt_WorkerFactor] then iPlayerAI.Setup.SerfFactor    := P[1];
                          if TextParam = PARAMVALUES[cpt_RecruitCount] then iPlayerAI.Setup.RecruitDelay  := P[1];
                          if TextParam = PARAMVALUES[cpt_TownDefence]  then iPlayerAI.Setup.TownDefence   := P[1];
                          if TextParam = PARAMVALUES[cpt_MaxSoldier]   then iPlayerAI.Setup.MaxSoldiers   := P[1];
                          if TextParam = PARAMVALUES[cpt_EquipRate]    then
                          begin
                            iPlayerAI.Setup.EquipRateLeather := P[1];
                            iPlayerAI.Setup.EquipRateIron    := P[1]; //Both the same for now, could be separate commands later
                          end;
                          if TextParam = PARAMVALUES[cpt_AttackFactor] then iPlayerAI.Setup.Aggressiveness:= P[1];
                          if TextParam = PARAMVALUES[cpt_TroopParam]   then
                          begin
                            iPlayerAI.DefencePositions.TroopFormations[TGroupType(P[1])].NumUnits := P[2];
                            iPlayerAI.DefencePositions.TroopFormations[TGroupType(P[1])].UnitsPerRow  := P[3];
                          end;
                        end;
    ct_AINoBuild:       if fLastPlayer >= 0 then
                          fPlayers[fLastPlayer].AI.Setup.Autobuild := False;
    ct_AIStartPosition: if fLastPlayer >= 0 then
                          fPlayers[fLastPlayer].AI.Setup.StartPosition := KMPoint(P[0]+1,P[1]+1);
    ct_SetAlliance:     if (fLastPlayer >= 0) and fPlayerEnabled[P[0]] then
                          if P[1] = 1 then
                            fPlayers[fLastPlayer].Alliances[P[0]] := at_Ally
                          else
                            fPlayers[fLastPlayer].Alliances[P[0]] := at_Enemy;
    ct_AttackPosition:  if fLastPlayer >= 0 then
                          //If target is building: Attack building
                          //If target is unit: Chase/attack unit
                          //If target is nothing: move to position
                          //However, because the unit/house target may not have been created yet, this must be processed after everything else
                          if fLastTroop <> nil then
                          if fParsingMode = mpm_Editor then
                          begin
                            fLastTroop.MapEdOrder.Order := goAttackUnit;
                            fLastTroop.MapEdOrder.Pos := KMPointDir(P[0]+1, P[1]+1, dir_NA);
                          end
                          else
                          begin
                            Inc(fAttackPositionsCount);
                            SetLength(fAttackPositions, fAttackPositionsCount+1);
                            fAttackPositions[fAttackPositionsCount-1].Group := fLastTroop;
                            fAttackPositions[fAttackPositionsCount-1].Target := KMPoint(P[0]+1,P[1]+1);
                          end
                          else
                            AddError('ct_AttackPosition without prior declaration of Troop');
    ct_AddGoal:         if fLastPlayer >= 0 then
                          //If the condition is time then P[3] is the time, else it is player ID
                          if TGoalCondition(P[0]) = gc_Time then
                            fPlayers[fLastPlayer].Goals.AddGoal(glt_Victory,TGoalCondition(P[0]),TGoalStatus(P[1]),P[3],P[2],-1)
                          else
                            if InRange(P[3], 0, fPlayers.Count - 1) then
                              if fPlayerEnabled[P[3]] then
                                fPlayers[fLastPlayer].Goals.AddGoal(glt_Victory,TGoalCondition(P[0]),TGoalStatus(P[1]),0,P[2],P[3])
                              else
                            else
                              AddError('Add_Goal for non existing player');
    ct_AddLostGoal:     if fLastPlayer >= 0 then
                          //If the condition is time then P[3] is the time, else it is player ID
                          if TGoalCondition(P[0]) = gc_Time then
                            fPlayers[fLastPlayer].Goals.AddGoal(glt_Survive,TGoalCondition(P[0]),TGoalStatus(P[1]),P[3],P[2],-1)
                          else
                            if InRange(P[3], 0, fPlayers.Count - 1) then
                              if fPlayerEnabled[P[3]] then
                                fPlayers[fLastPlayer].Goals.AddGoal(glt_Survive,TGoalCondition(P[0]),TGoalStatus(P[1]),0,P[2],P[3])
                              else
                            else
                              AddError('Add_LostGoal for non existing player');
    ct_AIDefence:       if fLastPlayer >=0 then
                        if InRange(P[3], Integer(Low(TGroupType)), Integer(High(TGroupType))) then //TPR 3 tries to set TGroupType 240 due to a missing space
                          fPlayers[fLastPlayer].AI.DefencePositions.AddDefencePosition(KMPointDir(P[0]+1, P[1]+1, TKMDirection(P[2]+1)),TGroupType(P[3]),P[4],TAIDefencePosType(P[5]));
    ct_SetMapColor:     if fLastPlayer >=0 then
                          //For now simply use the minimap color for all color, it is too hard to load all 8 shades from ct_SetNewRemap
                          fPlayers[fLastPlayer].FlagColor := fResource.Palettes.DefDal.Color32(P[0]);
    ct_AIAttack:        begin
                          //Set up the attack command
                          if TextParam = AI_ATTACK_PARAMS[cpt_Type] then
                            if InRange(P[1], Low(RemakeAttackType), High(RemakeAttackType)) then
                              fAIAttack.AttackType := RemakeAttackType[P[1]]
                            else
                              AddError('Unknown parameter ' + IntToStr(P[1]) + ' at ct_AIAttack');
                          if TextParam = AI_ATTACK_PARAMS[cpt_TotalAmount] then
                            fAIAttack.TotalMen := P[1];
                          if TextParam = AI_ATTACK_PARAMS[cpt_Counter] then
                            fAIAttack.Delay := P[1];
                          if TextParam = AI_ATTACK_PARAMS[cpt_Range] then
                            fAIAttack.Range := P[1];
                          if TextParam = AI_ATTACK_PARAMS[cpt_TroopAmount] then
                            fAIAttack.GroupAmounts[TGroupType(P[1])] := P[2];
                          if TextParam = AI_ATTACK_PARAMS[cpt_Target] then
                            fAIAttack.Target := TAIAttackTarget(P[1]);
                          if TextParam = AI_ATTACK_PARAMS[cpt_Position] then
                            fAIAttack.CustomPosition := KMPoint(P[1]+1,P[2]+1);
                          if TextParam = AI_ATTACK_PARAMS[cpt_TakeAll] then
                            fAIAttack.TakeAll := True;
                        end;
    ct_CopyAIAttack:    if fLastPlayer >= 0 then
                          //Save the attack to the AI assets
                          fPlayers[fLastPlayer].AI.Attacks.AddAttack(fAIAttack);
    ct_EnablePlayer:    begin
                          //Serves no real purpose, all players have this command anyway
                        end;
    ct_SetNewRemap:     begin
                          //Disused. Minimap color is used for all colors now. However it might be better to use these values in the long run as sometimes the minimap colors do not match well
                        end;
  end;
  Result := true; //Must have worked if we haven't exited by now
end;


//Determine what we are attacking: House, Unit or just walking to some place
procedure TMissionParserStandard.ProcessAttackPositions;
var
  I: Integer;
  H: TKMHouse;
  U: TKMUnit;
begin
  Assert((fParsingMode <> mpm_Editor) or (fAttackPositionsCount = 0), 'AttackPositions should be handled by MapEd');

  for I := 0 to fAttackPositionsCount - 1 do
    with fAttackPositions[I] do
    begin
      H := fPlayers.HousesHitTest(Target.X, Target.Y); //Attack house
      if (H <> nil) and (not H.IsDestroyed) and (fPlayers.CheckAlliance(Group.Owner, H.Owner) = at_Enemy) then
        Group.OrderAttackHouse(H)
      else
      begin
        U := fTerrain.UnitsHitTest(Target.X, Target.Y); //Chase/attack unit
        if (U <> nil) and (not U.IsDeadOrDying) and (fPlayers.CheckAlliance(Group.Owner, U.Owner) = at_Enemy) then
          Group.OrderAttackUnit(U)
        else
          Group.OrderWalk(Target); //Just move to position
      end;
    end;
end;


//Write out a KaM format mission file to aFileName
procedure TMissionParserStandard.SaveDATFile(const aFileName: string);
const
  COMMANDLAYERS = 4;
var
  f: textfile;
  I: longint; //longint because it is used for encoding entire output, which will limit the file size
  K,iX,iY,CommandLayerCount: Integer;
  HouseCount: array[THouseType] of Integer;
  Res: TResourceType;
  G: TGroupType;
  U: TKMUnit;
  H: TKMHouse;
  Group: TKMUnitGroup;
  HT: THouseType;
  ReleaseAllHouses: Boolean;
  SaveString: AnsiString;

  procedure AddData(aText: AnsiString);
  begin
    if CommandLayerCount = -1 then //No layering
      SaveString := SaveString + aText + eol //Add to the string normally
    else
    begin
      case (CommandLayerCount mod COMMANDLAYERS) of
        0:   SaveString := SaveString + eol + aText //Put a line break every 4 commands
        else SaveString := SaveString + ' ' + aText; //Just put spaces so commands "layer"
      end;
      inc(CommandLayerCount);
    end
  end;

  procedure AddCommand(aCommand: TKMCommandType; aComParam: TKMCommandParamType; aParams: array of integer); overload;
  var OutData: AnsiString; I:integer;
  begin
    OutData := '!' + COMMANDVALUES[aCommand];

    if aComParam <> cpt_Unknown then
      OutData := OutData + ' ' + PARAMVALUES[aComParam];

    for I:=Low(aParams) to High(aParams) do
      OutData := OutData + ' ' + AnsiString(IntToStr(aParams[I]));

    AddData(OutData);
  end;

  procedure AddCommand(aCommand: TKMCommandType; aComParam: TAIAttackParamType; aParams: array of integer); overload;
  var OutData: AnsiString; I:integer;
  begin
    OutData := '!' + COMMANDVALUES[aCommand] + ' ' + AI_ATTACK_PARAMS[aComParam];

    for I:=Low(aParams) to High(aParams) do
      OutData := OutData + ' ' + AnsiString(IntToStr(aParams[I]));

    AddData(OutData);
  end;

  procedure AddCommand(aCommand:TKMCommandType; aParams: array of integer); overload;
  begin
    AddCommand(aCommand, cpt_Unknown, aParams);
  end;

begin

  //Put data into stream
  SaveString := '';
  CommandLayerCount := -1; //Some commands (road/fields) are layered so the file is easier to read (not so many lines)

  //Main header, use same filename for MAP
  AddData('!'+COMMANDVALUES[ct_SetMap] + ' "data\mission\smaps\' + AnsiString(ExtractFileName(TruncateExt(aFileName))) + '.map"');
  if fGame.MissionMode = mm_Tactic then AddCommand(ct_SetTactic, []);
  AddCommand(ct_SetMaxPlayer, [fPlayers.Count]);
  AddData(''); //NL

  //Player loop
  for I:=0 to fPlayers.Count-1 do
  begin
    //Player header, using same order of commands as KaM
    AddCommand(ct_SetCurrPlayer, [I]); //In script player 0 is the first
    if fPlayers[I].PlayerType = pt_Human then
      AddCommand(ct_SetHumanPlayer, [I]);
    AddCommand(ct_EnablePlayer, [I]);
    if fPlayers[I].PlayerType = pt_Computer then
      AddCommand(ct_AIPlayer, []);

    AddCommand(ct_SetMapColor, [fPlayers[I].FlagColorIndex]);
    if not KMSamePoint(fPlayers[I].CenterScreen, KMPoint(0,0)) then
      AddCommand(ct_CenterScreen, [fPlayers[I].CenterScreen.X-1,fPlayers[I].CenterScreen.Y-1]);

    with fGame.MapEditor.Revealers[I] do
    for K := 0 to Count - 1 do
      if (Items[K].X = 0) and (Items[K].Y = 0) and (Tag[K] = 255) then
        AddCommand(ct_ClearUp, [255])
      else
        AddCommand(ct_ClearUp, [Items[K].X-1, Items[K].Y-1, Tag[K]]);

    AddData(''); //NL

    //Human specific, e.g. goals, center screen (though all players can have it, only human can use it)
    for K:=0 to fPlayers[I].Goals.Count-1 do
      with fPlayers[I].Goals[K] do
      begin
        if (GoalType = glt_Victory) or (GoalType = glt_None) then //For now treat none same as normal goal, we can add new command for it later
          if GoalCondition = gc_Time then
            AddCommand(ct_AddGoal, [byte(GoalCondition),byte(GoalStatus),MessageToShow,GoalTime])
          else
            AddCommand(ct_AddGoal, [byte(GoalCondition),byte(GoalStatus),MessageToShow,PlayerIndex]);

        if GoalType = glt_Survive then
          if GoalCondition = gc_Time then
            AddCommand(ct_AddLostGoal, [byte(GoalCondition),byte(GoalStatus),MessageToShow,GoalTime])
          else
            AddCommand(ct_AddLostGoal, [byte(GoalCondition),byte(GoalStatus),MessageToShow,PlayerIndex]);
      end;
    AddData(''); //NL

    //Computer specific, e.g. AI commands
    if fPlayers[I].PlayerType = pt_Computer then
    begin
      AddCommand(ct_AIStartPosition, [fPlayers[I].AI.Setup.StartPosition.X-1,fPlayers[I].AI.Setup.StartPosition.Y-1]);
      if not fPlayers[I].AI.Setup.AutoBuild then
        AddCommand(ct_AINoBuild, []);
      AddCommand(ct_AICharacter,cpt_Recruits, [fPlayers[I].AI.Setup.RecruitFactor]);
      AddCommand(ct_AICharacter,cpt_WorkerFactor, [fPlayers[I].AI.Setup.SerfFactor]);
      AddCommand(ct_AICharacter,cpt_Constructors, [fPlayers[I].AI.Setup.WorkerFactor]);
      AddCommand(ct_AICharacter,cpt_TownDefence, [fPlayers[I].AI.Setup.TownDefence]);
      //Only store if a limit is in place (high is the default)
      if fPlayers[I].AI.Setup.MaxSoldiers <> High(fPlayers[I].AI.Setup.MaxSoldiers) then
        AddCommand(ct_AICharacter,cpt_MaxSoldier, [fPlayers[I].AI.Setup.MaxSoldiers]);
      AddCommand(ct_AICharacter,cpt_EquipRate,    [fPlayers[I].AI.Setup.EquipRateLeather]); //Iron and Leather could be made into separate commands later
      AddCommand(ct_AICharacter,cpt_AttackFactor, [fPlayers[I].AI.Setup.Aggressiveness]);
      AddCommand(ct_AICharacter,cpt_RecruitCount, [fPlayers[I].AI.Setup.RecruitDelay]);
      for G:=Low(TGroupType) to High(TGroupType) do
        if fPlayers[I].AI.DefencePositions.TroopFormations[G].NumUnits <> 0 then //Must be valid and used
          AddCommand(ct_AICharacter, cpt_TroopParam, [KaMGroupType[G], fPlayers[I].AI.DefencePositions.TroopFormations[G].NumUnits, fPlayers[I].AI.DefencePositions.TroopFormations[G].UnitsPerRow]);
      AddData(''); //NL
      for K:=0 to fPlayers[I].AI.DefencePositions.Count - 1 do
        with fPlayers[I].AI.DefencePositions[K] do
          AddCommand(ct_AIDefence, [Position.Loc.X-1,Position.Loc.Y-1,byte(Position.Dir)-1,KaMGroupType[GroupType],Radius,byte(DefenceType)]);
      AddData(''); //NL
      AddData(''); //NL
      for K:=0 to fPlayers[I].AI.Attacks.Count - 1 do
        with fPlayers[I].AI.Attacks[K] do
        begin
          AddCommand(ct_AIAttack, cpt_Type, [KaMAttackType[AttackType]]);
          AddCommand(ct_AIAttack, cpt_TotalAmount, [TotalMen]);
          if TakeAll then
            AddCommand(ct_AIAttack, cpt_TakeAll, [])
          else
            for G:=Low(TGroupType) to High(TGroupType) do
              AddCommand(ct_AIAttack, cpt_TroopAmount, [KaMGroupType[G], GroupAmounts[G]]);

          if (Delay > 0) or (AttackType = aat_Once) then //Type once must always have counter because it uses the delay
            AddCommand(ct_AIAttack,cpt_Counter, [Delay]);

          AddCommand(ct_AIAttack,cpt_Target, [Byte(Target)]);
          if Target = att_CustomPosition then
            AddCommand(ct_AIAttack,cpt_Position, [CustomPosition.X-1,CustomPosition.Y-1]);

          if Range > 0 then
            AddCommand(ct_AIAttack,cpt_Range, [Range]);

          AddCommand(ct_CopyAIAttack, [K]); //Store attack with ID number
          AddData(''); //NL
        end;
      AddData(''); //NL
    end;

    //General, e.g. units, roads, houses, etc.
    //Alliances
    for K:=0 to fPlayers.Count-1 do
      if K<>I then
        AddCommand(ct_SetAlliance, [K, byte(fPlayers[I].Alliances[K])]); //0=enemy, 1=ally
    AddData(''); //NL

    //Release/block houses
    ReleaseAllHouses := True;
    for HT := Low(THouseType) to High(THouseType) do
    if fResource.HouseDat[HT].IsValid then //Exclude ht_None / ht_Any
    begin
      if fPlayers[I].Stats.HouseBlocked[HT] then
      begin
        AddCommand(ct_BlockHouse, [HouseTypeToIndex[HT]-1]);
        ReleaseAllHouses := false;
      end
      else
        if fPlayers[I].Stats.HouseGranted[HT] then
          AddCommand(ct_ReleaseHouse, [HouseTypeToIndex[HT]-1])
        else
          ReleaseAllHouses := false;
    end;
    if ReleaseAllHouses then
      AddCommand(ct_ReleaseAllHouses, []);

    //Block trades
    for Res := WARE_MIN to WARE_MAX do
      if not fPlayers[I].Stats.AllowToTrade[Res] then
        AddCommand(ct_BlockTrade, [ResourceKaMOrder[Res]]);

    //Houses
    for K:=0 to fPlayers[I].Houses.Count-1 do
    begin
      H := fPlayers[I].Houses[K];
      if not H.IsDestroyed then
      begin
        AddCommand(ct_SetHouse, [HouseTypeToIndex[H.HouseType]-1, H.GetPosition.X-1, H.GetPosition.Y-1]);
        if H.IsDamaged then
          AddCommand(ct_SetHouseDamage, [H.GetDamage]);
      end;
    end;
    AddData(''); //NL

    //Wares. Check every house to see if it has any wares in it
    FillChar(HouseCount, SizeOf(HouseCount), #0);
    for K := 0 to fPlayers[I].Houses.Count - 1 do
    begin
      H := fPlayers[I].Houses[K];
      Inc(HouseCount[H.HouseType]);

      if H.IsDestroyed then Continue;

      //First two Stores use special KaM commands
      if (H.HouseType = ht_Store) and (HouseCount[ht_Store] <= 2) then
      begin
        for Res := WARE_MIN to WARE_MAX do
          if H.CheckResIn(Res) > 0 then
            case HouseCount[ht_Store] of
              1:  AddCommand(ct_AddWare, [ResourceKaMOrder[Res], H.CheckResIn(Res)]);
              2:  AddCommand(ct_AddWareToSecond, [ResourceKaMOrder[Res], H.CheckResIn(Res)]);
            end;
      end
      else
      //First Barracks uses special KaM command
      if (H.HouseType = ht_Barracks) and (HouseCount[ht_Barracks] <= 1) then
      begin
        for Res := WARFARE_MIN to WARFARE_MAX do
          if H.CheckResIn(Res) > 0 then
            AddCommand(ct_AddWeapon, [ResourceKaMOrder[Res], H.CheckResIn(Res)]); //Ware, Count
      end
      else
        for Res := WARE_MIN to WARE_MAX do
          if H.CheckResIn(Res) > 0 then
            AddCommand(ct_AddWareTo, [HouseTypeToIndex[H.HouseType]-1, HouseCount[H.HouseType], ResourceKaMOrder[Res], H.CheckResIn(Res)]);

    end;
    AddData(''); //NL


    //Roads and fields. We must check EVERY terrain tile
    CommandLayerCount := 0; //Enable command layering
    for iY := 1 to fTerrain.MapY do
      for iX := 1 to fTerrain.MapX do
        if fTerrain.Land[iY,iX].TileOwner = fPlayers[I].PlayerIndex then
        begin
          if fTerrain.Land[iY,iX].TileOverlay = to_Road then
            AddCommand(ct_SetRoad, [iX-1,iY-1]);
          if fTerrain.TileIsCornField(KMPoint(iX,iY)) then
            AddCommand(ct_SetField, [iX-1,iY-1]);
          if fTerrain.TileIsWineField(KMPoint(iX,iY)) then
            AddCommand(ct_SetWinefield, [iX-1,iY-1]);
        end;
    CommandLayerCount := -1; //Disable command layering
    AddData(''); //Extra NL because command layering doesn't put one
    AddData(''); //NL

    //Units
    for K := 0 to fPlayers[I].Units.Count - 1 do
    begin
      U := fPlayers[I].Units[K];
      if not (U is TKMUnitWarrior) then //Groups get saved separately
        AddCommand(ct_SetUnit, [UnitTypeToOldIndex[U.UnitType], U.GetPosition.X-1, U.GetPosition.Y-1]);
    end;

    //Unit groups
    for K := 0 to fPlayers[I].UnitGroups.Count - 1 do
    begin
      Group := fPlayers[I].UnitGroups[K];
      AddCommand(ct_SetGroup, [UnitTypeToIndex[Group.UnitType], Group.Position.X-1, Group.Position.Y-1, Byte(Group.Direction)-1, Group.UnitsPerRow, Group.MapEdCount]);
      if Group.Condition = UNIT_MAX_CONDITION then
        AddCommand(ct_SetGroupFood, []);

      if Group.MapEdOrder.Order = goWalkTo then
        AddCommand(ct_SendGroup, [Group.MapEdOrder.Pos.Loc.X-1, Group.MapEdOrder.Pos.Loc.Y-1, Byte(Group.MapEdOrder.Pos.Dir)-1]);
      if Group.MapEdOrder.Order = goAttackUnit then
        AddCommand(ct_AttackPosition, [Group.MapEdOrder.Pos.Loc.X-1, Group.MapEdOrder.Pos.Loc.Y-1]);
    end;

    AddData(''); //NL
    AddData(''); //NL
  end; //Player loop

  //Main footer

  //Animals, wares to all, etc. go here
  AddData('//Animals');
  for I:=0 to fPlayers.PlayerAnimals.Units.Count-1 do
  begin
    U := fPlayers.PlayerAnimals.Units[I];
    AddCommand(ct_SetUnit, [UnitTypeToOldIndex[U.UnitType], U.GetPosition.X-1, U.GetPosition.Y-1]);
  end;
  AddData(''); //NL

  //Similar footer to one in Lewin's Editor, useful so ppl know what mission was made with.
  AddData('//This mission was made with KaM Remake Map Editor version '+GAME_VERSION+' at '+AnsiString(DateTimeToStr(Now)));

  //Write uncoded file for debug
  assignfile(f, aFileName+'.txt'); rewrite(f);
  write(f, SaveString);
  closefile(f);

  //Encode it
  for I:=1 to Length(SaveString) do
    SaveString[I] := AnsiChar(Byte(SaveString[I]) xor 239);

  //Write it
  assignfile(f, aFileName); rewrite(f);
  write(f, SaveString);
  closefile(f);
end;


end.
