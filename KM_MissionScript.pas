unit KM_MissionScript;
{$I KaM_Remake.inc}
interface
uses
  {$IFDEF MSWindows} Windows, {$ENDIF}
  Classes, KromUtils, SysUtils, Dialogs, Math,
  KM_CommonClasses, KM_Defaults, KM_Points,
  KM_AIAttacks, KM_Houses, KM_Units, KM_Terrain, KM_Units_Warrior;


type
  TMissionParsingMode = (
                          mpm_Info,   //Return basic mission properties
                          mpm_Single,
                          mpm_Multi,  //Skip players
                          mpm_Editor, //Ignore errors, load armies differently
                          mpm_Preview //Skip as much as we can
                        );

  TKMCommandType = (ct_Unknown=0,ct_SetMap,ct_SetMaxPlayer,ct_SetCurrPlayer,ct_SetHumanPlayer,ct_SetHouse,
                    ct_SetTactic,ct_AIPlayer,ct_EnablePlayer,ct_SetNewRemap,ct_SetMapColor,ct_CenterScreen,
                    ct_ClearUp,ct_BlockTrade,ct_BlockHouse,ct_ReleaseHouse,ct_ReleaseAllHouses,ct_AddGoal,ct_AddLostGoal,
                    ct_SetUnit,ct_SetRoad,ct_SetField,ct_Set_Winefield,ct_SetStock,ct_AddWare,ct_SetAlliance,
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
    PlayerCount: shortint;
    HumanPlayerID: TPlayerIndex;
    VictoryCond:string;
    DefeatCond:string;
  end;

  TKMAttackPosition = record
    Warrior: TKMUnitWarrior;
    Target: TKMPoint;
  end;

  TMissionParser = class
  private
    fTerrain: TTerrain;
    fParsingMode: TMissionParsingMode; //Data gets sent to Game differently depending on Game/Editor mode
    fStrictParsing: boolean; //Report non-fatal script errors such as SEND_GROUP without defining a group first
    fRemapCount: byte;
    fRemap: TPlayerArray;
    fErrorMessage: string; //Errors descriptions accumulate here
    fMissionFileName: string;

    fLastPlayer: integer;
    fLastHouse: TKMHouse;
    fLastTroop: TKMUnitWarrior;
    fAIAttack: TAIAttack;
    fAttackPositions: array of TKMAttackPosition;
    fAttackPositionsCount: integer;

    fMissionInfo: TKMMissionInfo;

    function LoadSimple(const aFileName:string):boolean;
    function LoadStandard(const aFileName:string):boolean;
    function LoadMapInfo(const aFileName:string):boolean;

    function TextToCommandType(const ACommandText: AnsiString): TKMCommandType;
    function ProcessCommand(CommandType: TKMCommandType; P: array of integer; TextParam: AnsiString):boolean;
    procedure GetDetailsProcessCommand(CommandType: TKMCommandType; const ParamList: array of integer; TextParam:AnsiString);
    procedure AddScriptError(const ErrorMsg:string; aFatal:boolean=false);
    procedure ProcessAttackPositions;
    function ReadMissionFile(const aFileName:string): AnsiString;
  public
    constructor Create(aMode:TMissionParsingMode; aStrictParsing:boolean); overload;
    constructor Create(aMode:TMissionParsingMode; aPlayersRemap:TPlayerArray; aStrictParsing:boolean); overload;
    function LoadMission(const aFileName: string; aTerrain: TTerrain):boolean;

    property ErrorMessage:string read fErrorMessage;
    property MissionInfo:TKMMissionInfo read fMissionInfo;

    procedure SaveDATFile(const aFileName: String);
  end;


implementation
uses KM_PlayersCollection, KM_Player, KM_PlayerAI, KM_Resource, KM_ResourceHouse, KM_ResourceResource;


const
  COMMANDVALUES: array[TKMCommandType] of AnsiString = (
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

  MAXPARAMS = 8;

  //This is a map of the valid values for !SET_UNIT, and the corresponing unit that will be created (matches KaM behavior)
  UnitsRemap: array[0..31] of TUnitType = (ut_Serf,ut_Woodcutter,ut_Miner,ut_AnimalBreeder,
    ut_Farmer,ut_Lamberjack,ut_Baker,ut_Butcher,ut_Fisher,ut_Worker,ut_StoneCutter,
    ut_Smith,ut_Metallurgist,ut_Recruit, //Units
    ut_Militia,ut_AxeFighter,ut_Swordsman,ut_Bowman,ut_Arbaletman,ut_Pikeman,ut_Hallebardman,
    ut_HorseScout,ut_Cavalry,ut_Barbarian, //Troops
    ut_Wolf,ut_Fish,ut_Watersnake,ut_Seastar,ut_Crab,ut_Waterflower,ut_Waterleaf,ut_Duck); //Animals

  UnitReverseRemap: array[TUnitType] of integer = (
  -1, -1, //ut_None, ut_Any
  0,1,2,3,4,5,6,7,8,9,10,11,12,13, //Citizens
  14,15,16,17,18,19,20,21,22,23, //Warriors
  -1,-1,-1,-1, {-1,-1,} //TPR warriors (can't be placed with SET_UNIT)
  24,25,26,27,28,29,30,31); //Animals

  //This is a map of the valid values for !SET_GROUP, and the corresponing unit that will be created (matches KaM behavior)
  TroopsRemap: array[14..29] of TUnitType = (
  ut_Militia,ut_AxeFighter,ut_Swordsman,ut_Bowman,ut_Arbaletman,
  ut_Pikeman,ut_Hallebardman,ut_HorseScout,ut_Cavalry,ut_Barbarian, //TSK Troops
  ut_Peasant,ut_Slingshot,ut_MetalBarbarian,ut_Horseman,
  {ut_Catapult,ut_Ballista);} //Seige, which are not yet enabled
  ut_None,ut_None); //Temp replacement for seige

  TroopsReverseRemap: array[TUnitType] of integer = (
  -1, -1, //ut_None, ut_Any
  -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, //Citizens
  14,15,16,17,18,19,20,21,22,23, //Warriors
  24,25,26,27, {28,29,} //TPR warriors
  -1,-1,-1,-1,-1,-1,-1,-1); //Animals


{ TMissionParser }
//Mode affect how certain parameters are loaded a bit differently
constructor TMissionParser.Create(aMode: TMissionParsingMode; aStrictParsing: boolean);
var i:integer;
begin
  Inherited Create;
  fParsingMode := aMode;
  fStrictParsing := aStrictParsing;

  for i:=0 to High(fRemap) do
    fRemap[i] := i;

  fRemapCount := MAX_PLAYERS;
end;


constructor TMissionParser.Create(aMode: TMissionParsingMode; aPlayersRemap: TPlayerArray; aStrictParsing:boolean);
var i:integer;
begin
  Inherited Create;
  fParsingMode := aMode;
  fStrictParsing := aStrictParsing;

  //PlayerRemap tells us which player should be used for which index
  //and which players should be ignored
  fRemap := aPlayersRemap;

  for i:=0 to High(fRemap) do
    inc(fRemapCount);
end;


function TMissionParser.LoadMission(const aFileName: string; aTerrain: TTerrain):boolean;
begin
  fTerrain := aTerrain;
  fMissionFileName := aFileName;

  Assert((aTerrain <> nil) or (fParsingMode = mpm_Info));

  //Set default values
  fMissionInfo.MapPath := '';
  fMissionInfo.MapSizeX := 0;
  fMissionInfo.MapSizeY := 0;
  fMissionInfo.MissionMode := mm_Normal;
  fMissionInfo.PlayerCount := 0;
  fMissionInfo.HumanPlayerID := PLAYER_NONE;
  fMissionInfo.VictoryCond := '';
  fMissionInfo.DefeatCond := '';

  if fParsingMode = mpm_Info then
    Result := LoadSimple(aFileName)
  else
    Result := LoadStandard(aFileName);

  //We double-check against success of every loading step and no errors
  Result := Result and (fErrorMessage='');
end;


function TMissionParser.TextToCommandType(const ACommandText: AnsiString): TKMCommandType;
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
function TMissionParser.ReadMissionFile(const aFileName: string): AnsiString;
var
  i,Num:cardinal;
  F:TMemoryStream;
begin
  if not FileExists(aFileName) then
  begin
    AddScriptError(Format('Mission file %s could not be found', [aFileName]), True);
    Result := '';
    Exit;
  end;

  //Load and decode .DAT file into FileText
  F := TMemoryStream.Create;
  try
    F.LoadFromFile(aFileName);

    if F.Size = 0 then
    begin
      AddScriptError(Format('Mission file %s is empty', [aFileName]), True);
      Result := '';
      Exit;
    end;

    //Detect whether mission is encoded so we can support decoded/encoded .DAT files
    //We can't test 1st char, it can be any. Instead see how often common chracters meet
    Num := 0;
    for i:=0 to F.Size-1 do               //tab, eol, 0..9, space, !
      if PByte(cardinal(F.Memory)+i)^ in [9,10,13,ord('0')..ord('9'),$20,$21] then
        inc(Num);

    //Usually 30-50% is numerals/spaces, tested on typical KaM maps, take half of that as margin
    if (Num/F.Size < 0.20) then
    for i:=0 to F.Size-1 do
      PByte(cardinal(F.Memory)+i)^ := PByte(cardinal(F.Memory)+i)^ xor 239;

    //Save text after decoding but before cleaning
    if WRITE_DECODED_MISSION then
      F.SaveToFile(aFileName+'.txt');

    for i:=0 to F.Size-1 do
      if PByte(cardinal(F.Memory)+i)^ in [9,10,13] then //tab, eol
        PByte(cardinal(F.Memory)+i)^ := $20; //Space

    Num := 0;
    for i:=0 to F.Size-1 do begin
      PByte(cardinal(F.Memory)+Num)^ := PByte(cardinal(F.Memory)+i)^;
      if (Num<=0) or (
        (PWord(cardinal(F.Memory)+Num-1)^ <> $2020) //Skip double spaces and !!
        and (PWord(cardinal(F.Memory)+Num-1)^ <> $2121)) then
        inc(Num);
    end;

    SetLength(Result, Num); //Because some extra characters were removed
    F.Position := 0;
    F.ReadBuffer(Result[1], Num);
  finally
    F.Free;
  end;
end;


{Acquire specific map details in a fast way}
function TMissionParser.LoadSimple(const aFileName:string):boolean;
const
  Max_Cmd=2;
var
  FileText: AnsiString;
  CommandText, Param, TextParam: AnsiString;
  ParamList: array[1..Max_Cmd] of integer;
  k, l, IntParam: integer;
  CommandType: TKMCommandType;
begin
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
          if (FileText[k]<>'!') and (k<length(FileText)) then
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
        GetDetailsProcessCommand(CommandType,ParamList,TextParam);
      end;
    end
    else
      inc(k);
  until (k>=length(FileText));
  //Apparently it's faster to parse till file end than check if all details are filled

  //It must have worked if we got to this point
  Result := true;

  Result := Result and LoadMapInfo(ChangeFileExt(fMissionFileName,'.map'));
end;


procedure TMissionParser.GetDetailsProcessCommand(CommandType: TKMCommandType; const ParamList: array of integer; TextParam:AnsiString);
begin
  with fMissionInfo do
  case CommandType of
    ct_SetMap:         MapPath       := RemoveQuotes(String(TextParam));
    ct_SetMaxPlayer:   PlayerCount   := ParamList[0];
    ct_SetTactic:      MissionMode   := mm_Tactic;
    ct_SetHumanPlayer: HumanPlayerID := ParamList[0];
{                       if TGoalCondition(ParamList[0]) = gc_Time then
                         VictoryCond := VictoryCond + fPlayers.Player[fLastPlayer].AddGoal(glt_Victory,TGoalCondition(ParamList[0]),TGoalStatus(ParamList[1]),ParamList[3],ParamList[2],play_none)
                       else
                         fPlayers.Player[fLastPlayer].AddGoal(glt_Victory,TGoalCondition(ParamList[0]),TGoalStatus(ParamList[1]),0,ParamList[2],TPlayerID(ParamList[3]));
}
    ct_AddGoal:        VictoryCond   := VictoryCond
                                        + GoalConditionStr[TGoalCondition(ParamList[0])] + ' '
                                        + GoalStatusStr[TGoalStatus(ParamList[1])]+', ';
    ct_AddLostGoal:    DefeatCond    := DefeatCond
                                        + GoalConditionStr[TGoalCondition(ParamList[0])] + ' '
                                        + GoalStatusStr[TGoalStatus(ParamList[1])]+', ';
  end;
end;


{Acquire specific map details in a fast way}
function TMissionParser.LoadMapInfo(const aFileName:string):boolean;
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
    AddScriptError('MissionParser can''t open the map because it''s too big.',true);
    Result := false;
    Exit;
  end;

  fMissionInfo.MapSizeX := sx;
  fMissionInfo.MapSizeY := sy;
  Result := true;
end;


function TMissionParser.LoadStandard(const aFileName:string):boolean;
var
  FileText, CommandText, Param, TextParam: AnsiString;
  ParamList: array[1..8] of integer;
  k, l, IntParam: integer;
  CommandType: TKMCommandType;
begin
  Result := false; //Set it right from the start

  //Reset fPlayers and other stuff
  FreeAndNil(fPlayers);
  fLastPlayer := -1;

  //Read the mission file into FileText
  FileText := ReadMissionFile(aFileName);
  if FileText = '' then Exit;

  //FileText should now be formatted nicely with 1 space between each parameter/command
  k := 1;
  repeat
    if FileText[k]='!' then
    begin
      for l:=1 to 8 do
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
      for l:=1 to 8 do
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
  begin
    AddScriptError('No human player detected - ''ct_SetHumanPlayer''',true);
    Exit;
  end;

  Result := true; //If we have reach here without exiting then it must have worked
end;


function TMissionParser.ProcessCommand(CommandType: TKMCommandType; P: array of integer; TextParam: AnsiString):boolean;
var
  MapFileName: string;
  i: integer;
  Qty: integer;
  H: TKMHouse;
  HT: THouseType;
  iPlayerAI: TKMPlayerAI;
begin
  Result := false; //Set it right from the start. There are several Exit points below

  case CommandType of
    ct_SetMap:         begin
                         MapFileName := RemoveQuotes(String(TextParam));
                         //Check for same filename.map in same folder first - Remake format
                         if FileExists(ChangeFileExt(fMissionFileName,'.map')) then
                           fTerrain.LoadFromFile(ChangeFileExt(fMissionFileName,'.map'), fParsingMode = mpm_Editor)
                         else
                         //Check for KaM format map path
                         if FileExists(ExeDir+MapFileName) then
                           fTerrain.LoadFromFile(ExeDir+MapFileName, fParsingMode = mpm_Editor)
                         else
                         begin
                           //Else abort loading and fail
                           AddScriptError('Map file couldn''t be found',true);
                           Exit;
                         end;
                       end;
    ct_SetMaxPlayer:   begin
                         if fPlayers=nil then
                           fPlayers := TKMPlayersCollection.Create(fTerrain);
                         if fParsingMode = mpm_Single then
                           fPlayers.AddPlayers(P[0])
                         else
                           fPlayers.AddPlayers(fRemapCount);
                       end;
    ct_SetTactic:       begin
                          if fPlayers = nil then
                            fPlayers := TKMPlayersCollection.Create(fTerrain);
                          fMissionInfo.MissionMode := mm_Tactic;
                        end;
    ct_SetCurrPlayer:   if InRange(P[0], 0, MAX_PLAYERS-1) then
                        begin
                          fLastPlayer := fRemap[P[0]]; //
                          fLastHouse := nil;
                          fLastTroop := nil;
                        end;
    ct_SetHumanPlayer:  if (fParsingMode <> mpm_Multi) and (fPlayers <> nil) then
                          if InRange(P[0], 0, fPlayers.Count-1) then
                          begin
                            fMissionInfo.HumanPlayerID := P[0];
                            fPlayers.Player[P[0]].PlayerType := pt_Human;
                          end;
                        //Multiplayer will set Human player itself after loading
    ct_AIPlayer:        if (fParsingMode <> mpm_Multi) and (fPlayers <> nil) then
                          if InRange(P[0],0,fPlayers.Count-1) then
                            fPlayers.Player[P[0]].PlayerType:=pt_Computer
                          else //This command doesn't require an ID, just use the current player
                            fPlayers.Player[fLastPlayer].PlayerType:=pt_Computer;
                        //Multiplayer will set AI players itself after loading
    ct_CenterScreen:    if fLastPlayer >= 0 then
                          fPlayers.Player[fLastPlayer].CenterScreen := KMPoint(P[0]+1,P[1]+1);
    ct_ClearUp:         if fLastPlayer >= 0 then
                          if P[0] = 255 then
                            fPlayers.Player[fLastPlayer].FogOfWar.RevealEverything
                          else
                            fPlayers.Player[fLastPlayer].FogOfWar.RevealCircle(KMPoint(P[0]+1,P[1]+1), P[2], 255);
    ct_SetHouse:        if fLastPlayer >= 0 then
                          if InRange(P[0], Low(HouseKaMType), High(HouseKaMType)) then
                            if fTerrain.CanPlaceHouseFromScript(HouseKaMType[P[0]], KMPoint(P[1]+1, P[2]+1)) then //todo: If the house can't be placed, show a warning so the guy making the map knows about it
                              fLastHouse := fPlayers.Player[fLastPlayer].AddHouse(
                                HouseKaMType[P[0]], P[1]+1, P[2]+1, false);
    ct_SetHouseDamage:  if fLastPlayer >= 0 then //Skip false-positives for skipped players
                          if fLastHouse <> nil then
                            fLastHouse.AddDamage(min(P[0],high(word)), fParsingMode = mpm_Editor)
                          else
                            AddScriptError('ct_SetHouseDamage without prior declaration of House');
    ct_SetUnit:         begin
                          //Animals should be added regardless of current player
                          if UnitsRemap[P[0]] in [ANIMAL_MIN..ANIMAL_MAX] then
                            fPlayers.PlayerAnimals.AddUnit(UnitsRemap[P[0]], KMPoint(P[1]+1, P[2]+1))
                          else
                          if (fLastPlayer >= 0) and (UnitsRemap[P[0]] in [HUMANS_MIN..HUMANS_MAX]) then
                            fPlayers.Player[fLastPlayer].AddUnit(UnitsRemap[P[0]], KMPoint(P[1]+1, P[2]+1));
                        end;

    ct_SetUnitByStock:  if fLastPlayer >= 0 then
                          if UnitsRemap[P[0]] in [HUMANS_MIN..HUMANS_MAX] then
                          begin
                            H := fPlayers.Player[fLastPlayer].FindHouse(ht_Store, 1);
                            if H <> nil then
                              fPlayers.Player[fLastPlayer].AddUnit(UnitsRemap[P[0]], KMPoint(H.GetEntrance.X, H.GetEntrance.Y+1));
                          end;
    ct_SetRoad:         if fLastPlayer >= 0 then
                          fPlayers.Player[fLastPlayer].AddRoadToList(KMPoint(P[0]+1,P[1]+1));
    ct_SetField:        if fLastPlayer >= 0 then
                          fPlayers.Player[fLastPlayer].AddField(KMPoint(P[0]+1,P[1]+1),ft_Corn);
    ct_Set_Winefield:   if fLastPlayer >= 0 then
                          fPlayers.Player[fLastPlayer].AddField(KMPoint(P[0]+1,P[1]+1),ft_Wine);
    ct_SetStock:        if fLastPlayer >= 0 then
                        begin //This command basically means: Put a SH here with road bellow it
                          fLastHouse := fPlayers.Player[fLastPlayer].AddHouse(ht_Store, P[0]+1,P[1]+1, false);
                          fPlayers.Player[fLastPlayer].AddRoadToList(KMPoint(P[0]+1,P[1]+2));
                          fPlayers.Player[fLastPlayer].AddRoadToList(KMPoint(P[0],P[1]+2));
                          fPlayers.Player[fLastPlayer].AddRoadToList(KMPoint(P[0]-1,P[1]+2));
                        end;
    ct_AddWare:         if (fParsingMode <> mpm_Preview) then
                        if fLastPlayer >= 0 then
                        begin
                          Qty := EnsureRange(P[1], -1, High(Word)); //Sometimes user can define it to be 999999
                          if Qty = -1 then Qty := High(Word); //-1 means maximum resources
                          H := fPlayers.Player[fLastPlayer].FindHouse(ht_Store,1);
                          if (H <> nil) and (ResourceKaMIndex[P[0]] in [WARE_MIN..WARE_MAX]) then
                            H.ResAddToIn(ResourceKaMIndex[P[0]], Qty, True);
                        end;
    ct_AddWareToAll:    if (fParsingMode <> mpm_Preview) then
                        begin
                          Qty := EnsureRange(P[1], -1, High(Word)); //Sometimes user can define it to be 999999
                          if Qty = -1 then Qty := High(Word); //-1 means maximum resources
                          for i:=0 to fPlayers.Count-1 do
                          begin
                            H := fPlayers.Player[i].FindHouse(ht_Store,1);
                            if (H<>nil) and (ResourceKaMIndex[P[0]] in [WARE_MIN..WARE_MAX]) then
                              H.ResAddToIn(ResourceKaMIndex[P[0]], Qty, True);
                          end;
                        end;
    ct_AddWareToSecond: if (fParsingMode <> mpm_Preview) then
                        if fLastPlayer >= 0 then
                        begin
                          Qty := EnsureRange(P[1], -1, High(Word)); //Sometimes user can define it to be 999999
                          if Qty = -1 then Qty := High(Word); //-1 means maximum resources

                          H := TKMHouseStore(fPlayers.Player[fLastPlayer].FindHouse(ht_Store, 2));
                          if (H <> nil) and (ResourceKaMIndex[P[0]] in [WARE_MIN..WARE_MAX]) then
                            H.ResAddToIn(ResourceKaMIndex[P[0]], Qty, True);
                        end;
    ct_AddWareTo:       if (fParsingMode <> mpm_Preview) then
                        if fLastPlayer >= 0 then
                        begin //HouseType, House Order, Ware Type, Count
                          Qty := EnsureRange(P[3], -1, High(Word)); //Sometimes user can define it to be 999999
                          if Qty = -1 then Qty := High(Word); //-1 means maximum resources

                          H := fPlayers.Player[fLastPlayer].FindHouse(HouseKaMType[P[0]], P[1]);
                          if (H <> nil) and (ResourceKaMIndex[P[2]] in [WARE_MIN..WARE_MAX]) then
                            H.ResAddToIn(ResourceKaMIndex[P[2]], Qty, True);
                        end;
    ct_AddWeapon:       if (fParsingMode <> mpm_Preview) then
                        if fLastPlayer >= 0 then
                        begin
                          Qty := EnsureRange(P[1], -1, High(Word)); //Sometimes user can define it to be 999999
                          if Qty = -1 then Qty := High(Word); //-1 means maximum weapons
                          H := TKMHouseBarracks(fPlayers.Player[fLastPlayer].FindHouse(ht_Barracks, 1));
                          if (H <> nil) and (ResourceKaMIndex[P[0]] in [WARFARE_MIN..WARFARE_MAX]) then
                            H.ResAddToIn(ResourceKaMIndex[P[0]], Qty, True);
                        end;
    ct_BlockTrade:      if (fParsingMode <> mpm_Preview) then
                        if fLastPlayer >= 0 then
                        begin
                          if ResourceKaMIndex[P[0]] in [WARE_MIN..WARE_MAX] then
                            fPlayers.Player[fLastPlayer].Stats.AllowToTrade[ResourceKaMIndex[P[0]]] := false;
                        end;
    ct_BlockHouse:      if (fParsingMode <> mpm_Preview) then
                        if fLastPlayer >= 0 then
                        begin
                          if InRange(P[0], Low(HouseKaMType), High(HouseKaMType)) then
                            fPlayers.Player[fLastPlayer].Stats.AllowToBuild[HouseKaMType[P[0]]] := false;
                        end;
    ct_ReleaseHouse:    if (fParsingMode <> mpm_Preview) then
                        if fLastPlayer >= 0 then
                        begin
                          if InRange(P[0], Low(HouseKaMType), High(HouseKaMType)) then
                            fPlayers.Player[fLastPlayer].Stats.HouseReleased[HouseKaMType[P[0]]] := true;
                        end;
    ct_ReleaseAllHouses:if (fParsingMode <> mpm_Preview) then
                        if fLastPlayer >= 0 then
                          for HT:=Low(THouseType) to High(THouseType) do
                            fPlayers.Player[fLastPlayer].Stats.HouseReleased[HT] := True;
    ct_SetGroup:        if fLastPlayer >= 0 then
                          if InRange(P[0], Low(TroopsRemap), High(TroopsRemap)) and (TroopsRemap[P[0]] <> ut_None) then
                            fLastTroop := TKMUnitWarrior(fPlayers.Player[fLastPlayer].AddUnitGroup(
                              TroopsRemap[P[0]],
                              KMPoint(P[1]+1, P[2]+1),
                              TKMDirection(P[3]+1),
                              P[4],
                              P[5],
                              fParsingMode=mpm_Editor //Editor mode = true
                              ));
    ct_SendGroup:       if (fParsingMode <> mpm_Preview) then
                        if fLastPlayer >= 0 then
                        begin
                          if fLastTroop <> nil then
                            fLastTroop.OrderWalk(KMPoint(P[0]+1, P[1]+1), TKMDirection(P[2]+1))
                          else
                            AddScriptError('ct_SendGroup without prior declaration of Troop');
                        end;
    ct_SetGroupFood:    if (fParsingMode <> mpm_Preview) then
                        if fLastPlayer >= 0 then
                        begin
                          if fLastTroop <> nil then
                            fLastTroop.SetGroupFullCondition
                          else
                            AddScriptError('ct_SetGroupFood without prior declaration of Troop');
                        end;
    ct_AICharacter:     if (fParsingMode <> mpm_Preview) then
                        if fLastPlayer >= 0 then
                        begin
                          if fPlayers.Player[fLastPlayer].PlayerType <> pt_Computer then Exit;
                          iPlayerAI := fPlayers.Player[fLastPlayer].AI; //Setup the AI's character
                          if TextParam = PARAMVALUES[cpt_Recruits]     then iPlayerAI.ReqRecruits         := P[1];
                          if TextParam = PARAMVALUES[cpt_Constructors] then iPlayerAI.ReqWorkers          := P[1];
                          if TextParam = PARAMVALUES[cpt_WorkerFactor] then iPlayerAI.ReqSerfFactor       := P[1];
                          if TextParam = PARAMVALUES[cpt_RecruitCount] then iPlayerAI.RecruitTrainTimeout := P[1];
                          if TextParam = PARAMVALUES[cpt_TownDefence]  then iPlayerAI.TownDefence         := P[1];
                          if TextParam = PARAMVALUES[cpt_MaxSoldier]   then iPlayerAI.MaxSoldiers         := P[1];
                          if TextParam = PARAMVALUES[cpt_EquipRate]    then iPlayerAI.EquipRate           := P[1];
                          if TextParam = PARAMVALUES[cpt_AttackFactor] then iPlayerAI.Aggressiveness      := P[1];
                          if TextParam = PARAMVALUES[cpt_TroopParam]   then
                          begin
                            iPlayerAI.TroopFormations[TGroupType(P[1])].NumUnits := P[2];
                            iPlayerAI.TroopFormations[TGroupType(P[1])].UnitsPerRow  := P[3];
                          end;
                        end;
    ct_AINoBuild:       if (fParsingMode <> mpm_Preview) then
                        if fLastPlayer >= 0 then
                          fPlayers.Player[fLastPlayer].AI.Autobuild := false;
    ct_AIStartPosition: if fLastPlayer >= 0 then
                          fPlayers.Player[fLastPlayer].AI.StartPosition := KMPoint(P[0]+1,P[1]+1);
    ct_SetAlliance:     if (fLastPlayer >=0) and (fRemap[P[0]] >= 0) then
                          if P[1] = 1 then
                            fPlayers.Player[fLastPlayer].Alliances[fRemap[P[0]]] := at_Ally
                          else
                            fPlayers.Player[fLastPlayer].Alliances[fRemap[P[0]]] := at_Enemy;
    ct_AttackPosition:  if (fParsingMode <> mpm_Preview) then
                        if fLastPlayer >= 0 then
                          //If target is building: Attack building
                          //If target is unit: Chase/attack unit
                          //If target is nothing: move to position
                          //However, because the unit/house target may not have been created yet, this must be processed after everything else
                          if fLastTroop <> nil then
                          begin
                            inc(fAttackPositionsCount);
                            SetLength(fAttackPositions, fAttackPositionsCount+1);
                            fAttackPositions[fAttackPositionsCount-1].Warrior := fLastTroop;
                            fAttackPositions[fAttackPositionsCount-1].Target := KMPoint(P[0]+1,P[1]+1);
                          end
                          else
                            AddScriptError('ct_AttackPosition without prior declaration of Troop');
    ct_AddGoal:         if (fParsingMode <> mpm_Preview) then
                        if fLastPlayer >= 0 then
                          //If the condition is time then P[3] is the time, else it is player ID
                          if TGoalCondition(P[0]) = gc_Time then
                            fPlayers.Player[fLastPlayer].Goals.AddGoal(glt_Victory,TGoalCondition(P[0]),TGoalStatus(P[1]),P[3],P[2],-1)
                          else
                            if fRemap[P[3]] >= 0 then
                              if fRemap[P[3]] <= fPlayers.Count-1 then
                                fPlayers.Player[fLastPlayer].Goals.AddGoal(glt_Victory,TGoalCondition(P[0]),TGoalStatus(P[1]),0,P[2],fRemap[P[3]])
                              else
                                AddScriptError('Add_Goal for non existing player');
    ct_AddLostGoal:     if (fParsingMode <> mpm_Preview) then
                        if fLastPlayer >= 0 then
                          //If the condition is time then P[3] is the time, else it is player ID
                          if TGoalCondition(P[0]) = gc_Time then
                            fPlayers.Player[fLastPlayer].Goals.AddGoal(glt_Survive,TGoalCondition(P[0]),TGoalStatus(P[1]),P[3],P[2],-1)
                          else
                            if fRemap[P[3]] >= 0 then
                              fPlayers.Player[fLastPlayer].Goals.AddGoal(glt_Survive,TGoalCondition(P[0]),TGoalStatus(P[1]),0,P[2],fRemap[P[3]])
                            else
                              AddScriptError('Add_LostGoal for non existing player');
    ct_AIDefence:       if (fParsingMode <> mpm_Preview) then
                        if fLastPlayer >=0 then
                          fPlayers.Player[fLastPlayer].AI.AddDefencePosition(KMPointDir(P[0]+1, P[1]+1, TKMDirection(P[2]+1)),TGroupType(P[3]),P[4],TAIDefencePosType(P[5]));
    ct_SetMapColor:     if fLastPlayer >=0 then
                          //For now simply use the minimap color for all color, it is too hard to load all 8 shades from ct_SetNewRemap
                          fPlayers.Player[fLastPlayer].FlagColor := fResource.Palettes.DefDal.Color32(P[0]);
    ct_AIAttack:        if (fParsingMode <> mpm_Preview) then
                        begin
                          //Set up the attack command
                          if TextParam = AI_ATTACK_PARAMS[cpt_Type] then
                            if InRange(P[1], Low(RemakeAttackType), High(RemakeAttackType)) then
                              fAIAttack.AttackType := RemakeAttackType[P[1]]
                            else
                              AddScriptError('Unknown parameter ' + IntToStr(P[1]) + ' at ct_AIAttack');
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
    ct_CopyAIAttack:    if (fParsingMode <> mpm_Preview) then
                        if fLastPlayer >= 0 then
                          //Save the attack to the AI assets
                          fPlayers.Player[fLastPlayer].AI.Attacks.AddAttack(fAIAttack);
    ct_EnablePlayer:    begin
                          //Serves no real purpose, all players have this command anyway
                        end;
    ct_SetNewRemap:     begin
                          //Disused. Minimap color is used for all colors now. However it might be better to use these values in the long run as sometimes the minimap colors do not match well
                        end;
  end;
  Result := true; //Must have worked if we haven't exited by now
end;


//A nice way of debugging script errors.
//Shows the error to the user so they know exactly what they did wrong.
procedure TMissionParser.AddScriptError(const ErrorMsg:string; aFatal:boolean=false);
begin
  if fStrictParsing or aFatal then
    fErrorMessage := fErrorMessage + ErrorMsg + '|';
end;


//Determine what we are attacking: House, Unit or just walking to some place
procedure TMissionParser.ProcessAttackPositions;
var
  i: integer;
  H: TKMHouse;
  U: TKMUnit;
begin
  for i:=0 to fAttackPositionsCount-1 do
    with fAttackPositions[i] do
    begin
      H := fPlayers.HousesHitTest(Target.X,Target.Y); //Attack house
      if (H <> nil) and (not H.IsDestroyed) and (fPlayers.CheckAlliance(Warrior.GetOwner,H.GetOwner) = at_Enemy) then
        Warrior.OrderAttackHouse(H)
      else
      begin
        U := fTerrain.UnitsHitTest(Target.X,Target.Y); //Chase/attack unit
        if (U <> nil) and (not U.IsDeadOrDying) and (fPlayers.CheckAlliance(Warrior.GetOwner,U.GetOwner) = at_Enemy) then
          Warrior.OrderAttackUnit(U)
        else
          Warrior.OrderWalk(Target); //Just move to position
      end;
    end;
end;


//Write out a KaM format mission file to aFileName
procedure TMissionParser.SaveDATFile(const aFileName: String);
const
  COMMANDLAYERS = 4;
var
  f:textfile;
  i: longint; //longint because it is used for encoding entire output, which will limit the file size
  k,iX,iY,CommandLayerCount: Integer;
  HouseCount: array[THouseType] of Integer;
  Res: TResourceType;
  G: TGroupType;
  U: TKMUnit;
  H: TKMHouse;
  HT: THouseType;
  ReleaseAllHouses: boolean;
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
  var OutData: AnsiString; i:integer;
  begin
    OutData := '!' + COMMANDVALUES[aCommand];

    if aComParam <> cpt_Unknown then
      OutData := OutData + ' ' + PARAMVALUES[aComParam];

    for i:=Low(aParams) to High(aParams) do
      OutData := OutData + ' ' + AnsiString(IntToStr(aParams[i]));

    AddData(OutData);
  end;

  procedure AddCommand(aCommand: TKMCommandType; aComParam: TAIAttackParamType; aParams: array of integer); overload;
  var OutData: AnsiString; i:integer;
  begin
    OutData := '!' + COMMANDVALUES[aCommand] + ' ' + AI_ATTACK_PARAMS[aComParam];

    for i:=Low(aParams) to High(aParams) do
      OutData := OutData + ' ' + AnsiString(IntToStr(aParams[i]));

    AddData(OutData);
  end;

  procedure AddCommand(aCommand:TKMCommandType; aParams:array of integer); overload;
  begin
    AddCommand(aCommand, cpt_Unknown, aParams);
  end;

begin

  //Put data into stream
  SaveString := '';
  CommandLayerCount := -1; //Some commands (road/fields) are layered so the file is easier to read (not so many lines)

  //Main header, use same filename for MAP
  AddData('!'+COMMANDVALUES[ct_SetMap] + ' "data\mission\smaps\' + AnsiString(ExtractFileName(TruncateExt(aFileName))) + '.map"');
  AddCommand(ct_SetMaxPlayer, [fPlayers.Count]);
  AddData(''); //NL

  //Player loop
  for i:=0 to fPlayers.Count-1 do
  begin
    //Player header, using same order of commands as KaM
    AddCommand(ct_SetCurrPlayer, [i]); //In script player 0 is the first
    if fPlayers.Player[i].PlayerType = pt_Human then
      AddCommand(ct_SetHumanPlayer, [i]);
    AddCommand(ct_EnablePlayer, [i]);
    if fPlayers.Player[i].PlayerType = pt_Computer then
      AddCommand(ct_AIPlayer, []);

    AddCommand(ct_SetMapColor, [fPlayers.Player[i].FlagColorIndex]);
    if not KMSamePoint(fPlayers.Player[i].CenterScreen, KMPoint(0,0)) then
      AddCommand(ct_CenterScreen, [fPlayers.Player[i].CenterScreen.X-1,fPlayers.Player[i].CenterScreen.Y-1]);

    AddData(''); //NL

    //Human specific, e.g. goals, center screen (though all players can have it, only human can use it)
    for k:=0 to fPlayers.Player[i].Goals.Count-1 do
      with fPlayers.Player[i].Goals[k] do
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
    if fPlayers.Player[i].PlayerType = pt_Computer then
    begin
      AddCommand(ct_AIStartPosition, [fPlayers.Player[i].AI.StartPosition.X-1,fPlayers.Player[i].AI.StartPosition.Y-1]);
      if not fPlayers.Player[i].AI.Autobuild then
        AddCommand(ct_AINoBuild, []);
      AddCommand(ct_AICharacter,cpt_Recruits, [fPlayers.Player[i].AI.ReqRecruits]);
      AddCommand(ct_AICharacter,cpt_WorkerFactor, [fPlayers.Player[i].AI.ReqSerfFactor]);
      AddCommand(ct_AICharacter,cpt_Constructors, [fPlayers.Player[i].AI.ReqWorkers]);
      AddCommand(ct_AICharacter,cpt_TownDefence, [fPlayers.Player[i].AI.TownDefence]);
      //Only store if a limit is in place (high is the default)
      if fPlayers.Player[i].AI.MaxSoldiers <> high(fPlayers.Player[i].AI.MaxSoldiers) then
        AddCommand(ct_AICharacter,cpt_MaxSoldier, [fPlayers.Player[i].AI.MaxSoldiers]);
      AddCommand(ct_AICharacter,cpt_EquipRate,    [fPlayers.Player[i].AI.EquipRate]);
      AddCommand(ct_AICharacter,cpt_AttackFactor, [fPlayers.Player[i].AI.Aggressiveness]);
      AddCommand(ct_AICharacter,cpt_RecruitCount, [fPlayers.Player[i].AI.RecruitTrainTimeout]);
      for G:=Low(TGroupType) to High(TGroupType) do
        if fPlayers.Player[i].AI.TroopFormations[G].NumUnits <> 0 then //Must be valid and used
          AddCommand(ct_AICharacter, cpt_TroopParam, [KaMGroupType[G], fPlayers.Player[i].AI.TroopFormations[G].NumUnits, fPlayers.Player[i].AI.TroopFormations[G].UnitsPerRow]);
      AddData(''); //NL
      for k:=0 to fPlayers.Player[i].AI.DefencePositionsCount-1 do
        with fPlayers.Player[i].AI.DefencePositions[k] do
          AddCommand(ct_AIDefence, [Position.Loc.X-1,Position.Loc.Y-1,byte(Position.Dir)-1,KaMGroupType[GroupType],DefenceRadius,byte(DefenceType)]);
      AddData(''); //NL
      AddData(''); //NL
      for k:=0 to fPlayers.Player[i].AI.Attacks.Count - 1 do
        with fPlayers.Player[i].AI.Attacks[k] do
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

          AddCommand(ct_CopyAIAttack, [k]); //Store attack with ID number
          AddData(''); //NL
        end;
      AddData(''); //NL
    end;

    //General, e.g. units, roads, houses, etc.
    //Alliances
    for k:=0 to fPlayers.Count-1 do
      if k<>i then
        AddCommand(ct_SetAlliance, [k, byte(fPlayers.Player[i].Alliances[k])]); //0=enemy, 1=ally
    AddData(''); //NL

    //Release/block houses
    ReleaseAllHouses := true;
    for HT := Low(THouseType) to High(THouseType) do
    if fResource.HouseDat[HT].IsValid then //Exclude ht_None / ht_Any
    begin
      if not fPlayers.Player[i].Stats.AllowToBuild[HT] then
      begin
        AddCommand(ct_BlockHouse, [HouseKaMOrder[HT]-1]);
        ReleaseAllHouses := false;
      end
      else
        if fPlayers.Player[i].Stats.HouseReleased[HT] then
          AddCommand(ct_ReleaseHouse, [HouseKaMOrder[HT]-1])
        else
          ReleaseAllHouses := false;
    end;
    if ReleaseAllHouses then
      AddCommand(ct_ReleaseAllHouses, []);

    //Block trades
    for Res := WARE_MIN to WARE_MAX do
      if not fPlayers.Player[i].Stats.AllowToTrade[Res] then
        AddCommand(ct_BlockTrade, [ResourceKaMOrder[Res]]);

    //Houses
    for k:=0 to fPlayers.Player[i].Houses.Count-1 do
    begin
      H := fPlayers.Player[i].Houses[k];
      if not H.IsDestroyed then
      begin
        AddCommand(ct_SetHouse, [HouseKaMOrder[H.HouseType]-1, H.GetPosition.X-1, H.GetPosition.Y-1]);
        if H.IsDamaged then
          AddCommand(ct_SetHouseDamage, [H.GetDamage]);
      end;
    end;
    AddData(''); //NL

    //Wares. Check every house to see if it has any wares in it
    FillChar(HouseCount, SizeOf(HouseCount), #0);
    for k:=0 to fPlayers.Player[i].Houses.Count-1 do
    begin
      H := fPlayers.Player[i].Houses[k];
      inc(HouseCount[H.HouseType]);

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
            AddCommand(ct_AddWareTo, [HouseKaMOrder[H.HouseType]-1, HouseCount[H.HouseType], ResourceKaMOrder[Res], H.CheckResIn(Res)]);

    end;
    AddData(''); //NL


    //Roads and fields. We must check EVERY terrain tile
    CommandLayerCount := 0; //Enable command layering
    for iY := 1 to fTerrain.MapY do
      for iX := 1 to fTerrain.MapX do
        if fTerrain.Land[iY,iX].TileOwner = fPlayers.Player[i].PlayerIndex then
        begin
          if fTerrain.Land[iY,iX].TileOverlay = to_Road then
            AddCommand(ct_SetRoad, [iX-1,iY-1]);
          if fTerrain.TileIsCornField(KMPoint(iX,iY)) then
            AddCommand(ct_SetField, [iX-1,iY-1]);
          if fTerrain.TileIsWineField(KMPoint(iX,iY)) then
            AddCommand(ct_Set_Winefield, [iX-1,iY-1]);
        end;
    CommandLayerCount := -1; //Disable command layering
    AddData(''); //Extra NL because command layering doesn't put one
    AddData(''); //NL

    //Units
    for k:=0 to fPlayers.Player[i].Units.Count-1 do
    begin
      U := fPlayers.Player[i].Units[k];
      if U is TKMUnitWarrior then
      begin
        if TKMUnitWarrior(U).IsCommander then //Parse only Commanders
        begin
          AddCommand(ct_SetGroup, [TroopsReverseRemap[U.UnitType], U.GetPosition.X-1, U.GetPosition.Y-1, Byte(U.Direction)-1, TKMUnitWarrior(U).UnitsPerRow, TKMUnitWarrior(U).fMapEdMembersCount+1]);
          if U.Condition = UNIT_MAX_CONDITION then
            AddCommand(ct_SetGroupFood, []);
        end;
      end
      else
        AddCommand(ct_SetUnit, [UnitReverseRemap[U.UnitType], U.GetPosition.X-1, U.GetPosition.Y-1]);
    end;

    AddData(''); //NL
    AddData(''); //NL
  end; //Player loop

  //Main footer

  //Animals, wares to all, etc. go here
  AddData('//Animals');
  for i:=0 to fPlayers.PlayerAnimals.Units.Count-1 do
  begin
    U := fPlayers.PlayerAnimals.Units[i];
    AddCommand(ct_SetUnit, [UnitReverseRemap[U.UnitType], U.GetPosition.X-1, U.GetPosition.Y-1]);
  end;
  AddData(''); //NL

  //Similar footer to one in Lewin's Editor, useful so ppl know what mission was made with.
  AddData('//This mission was made with KaM Remake Map Editor version '+GAME_VERSION+' at '+AnsiString(DateTimeToStr(Now)));

  //Write uncoded file for debug
  assignfile(f, aFileName+'.txt'); rewrite(f);
  write(f, SaveString);
  closefile(f);

  //Encode it
  for i:=1 to Length(SaveString) do
    SaveString[i] := AnsiChar(Byte(SaveString[i]) xor 239);

  //Write it
  assignfile(f, aFileName); rewrite(f);
  write(f, SaveString);
  closefile(f);
end;


end.
