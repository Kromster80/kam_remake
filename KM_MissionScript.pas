unit KM_MissionScript;
{$I KaM_Remake.inc}
interface
uses
  {$IFDEF MSWindows} Windows, {$ENDIF}
  Classes, KromUtils, SysUtils, Dialogs, Math, KM_CommonTypes, KM_Defaults, KM_Utils,
  KM_Houses, KM_Units, KM_Units_Warrior;


  {Settings for mission loader:

  Info
  - Read basic mission properties (size, players, mode, ..)

  Single player
  -

  Multi-player
  - skip players loading
  - alter starting resources
  - alter starting army
  - alter starting houses

  MapEd
  - ignore certain errors
  - load armies differently

  }

type
  TMissionParsingMode = (
                          mpm_Info,  //Return basic mission properties
                          mpm_Single,
                          mpm_Multi, // Skip players
                          mpm_Editor // Ignore errors, load armies differently
                        );

  TKMCommandType = (ct_Unknown=0,ct_SetMap,ct_SetMaxPlayer,ct_SetCurrPlayer,ct_SetHumanPlayer,ct_SetHouse,
                    ct_SetTactic,ct_AIPlayer,ct_EnablePlayer,ct_SetNewRemap,ct_SetMapColor,ct_CenterScreen,
                    ct_ClearUp,ct_BlockHouse,ct_ReleaseHouse,ct_ReleaseAllHouses,ct_AddGoal,ct_AddLostGoal,
                    ct_SetUnit,ct_SetRoad,ct_SetField,ct_Set_Winefield,ct_SetStock,ct_AddWare,ct_SetAlliance,
                    ct_SetHouseDamage,ct_SetUnitByStock,ct_SetGroup,ct_SetGroupFood,ct_SendGroup,
                    ct_AttackPosition,ct_AddWareToSecond,ct_AddWareToAll,ct_AddWeapon,ct_AICharacter,
                    ct_AINoBuild,ct_AIStartPosition,ct_AIDefence,ct_AIAttack,ct_CopyAIAttack);

  TKMCommandParamType = (cpt_Unknown=0,cpt_Recruits,cpt_Constructors,cpt_WorkerFactor,cpt_RecruitCount,cpt_TownDefence,
                         cpt_MaxSoldier,cpt_AttackFactor,cpt_TroopParam,cpt_Type,cpt_TotalAmount,cpt_Counter,cpt_Range,
                         cpt_TroopAmount,cpt_Target,cpt_Position,cpt_TakeAll);

const
  COMMANDVALUES: array[TKMCommandType] of shortstring = (
    '','SET_MAP','SET_MAX_PLAYER','SET_CURR_PLAYER','SET_HUMAN_PLAYER','SET_HOUSE',
    'SET_TACTIC','SET_AI_PLAYER','ENABLE_PLAYER','SET_NEW_REMAP','SET_MAP_COLOR',
    'CENTER_SCREEN','CLEAR_UP','BLOCK_HOUSE','RELEASE_HOUSE','RELEASE_ALL_HOUSES',
    'ADD_GOAL','ADD_LOST_GOAL','SET_UNIT','SET_STREET','SET_FIELD','SET_WINEFIELD',
    'SET_STOCK','ADD_WARE','SET_ALLIANCE','SET_HOUSE_DAMAGE','SET_UNIT_BY_STOCK',
    'SET_GROUP','SET_GROUP_FOOD','SEND_GROUP','ATTACK_POSITION','ADD_WARE_TO_SECOND',
    'ADD_WARE_TO_ALL','ADD_WEAPON','SET_AI_CHARACTER','SET_AI_NO_BUILD','SET_AI_START_POSITION',
    'SET_AI_DEFENSE','SET_AI_ATTACK','COPY_AI_ATTACK');

  PARAMVALUES: array[TKMCommandParamType] of shortstring = (
    '','RECRUTS','CONSTRUCTORS','WORKER_FACTOR','RECRUT_COUNT','TOWN_DEFENSE',
    'MAX_SOLDIER','ATTACK_FACTOR','TROUP_PARAM','TYPE','TOTAL_AMOUNT','COUNTER','RANGE',
    'TROUP_AMOUNT','TARGET','POSITION','TAKEALL');

  MAXPARAMS = 8;

  //This is a map of the valid values for !SET_UNIT, and the corrisponing unit that will be created (matches KaM behavior)
  UnitsRemap: array[0..31] of TUnitType = (ut_Serf,ut_Woodcutter,ut_Miner,ut_AnimalBreeder,
    ut_Farmer,ut_Lamberjack,ut_Baker,ut_Butcher,ut_Fisher,ut_Worker,ut_StoneCutter,
    ut_Smith,ut_Metallurgist,ut_Recruit, //Units
    ut_Militia,ut_AxeFighter,ut_Swordsman,ut_Bowman,ut_Arbaletman,ut_Pikeman,ut_Hallebardman,
    ut_HorseScout,ut_Cavalry,ut_Barbarian, //Troops
    ut_Wolf,ut_Fish,ut_Watersnake,ut_Seastar,ut_Crab,ut_Waterflower,ut_Waterleaf,ut_Duck); //Animals

  //This is a map of the valid values for !SET_GROUP, and the corrisponing unit that will be created (matches KaM behavior)
  TroopsRemap: array[14..29] of TUnitType = (ut_Militia,ut_AxeFighter,ut_Swordsman,ut_Bowman,ut_Arbaletman,
  ut_Pikeman,ut_Hallebardman,ut_HorseScout,ut_Cavalry,ut_Barbarian, //TSK Troops
  {ut_Peasant,ut_Slingshot,ut_MetalBarbarian,ut_Horseman,ut_Catapult,ut_Ballista);} //TPR Troops, which are not yet enabled
  ut_None,ut_None,ut_None,ut_None,ut_None,ut_None); //Temp replacement for TPR Troops

type
  TKMMissionDetails = record
    MapPath: string;
    MapSize: TKMPoint;
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

type
  TMissionParser = class
  private
    fParsingMode:TMissionParsingMode; //Data gets sent to Game differently depending on Game/Editor mode
    fErrorMessage:string; //Errors descriptions accumulate here
    fMissionFileName:string;

    fCurrentPlayerIndex: integer;
    fLastHouse: TKMHouse;
    fLastTroop: TKMUnitWarrior;
    fAIAttack: TAIAttack;
    fAttackPositions: array of TKMAttackPosition;
    fAttackPositionsCount: integer;

    fMissionInfo:TKMMissionDetails;

    function LoadSimple(const aFileName:string):boolean;
    function LoadStandard(const aFileName:string):boolean;
    function LoadMapInfo(const aFileName:string):boolean;

    function TextToCommandType(const ACommandText: shortstring): TKMCommandType;
    function UnitTypeToScriptID(aUnitType:TUnitType):integer;
    function ProcessCommand(CommandType: TKMCommandType; ParamList: array of integer; TextParam:shortstring):boolean;
    procedure GetDetailsProcessCommand(CommandType: TKMCommandType; const ParamList: array of integer; TextParam:shortstring);
    procedure DebugScriptError(const ErrorMsg:string);
    procedure ProcessAttackPositions;
    function ReadMissionFile(const aFileName:string):string;
  public
    constructor Create(aMode:TMissionParsingMode);
    function LoadMission(const aFileName:string):boolean;

    property ErrorMessage:string read fErrorMessage;
    property MissionDetails:TKMMissionDetails read fMissionInfo;

    function SaveDATFile(const aFileName:string):boolean;
  end;


implementation
uses KM_PlayersCollection, KM_Terrain, KM_Viewport, KM_Player, KM_PlayerAI, KM_ResourceGFX;


{ TMissionParser }
//Mode affect how certain parameters are loaded a bit differently
constructor TMissionParser.Create(aMode:TMissionParsingMode);
begin
  Inherited Create;
  fParsingMode := aMode;
end;


function TMissionParser.LoadMission(const aFileName:string):boolean;
begin
  fMissionFileName := aFileName;

  //Set default values
  fMissionInfo.MapPath := '';
  fMissionInfo.MapSize.X := 0;
  fMissionInfo.MapSize.Y := 0;
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


function TMissionParser.TextToCommandType(const ACommandText: shortstring): TKMCommandType;
var
  i: TKMCommandType;
begin
  Result := ct_Unknown;
  for i:=low(TKMCommandType) to high(TKMCommandType) do
  begin
    if ACommandText = '!'+COMMANDVALUES[i] then
    begin
      Result := i;
      break;
    end;
  end;
  if Result = ct_Unknown then fLog.AddToLog(ACommandText);
end;


//Read mission file to a string and if necessary - decode it
function TMissionParser.ReadMissionFile(const aFileName:string):string;
var
  i,Num:cardinal;
  F:TMemoryStream;
begin
  if not CheckFileExists(aFileName) then exit;

  //Load and decode .DAT file into FileText
  F := TMemoryStream.Create;
  try
    F.LoadFromFile(aFileName);

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
  CommandText, Param, TextParam: shortstring;
  ParamList: array[1..Max_Cmd] of integer;
  k, l: integer;
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
              Param:=Param+FileText[k];
              inc(k);
            until((k>=length(FileText))or(FileText[k]='!')or(FileText[k]=#32)); //Until we find another ! OR we run out of data
            //Convert to an integer, if possible
            if StrToIntDef(Param,-999) <> -999 then ParamList[l] := StrToInt(Param)
            else if l=1 then TextParam:=Param; //Accept text for first parameter

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


procedure TMissionParser.GetDetailsProcessCommand(CommandType: TKMCommandType; const ParamList: array of integer; TextParam:shortstring);
begin
  with fMissionInfo do
  case CommandType of
    ct_SetMap:         MapPath       := RemoveQuotes(TextParam);
    ct_SetMaxPlayer:   PlayerCount   := ParamList[0];
    ct_SetTactic:      MissionMode   := mm_Tactic;
    ct_SetHumanPlayer: HumanPlayerID := ParamList[0];
{                       if TGoalCondition(ParamList[0]) = gc_Time then
                         VictoryCond := VictoryCond + fPlayers.Player[fCurrentPlayerIndex].AddGoal(glt_Victory,TGoalCondition(ParamList[0]),TGoalStatus(ParamList[1]),ParamList[3],ParamList[2],play_none)
                       else
                         fPlayers.Player[fCurrentPlayerIndex].AddGoal(glt_Victory,TGoalCondition(ParamList[0]),TGoalStatus(ParamList[1]),0,ParamList[2],TPlayerID(ParamList[3]));
}
    ct_AddGoal:        VictoryCond   := VictoryCond
                                        + GoalConditionStr[TGoalCondition(ParamList[0])]
                                        + GoalStatusStr[TGoalStatus(ParamList[1])]+'|';
    ct_AddLostGoal:    DefeatCond    := DefeatCond
                                        + GoalConditionStr[TGoalCondition(ParamList[0])]
                                        + GoalStatusStr[TGoalStatus(ParamList[1])]+'|';
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
    DebugScriptError('MissionParser can''t open the map because it''s too big.');
    Result := false;
    Exit;
  end;
  
  fMissionInfo.MapSize.X := sx;
  fMissionInfo.MapSize.Y := sy;
  Result := true;
end;


function TMissionParser.LoadStandard(const aFileName:string):boolean;
var
  FileText, CommandText, Param, TextParam: AnsiString;
  ParamList: array[1..8] of integer;
  k, l: integer;
  CommandType: TKMCommandType;
begin
  Result := false; //Set it right from the start

  //Reset fPlayers and other stuff
  FreeAndNil(fPlayers);
  fCurrentPlayerIndex := 0;

  //Read the mission file into FileText
  FileText := ReadMissionFile(aFileName);
  if FileText = '' then exit;

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
          if StrToIntDef(Param,-999) <> -999 then ParamList[l] := StrToInt(Param)
          else if l=1 then TextParam:=Param; //Accept text for first parameter

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
    DebugScriptError('No human player detected - ''ct_SetHumanPlayer''');
    Exit;
  end;

  Result := true; //If we have reach here without exiting then it must have worked
end;


function TMissionParser.ProcessCommand(CommandType: TKMCommandType; ParamList: array of integer; TextParam:shortString):boolean;
var
  MyStr: string;
  i, MyInt: integer;
  Storehouse: TKMHouseStore;
  Barracks: TKMHouseBarracks;
  iPlayerAI: TKMPlayerAI;
begin
  Result := false; //Set it right from the start. There are several Exit points below

  case CommandType of
    ct_SetMap:         begin
                         MyStr := RemoveQuotes(TextParam);
                         //Check for same filename.map in same folder first - Remake format
                         if CheckFileExists(ChangeFileExt(fMissionFileName,'.map'),true) then
                           fTerrain.LoadFromFile(ChangeFileExt(fMissionFileName,'.map'))
                         else
                         //Check for KaM format map path
                         if CheckFileExists(ExeDir+MyStr,true) then
                           fTerrain.LoadFromFile(ExeDir+MyStr)
                         else
                         begin
                           //Else abort loading and fail
                           DebugScriptError('Map file couldn''t be found');
                           Exit;
                         end;
                       end;
    ct_SetMaxPlayer:   begin
                         if fPlayers=nil then fPlayers := TKMPlayersCollection.Create;
                         fPlayers.AddPlayers(ParamList[0]);
                       end;
    ct_SetTactic:      begin
                         if fPlayers=nil then fPlayers := TKMPlayersCollection.Create;
                         fMissionInfo.MissionMode := mm_Tactic;
                       end;
    ct_SetCurrPlayer:  begin
                       if InRange(ParamList[0], 0, fPlayers.Count-1) then
                       begin
                         fCurrentPlayerIndex := ParamList[0];
                         fLastHouse := nil;
                         fLastTroop := nil;
                       end;
                       end;
    ct_SetHumanPlayer: begin
                       if (fPlayers <> nil) and (fParsingMode <> mpm_Multi) then
                         if InRange(ParamList[0],0,fPlayers.Count-1) then
                         begin
                           fMissionInfo.HumanPlayerID := ParamList[0];
                           fPlayers.Player[ParamList[0]].PlayerType := pt_Human;
                         end;
                       end;
    ct_AIPlayer:       begin
                       if (fPlayers <> nil) and (fParsingMode <> mpm_Multi) then
                         if InRange(ParamList[0],0,fPlayers.Count-1) then
                           fPlayers.Player[ParamList[0]].PlayerType:=pt_Computer
                         else //This command doesn't require an ID, just use the current player
                           fPlayers.Player[fCurrentPlayerIndex].PlayerType:=pt_Computer;
                       end;
    ct_CenterScreen:   begin
                         fPlayers.Player[fCurrentPlayerIndex].CenterScreen := KMPointX1Y1(ParamList[0],ParamList[1]);
                       end;
    ct_ClearUp:        begin
                       if ParamList[0] = 255 then
                         fPlayers.Player[fCurrentPlayerIndex].FogOfWar.RevealEverything
                       else
                         fPlayers.Player[fCurrentPlayerIndex].FogOfWar.RevealCircle(KMPointX1Y1(ParamList[0],ParamList[1]), ParamList[2], 255);
                       end;
    ct_SetHouse:       begin
                       if InRange(ParamList[0],0,HOUSE_COUNT-1) then
                         fLastHouse := fPlayers.Player[fCurrentPlayerIndex].AddHouse(THouseType(ParamList[0]+1), ParamList[1]+1, ParamList[2]+1, false);
                       end;
    ct_SetHouseDamage: begin
                       if fLastHouse <> nil then
                         fLastHouse.AddDamage(ParamList[0])
                       else
                         DebugScriptError('ct_SetHouseDamage without prior declaration of House');
                       end;
    ct_SetUnit:        begin
                       if InRange(ParamList[0],0,31) then
                         fPlayers.Player[fCurrentPlayerIndex].AddUnit(UnitsRemap[ParamList[0]],KMPointX1Y1(ParamList[1],ParamList[2]));
                       end;
    ct_SetUnitByStock: begin
                       if InRange(ParamList[0],0,31) then
                       begin
                         Storehouse:=TKMHouseStore(fPlayers.Player[fCurrentPlayerIndex].FindHouse(ht_Store,1));
                         if Storehouse<>nil then
                           fPlayers.Player[fCurrentPlayerIndex].AddUnit(UnitsRemap[ParamList[0]],KMPointY1(Storehouse.GetEntrance));
                       end;
                       end;
    ct_SetRoad:        begin
                         fPlayers.Player[fCurrentPlayerIndex].AddRoadsToList(KMPointX1Y1(ParamList[0],ParamList[1]));
                       end;
    ct_SetField:       begin
                         fPlayers.Player[fCurrentPlayerIndex].AddField(KMPointX1Y1(ParamList[0],ParamList[1]),ft_Corn);
                       end;
    ct_Set_Winefield:  begin
                         fPlayers.Player[fCurrentPlayerIndex].AddField(KMPointX1Y1(ParamList[0],ParamList[1]),ft_Wine);
                       end;
    ct_SetStock:       begin //This command basically means: Put a storehouse here with road bellow it
                         fLastHouse := fPlayers.Player[fCurrentPlayerIndex].AddHouse(ht_Store, ParamList[0]+1,ParamList[1]+1, false);
                         fPlayers.Player[fCurrentPlayerIndex].AddRoadsToList(KMPointX1Y1(ParamList[0],ParamList[1]+1));
                         fPlayers.Player[fCurrentPlayerIndex].AddRoadsToList(KMPointX1Y1(ParamList[0]-1,ParamList[1]+1));
                         fPlayers.Player[fCurrentPlayerIndex].AddRoadsToList(KMPointX1Y1(ParamList[0]-2,ParamList[1]+1));
                       end;
    ct_AddWare:        begin
                         MyInt:=ParamList[1];
                         if MyInt = -1 then MyInt:=High(Word); //-1 means maximum resources
                         MyInt:=EnsureRange(MyInt,0,High(Word)); //Sometimes user can define it to be 999999
                         Storehouse:=TKMHouseStore(fPlayers.Player[fCurrentPlayerIndex].FindHouse(ht_Store,1));
                         if (Storehouse<>nil) and (InRange(ParamList[0]+1,1,28)) then Storehouse.AddMultiResource(TResourceType(ParamList[0]+1),MyInt);
                       end;
    ct_AddWareToAll:   begin
                         MyInt:=ParamList[1];
                         if MyInt = -1 then MyInt:=High(Word); //-1 means maximum resources
                         for i:=0 to fPlayers.Count-1 do
                         begin
                           Storehouse:=TKMHouseStore(fPlayers.Player[i].FindHouse(ht_Store,1));
                           if (Storehouse<>nil) and (InRange(ParamList[0]+1,1,28)) then Storehouse.AddMultiResource(TResourceType(ParamList[0]+1),MyInt);
                         end;
                       end;
    ct_AddWareToSecond:begin
                         MyInt:=ParamList[1];
                         if MyInt = -1 then MyInt:=High(Word); //-1 means maximum resources
                         Storehouse:=TKMHouseStore(fPlayers.Player[fCurrentPlayerIndex].FindHouse(ht_Store,2));
                         if (Storehouse<>nil) and (InRange(ParamList[0]+1,1,28)) then Storehouse.AddMultiResource(TResourceType(ParamList[0]+1),MyInt);
                       end;
    ct_AddWeapon:      begin
                         MyInt:=ParamList[1];
                         if MyInt = -1 then MyInt:=High(Word); //-1 means maximum weapons
                         Barracks:=TKMHouseBarracks(fPlayers.Player[fCurrentPlayerIndex].FindHouse(ht_Barracks,1));
                         if (Barracks<>nil) and (InRange(ParamList[0]+1,17,27)) then Barracks.AddMultiResource(TResourceType(ParamList[0]+1),MyInt);
                       end;
    ct_BlockHouse:     begin
                         if InRange(ParamList[0],0,HOUSE_COUNT-1) then
                           fPlayers.Player[fCurrentPlayerIndex].Stats.AllowToBuild[ParamList[0]+1]:=false;
                       end;
    ct_ReleaseHouse:   begin
                         if InRange(ParamList[0],0,HOUSE_COUNT-1) then
                           fPlayers.Player[fCurrentPlayerIndex].Stats.BuildReqDone[ParamList[0]+1]:=true;
                       end;
   ct_ReleaseAllHouses:begin
                         for i:=1 to HOUSE_COUNT do
                           fPlayers.Player[fCurrentPlayerIndex].Stats.BuildReqDone[i]:=true;
                       end;
    ct_SetGroup:       begin
                         if InRange(ParamList[0],14,23) then //Needs changing to 29 once TPR troops are supported
                                                             //@Lewin: We need a sort of UnitIsArmy procedure somewhere
                                                             //cos atm there are too many places where values input by hand
                                                             //and if we to add e.g. new unit we'll need to fix all those manualy
                           fLastTroop := TKMUnitWarrior(fPlayers.Player[fCurrentPlayerIndex].AddGroup(
                             TroopsRemap[ParamList[0]],
                             KMPointX1Y1(ParamList[1],ParamList[2]),
                             TKMDirection(ParamList[3]+1),
                             ParamList[4],
                             ParamList[5],
                             fParsingMode=mpm_Editor //Editor mode = true
                             ));
                       end;
    ct_SendGroup:      begin
                         if fLastTroop <> nil then
                           fLastTroop.OrderWalk(KMPointDir(KMPointX1Y1(ParamList[0],ParamList[1]),ParamList[2]))
                         else
                           DebugScriptError('ct_SendGroup without prior declaration of Troop');
                       end;
    ct_SetGroupFood:   begin
                         if fLastTroop <> nil then
                           fLastTroop.SetGroupFullCondition
                         else
                           DebugScriptError('ct_SetGroupFood without prior declaration of Troop');
                       end;
    ct_AICharacter:    begin
                         if fPlayers.Player[fCurrentPlayerIndex].PlayerType <> pt_Computer then exit;
                         iPlayerAI := fPlayers.Player[fCurrentPlayerIndex].AI; //Setup the AI's character
                         if TextParam = PARAMVALUES[cpt_Recruits]     then iPlayerAI.ReqRecruits         := ParamList[1];
                         if TextParam = PARAMVALUES[cpt_Constructors] then iPlayerAI.ReqWorkers          := ParamList[1];
                         if TextParam = PARAMVALUES[cpt_WorkerFactor] then iPlayerAI.ReqSerfFactor       := ParamList[1];
                         if TextParam = PARAMVALUES[cpt_RecruitCount] then iPlayerAI.RecruitTrainTimeout := ParamList[1];
                         if TextParam = PARAMVALUES[cpt_TownDefence]  then iPlayerAI.TownDefence         := ParamList[1];
                         if TextParam = PARAMVALUES[cpt_MaxSoldier]   then iPlayerAI.MaxSoldiers         := ParamList[1];
                         if TextParam = PARAMVALUES[cpt_AttackFactor] then iPlayerAI.Aggressiveness      := ParamList[1];
                         if TextParam = PARAMVALUES[cpt_TroopParam]   then
                         begin
                           iPlayerAI.TroopFormations[TGroupType(ParamList[1]+1)].NumUnits := ParamList[2];
                           iPlayerAI.TroopFormations[TGroupType(ParamList[1]+1)].UnitsPerRow  := ParamList[3];
                         end;
                       end;
    ct_AINoBuild:      begin
                         fPlayers.Player[fCurrentPlayerIndex].AI.Autobuild := false;
                       end;
    ct_AIStartPosition:begin
                         fPlayers.Player[fCurrentPlayerIndex].AI.StartPosition := KMPointX1Y1(ParamList[0],ParamList[1]);
                       end;
    ct_SetAlliance:    begin
                         if ParamList[1] = 1 then
                           fPlayers.Player[fCurrentPlayerIndex].Alliances[ParamList[0]] := at_Ally
                         else
                           fPlayers.Player[fCurrentPlayerIndex].Alliances[ParamList[0]] := at_Enemy;
                       end;
    ct_AttackPosition: begin
                         //If target is building: Attack building
                         //If target is unit: Chase/attack unit
                         //If target is nothing: move to position
                         //However, because the unit/house target may not have been created yet, this must be processed after everything else
                         if fLastTroop <> nil then
                         begin
                           inc(fAttackPositionsCount);
                           SetLength(fAttackPositions, fAttackPositionsCount+1);
                           fAttackPositions[fAttackPositionsCount-1].Warrior := fLastTroop;
                           fAttackPositions[fAttackPositionsCount-1].Target := KMPointX1Y1(ParamList[0],ParamList[1]);
                         end
                         else
                           DebugScriptError('ct_AttackPosition without prior declaration of Troop');
                       end;
    ct_AddGoal:        begin
                         //If the condition is time then ParamList[3] is the time, else it is player ID
                         if TGoalCondition(ParamList[0]) = gc_Time then
                           fPlayers.Player[fCurrentPlayerIndex].Goals.AddGoal(glt_Victory,TGoalCondition(ParamList[0]),TGoalStatus(ParamList[1]),ParamList[3],ParamList[2],-1)
                         else begin
                           if ParamList[3] > fPlayers.Count-1 then begin
                             DebugScriptError('Add_Goal for non existing player');
                             exit;
                           end;
                           fPlayers.Player[fCurrentPlayerIndex].Goals.AddGoal(glt_Victory,TGoalCondition(ParamList[0]),TGoalStatus(ParamList[1]),0,ParamList[2],ParamList[3]);
                         end;
                       end;
    ct_AddLostGoal:    begin
                         //If the condition is time then ParamList[3] is the time, else it is player ID
                         if TGoalCondition(ParamList[0]) = gc_Time then
                           fPlayers.Player[fCurrentPlayerIndex].Goals.AddGoal(glt_Survive,TGoalCondition(ParamList[0]),TGoalStatus(ParamList[1]),ParamList[3],ParamList[2],-1)
                         else begin
                           if ParamList[3] > fPlayers.Count-1 then begin
                             DebugScriptError('Add_LostGoal for non existing player');
                             exit;
                           end;
                           fPlayers.Player[fCurrentPlayerIndex].Goals.AddGoal(glt_Survive,TGoalCondition(ParamList[0]),TGoalStatus(ParamList[1]),0,ParamList[2],ParamList[3]);
                         end;
                       end;
    ct_AIDefence:      begin
                         fPlayers.Player[fCurrentPlayerIndex].AI.AddDefencePosition(KMPointDir(KMPointX1Y1(ParamList[0],ParamList[1]),ParamList[2]),TGroupType(ParamList[3]+1),ParamList[4],TAIDefencePosType(ParamList[5]));
                       end;
    ct_SetMapColor:    begin
                         //For now simply use the minimap color for all color, it is too hard to load all 8 shades from ct_SetNewRemap
                         fPlayers.Player[fCurrentPlayerIndex].FlagColor := fResource.GetColor32(ParamList[0], DEF_PAL);
                       end;
    ct_AIAttack:       begin
                         //Set up the attack command
                         if TextParam = PARAMVALUES[cpt_Type] then
                           if InRange(ParamList[1],0,2) then
                             fAIAttack.AttackType := RemakeAttackType[ParamList[1]]
                           else
                             DebugScriptError('Unknown parameter '+inttostr(ParamList[1])+' at ct_AIAttack');
                         if TextParam = PARAMVALUES[cpt_TotalAmount] then
                           fAIAttack.TotalMen := ParamList[1];
                         if TextParam = PARAMVALUES[cpt_Counter] then
                           fAIAttack.Delay := ParamList[1];
                         if TextParam = PARAMVALUES[cpt_Range] then
                           fAIAttack.Range := ParamList[1];
                         if TextParam = PARAMVALUES[cpt_TroopAmount] then
                           fAIAttack.GroupAmounts[TGroupType(ParamList[1]+1)] := ParamList[2];
                         if TextParam = PARAMVALUES[cpt_Target] then
                           fAIAttack.Target := TAIAttackTarget(ParamList[1]);
                         if TextParam = PARAMVALUES[cpt_Position] then
                           fAIAttack.CustomPosition := KMPointX1Y1(ParamList[1],ParamList[2]);
                         if TextParam = PARAMVALUES[cpt_TakeAll] then
                           fAIAttack.TakeAll := true;
                       end;
    ct_CopyAIAttack:   begin
                         //Save the attack to the AI assets
                         fPlayers.Player[fCurrentPlayerIndex].AI.AddAttack(fAIAttack);
                       end;
    ct_EnablePlayer:   begin
                         //Serves no real purpose, all players have this command anyway
                       end;
    ct_SetNewRemap:    begin
                         //Disused. Minimap color is used for all colors now. However it might be better to use these values in the long run as sometimes the minimap colors do not match well
                       end;
  end;
  Result := true; //Must have worked if we haven't exited by now
end;


//A nice way of debugging script errors.
//Shows the error to the user so they know exactly what they did wrong.
procedure TMissionParser.DebugScriptError(const ErrorMsg:string);
begin
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


function TMissionParser.UnitTypeToScriptID(aUnitType:TUnitType):integer;
var i:integer;
begin
  Result := -1;
  for i:=low(UnitsRemap) to high(UnitsRemap) do
    if UnitsRemap[i] = aUnitType then
    begin
      Result := i;
      exit;
    end;
end;


//Write out a KaM format mission file to aFileName
function TMissionParser.SaveDATFile(const aFileName:string):boolean;
const
  COMMANDLAYERS = 4;
var
  f:textfile;
  i: longint; //longint because it is used for encoding entire output, which will limit the file size
  k,iX,iY,CommandLayerCount,StoreCount,BarracksCount: integer;
  Res:TResourceType;
  Group: TGroupType;
  CurUnit: TKMUnit;
  CurHouse: TKMHouse;
  ReleaseAllHouses: boolean;
  SaveString: string;

  procedure AddData(aText:shortstring);
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

  procedure AddCommand(aCommand:TKMCommandType; aComParam:TKMCommandParamType; aParams:array of integer); overload;
  var OutData: string; i:integer;
  begin
    OutData := '!' + COMMANDVALUES[aCommand];

    if aComParam <> cpt_Unknown then
      OutData := OutData + ' ' + PARAMVALUES[aComParam];

    for i:=Low(aParams) to High(aParams) do
      OutData := OutData + ' ' + IntToStr(aParams[i]);

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
  AddData('!'+COMMANDVALUES[ct_SetMap] + ' "data\mission\smaps\' + ExtractFileName(TruncateExt(aFileName)) + '.map"');
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
      AddCommand(ct_AIStartPosition, [fPlayers.Player[i].AI.StartPosition.X,fPlayers.Player[i].AI.StartPosition.Y]);
      if not fPlayers.Player[i].AI.Autobuild then
        AddCommand(ct_AINoBuild, []);
      AddCommand(ct_AICharacter,cpt_Recruits, [fPlayers.Player[i].AI.ReqRecruits]);
      AddCommand(ct_AICharacter,cpt_WorkerFactor, [fPlayers.Player[i].AI.ReqSerfFactor]);
      AddCommand(ct_AICharacter,cpt_Constructors, [fPlayers.Player[i].AI.ReqWorkers]);
      AddCommand(ct_AICharacter,cpt_TownDefence, [fPlayers.Player[i].AI.TownDefence]);
      //Only store if a limit is in place (high is the default)
      if fPlayers.Player[i].AI.MaxSoldiers <> high(fPlayers.Player[i].AI.MaxSoldiers) then
        AddCommand(ct_AICharacter,cpt_MaxSoldier, [fPlayers.Player[i].AI.MaxSoldiers]);
      AddCommand(ct_AICharacter,cpt_AttackFactor, [fPlayers.Player[i].AI.Aggressiveness]);
      AddCommand(ct_AICharacter,cpt_RecruitCount, [fPlayers.Player[i].AI.RecruitTrainTimeout]);
      for Group:=low(TGroupType) to high(TGroupType) do
        if (Group <> gt_None) and (fPlayers.Player[i].AI.TroopFormations[Group].NumUnits <> 0) then //Must be valid and used
          AddCommand(ct_AICharacter,cpt_TroopParam, [byte(Group)-1,fPlayers.Player[i].AI.TroopFormations[Group].NumUnits,fPlayers.Player[i].AI.TroopFormations[Group].UnitsPerRow]);
      AddData(''); //NL
      for k:=0 to fPlayers.Player[i].AI.DefencePositionsCount-1 do
        with fPlayers.Player[i].AI.DefencePositions[k] do
          AddCommand(ct_AIDefence, [Position.Loc.X-1,Position.Loc.Y-1,Position.Dir,byte(GroupType)-1,DefenceRadius,byte(DefenceType)]);
      AddData(''); //NL
      AddData(''); //NL
      for k:=0 to fPlayers.Player[i].AI.ScriptedAttacksCount-1 do
        with fPlayers.Player[i].AI.ScriptedAttacks[k] do
        begin
          AddCommand(ct_AIAttack,cpt_Type, [byte(AttackType)]);
          AddCommand(ct_AIAttack,cpt_TotalAmount, [TotalMen]);
          if TakeAll then
            AddCommand(ct_AIAttack,cpt_TakeAll, [])
          else
            for Group:=low(TGroupType) to high(TGroupType) do
              if Group <> gt_None then
                AddCommand(ct_AIAttack,cpt_TroopAmount, [byte(Group)-1, GroupAmounts[Group]]);

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
        AddCommand(ct_SetAlliance, [k,byte(fPlayers.Player[i].Alliances[k])]); //0=enemy, 1=ally
    AddData(''); //NL
    
    //Release/block houses
    ReleaseAllHouses := true;
    for k:=1 to HOUSE_COUNT do
    begin
      if not fPlayers.Player[i].Stats.AllowToBuild[k] then
      begin
        AddCommand(ct_BlockHouse, [k-1]);
        ReleaseAllHouses := false;
      end
      else
        if fPlayers.Player[i].Stats.BuildReqDone[k] then
          AddCommand(ct_ReleaseHouse, [k-1])
        else
          ReleaseAllHouses := false;
    end;
    if ReleaseAllHouses then
      AddCommand(ct_ReleaseAllHouses, []);

    //Houses
    for k:=0 to fPlayers.Player[i].Houses.Count-1 do
    begin
      CurHouse := TKMHouse(fPlayers.Player[i].Houses.Items[k]);
      if not CurHouse.IsDestroyed then
      begin
        AddCommand(ct_SetHouse, [byte(CurHouse.GetHouseType)-1,CurHouse.GetPosition.X-1,CurHouse.GetPosition.Y-1]);
        if CurHouse.IsDamaged then
          AddCommand(ct_SetHouseDamage, [CurHouse.GetDamage]);
      end;
    end;
    AddData(''); //NL

    //Wares. Check every house to see if it's a store or barracks
    StoreCount := 0;
    BarracksCount := 0;
    for k:=0 to fPlayers.Player[i].Houses.Count-1 do
    begin
      CurHouse := TKMHouse(fPlayers.Player[i].Houses.Items[k]);
      if not CurHouse.IsDestroyed then
      begin
        if CurHouse is TKMHouseStore then
        begin
          inc(StoreCount);
          if StoreCount <= 2 then //For now only handle 2 storehouses, we can add a new command later
            for Res:=rt_Trunk to rt_Fish do
              if TKMHouseStore(CurHouse).CheckResIn(Res) > 0 then
                if StoreCount = 1 then
                  AddCommand(ct_AddWare, [byte(Res)-1,TKMHouseStore(CurHouse).CheckResIn(Res)]) //Ware, Count
                else
                  AddCommand(ct_AddWareToSecond, [byte(Res)-1,TKMHouseStore(CurHouse).CheckResIn(Res)]); //Ware, Count
        end;
        if CurHouse is TKMHouseBarracks then
        begin
          inc(BarracksCount);
          if BarracksCount <= 1 then //For now only handle 1 barracks, we can add a new command later
            for Res:=rt_Shield to rt_Horse do
              if TKMHouseBarracks(CurHouse).CheckResIn(Res) > 0 then
                AddCommand(ct_AddWeapon, [byte(Res)-1,TKMHouseBarracks(CurHouse).CheckResIn(Res)]); //Ware, Count
        end;
      end;
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
      CurUnit := TKMUnit(fPlayers.Player[i].Units.Items[k]);
      if CurUnit is TKMUnitWarrior then
      begin
        if TKMUnitWarrior(CurUnit).IsCommander then //Parse only Commanders
        begin
          AddCommand(ct_SetGroup, [UnitTypeToScriptID(CurUnit.UnitType),CurUnit.GetPosition.X-1,CurUnit.GetPosition.Y-1,byte(CurUnit.Direction)-1,TKMUnitWarrior(CurUnit).UnitsPerRow,TKMUnitWarrior(CurUnit).fMapEdMembersCount+1]);
          if CurUnit.Condition = UNIT_MAX_CONDITION then
            AddCommand(ct_SetGroupFood, []);
        end;
      end
      else
        AddCommand(ct_SetUnit, [UnitTypeToScriptID(CurUnit.UnitType),CurUnit.GetPosition.X-1,CurUnit.GetPosition.Y-1]);
    end;

    AddData(''); //NL
    AddData(''); //NL
  end; //Player loop

  //Main footer

  //Animals, wares to all, etc. go here
  AddData('//Animals');
  for i:=0 to fPlayers.PlayerAnimals.Units.Count-1 do
  begin
    CurUnit := fPlayers.PlayerAnimals.Units.Items[i];
    AddCommand(ct_SetUnit, [UnitTypeToScriptID(CurUnit.UnitType),CurUnit.GetPosition.X-1,CurUnit.GetPosition.Y-1]);
  end;
  AddData(''); //NL

  //Similar footer to one in Lewin's Editor, useful so ppl know what mission was made with.
  AddData('//This mission was made with KaM Remake Map Editor version '+GAME_VERSION+' at '+ShortString(DateTimeToStr(Now)));

  //Write uncoded file for debug
  assignfile(f, aFileName+'.txt'); rewrite(f);
  write(f, SaveString);
  closefile(f);

  //Encode it
  for i:=1 to length(SaveString) do
    SaveString[i]:=chr(byte(SaveString[i]) xor 239);
  //Write it
  assignfile(f, aFileName); rewrite(f);
  write(f, SaveString);
  closefile(f);

  Result := true; //Success
end;


end.


