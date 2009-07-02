unit KM_LoadDAT;
interface
uses
  Windows, Classes, KromUtils, SysUtils, StrUtils, Dialogs, Math, KM_Defaults, KM_Houses;

type
  TKMCommandType = (ct_Unknown=0,ct_SetMap,ct_SetMaxPlayer,ct_SetCurrPlayer,ct_SetHumanPlayer,ct_SetHouse,
                    ct_SetTactic,ct_AIPlayer,ct_EnablePlayer,ct_SetNewRemap,ct_SetMapColor,ct_CenterScreen,
                    ct_ClearUp,ct_BlockHouse,ct_ReleaseHouse,ct_ReleaseAllHouses,ct_AddGoal,ct_AddLostGoal,
                    ct_SetUnit,ct_SetRoad,ct_SetField,ct_Set_Winefield,ct_SetStock,ct_AddWare,ct_SetAlliance,
                    ct_SetHouseDamage,ct_SetUnitByStock,ct_SetGroup,ct_SetGroupFood,ct_SendGroup,
                    ct_AttackPosition,ct_AddWareToSecond,ct_AddWareToAll,ct_AddWeapon);

  TKMMissionDetails = record
    MapPath: string;
    IsFight: boolean;
    TeamCount, HumanPlayerID: shortint;
  end;
  TKMMapDetails = record
    MapSize: TKMPoint;
  end;

  TKMCommandTypeSet = set of TKMCommandType;

const
  COMMANDVALUES: array[TKMCommandType] of string = (
    '','SET_MAP','SET_MAX_PLAYER','SET_CURR_PLAYER','SET_HUMAN_PLAYER','SET_HOUSE',
    'SET_TACTIC','SET_AI_PLAYER','ENABLE_PLAYER','SET_NEW_REMAP','SET_MAP_COLOR',
    'CENTER_SCREEN','CLEAR_UP','BLOCK_HOUSE','RELEASE_HOUSE','RELEASE_ALL_HOUSES',
    'ADD_GOAL','ADD_LOST_GOAL','SET_UNIT','SET_STREET','SET_FIELD','SET_WINEFIELD',
    'SET_STOCK','ADD_WARE','SET_ALLIANCE','SET_HOUSE_DAMAGE','SET_UNIT_BY_STOCK',
    'SET_GROUP','SET_GROUP_FOOD','SEND_GROUP','ATTACK_POSITION','ADD_WARE_TO_SECOND',
    'ADD_WARE_TO_ALL','ADD_WEAPON');
    //SET_AI_NO_BUILD, SET_AI_START_POSITION, SET_AI_CHARACTER, SET_AI_DEFENSE, ...

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

  CommandsUsedInGetMissionDetails: TKMCommandTypeSet = [ct_SetMap,ct_SetMaxPlayer,ct_SetHumanPlayer,ct_SetTactic];

type
  TMissionParser = class(TObject)
  private     { Private declarations }
    OpenedMissionName:string;
    CurrentPlayerIndex: integer;
    LastHouse: TKMHouse;
    function ProcessCommand(CommandType: TKMCommandType; ParamList: array of integer; TextParam:string):boolean;
    procedure GetDetailsProcessCommand(CommandType: TKMCommandType; ParamList: array of integer; TextParam:string; var MissionDetails: TKMMissionDetails);
    procedure DebugScriptError(ErrorMsg:string);
    procedure UnloadMission;
    function ReadMissionFile(aFileName:string):string;
  public      { Public declarations }
    constructor Create;
    function LoadDATFile(aFileName:string):boolean;
    function GetMissionDetails(aFileName:string):TKMMissionDetails;
    function GetMapDetails(aFileName:string):TKMMapDetails;
end;

type
  TKMMapsInfo = class(TObject)
  private
    MapCount:byte;
    Maps:array[1..255]of record
      Folder:string; //Map folder
      IsFight:boolean; //Fight or Build map
      PlayerCount:byte;
      Title,SmallDesc,BigDesc:string;
      MapSize:string; //S,M,L,XL
    end;
  public
    procedure ScanSingleMapsFolder(Path:string);
    property GetMapCount:byte read MapCount;
    function IsFight(ID:integer):boolean;
    function GetPlayerCount(ID:integer):byte;
    function GetTitle(ID:integer):string;
    function GetSmallDesc(ID:integer):string;
    function GetBigDesc(ID:integer):string;
    function GetMapSize(ID:integer):string;
    function GetFolder(ID:integer):string;
    function GetMissionFile(ID:integer):string;
    function GetTyp(ID:integer):string;
    function GetWin(ID:integer):string;
    function GetDefeat(ID:integer):string;
  end;

var
  fMissionParser: TMissionParser;

implementation
uses KM_Users, KM_Terrain, KM_Viewport;

function GetCommandTypeFromText(ACommandText: string): TKMCommandType;
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

constructor TMissionParser.Create;
begin
  inherited Create;
end;

procedure TMissionParser.UnloadMission;
begin
  FreeAndNil(fPlayers);
  CurrentPlayerIndex := 0;
  MissionMode:=mm_Normal;
end;

function TMissionParser.ReadMissionFile(aFileName:string):string;
const ENCODED = true; //If false then files will be opened as text
var
  FileText: string;
  i, k, FileSize: integer;
  f: file;
  c:array of char;
begin
  if not CheckFileExists(aFileName) then exit;

  //Load and decode .DAT file into FileText
  SetLength(c,1024000);
  assignfile(f,aFileName); reset(f,1);
  blockread(f,c[1],length(c),FileSize);
  Assert(FileSize<>length(c),'DAT file size is too big, can''t fit into buffer');
  setlength(FileText,FileSize);
  closefile(f);

  //@Lewin: ENCODED := c[1] = chr(206); //That is encoded first char
  //@Krom: We can't use that method. Some mission editors (e.g. Thunderwolf's) put a comment at the
  //top of the file stating that it was made with his editor. And we don't want to force people to
  //start with a command. I vote we use: If file text contains !SET_MAX_PLAYERS then it is real.
  //Because every mission must have that command. That's what I'm using for my mission editor, and
  //it works well. (1 string search won't take long, will it?)
  //@Lewin: Okay, then we could grab 100bytes from the middle and estimate if they are in a..Z range or XORed.
  //@Krom: Yes, that sounds ok to me. I'll add it at some point, but it's low prioraty IMO, I'd rather
  //get the game engine working. If you think it's urgent then feel free to write it yourself.

  i:=1; k:=1;
  repeat
    if ENCODED then
      FileText[k]:=chr(ord(c[i]) xor 239)
    else
      FileText[k]:=c[i];
    if (FileText[k]=#9)or(FileText[k]=#10)or(FileText[k]=#13) then FileText[k]:=' ';
    inc(i);
    if (k>1)and(((FileText[k-1]=#32)and(FileText[k]=#32))or((FileText[k-1]='!')and(FileText[k]='!'))) then else
    inc(k);
  until(i>FileSize);
  setlength(FileText,k); //Because some extra characters are removed
  SetLength(c,0); //Clear the buffer to save RAM
  Result := FileText;
end;


{Acquire specific map details in a fast way}
function TMissionParser.GetMissionDetails(aFileName:string):TKMMissionDetails;
const
  Max_Cmd=1;
var
  FileText, CommandText, Param, TextParam: string;
  ParamList: array[1..Max_Cmd] of integer;
  k, l: integer;
  CommandType: TKMCommandType;
begin
  //Set default values
  Result.MapPath := '';
  Result.IsFight := false;
  Result.TeamCount := 0;
  Result.HumanPlayerID := 0;

  FileText := ReadMissionFile(aFileName);
  if FileText = '' then exit;

  //We need only these 4 commands
  //!SET_MAP, !SET_MAX_PLAYER, !SET_TACTIC, !SET_HUMAN_PLAYER

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
      if (CommandText='SET_MAP')or(CommandText='SET_MAX_PLAYER')or
         (CommandText='SET_TACTIC')or(CommandText='SET_HUMAN_PLAYER') then
      begin
        //Now convert command into type
        CommandType := GetCommandTypeFromText(CommandText);
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
        GetDetailsProcessCommand(CommandType,ParamList,TextParam,Result);
      end;
    end
    else
      inc(k);
  until ((k>=length(FileText))
  //or((Result.MapPath<>'')and(Result.IsFight>=0)and(Result.TeamCount>=0)and(Result.HumanPlayerID>=0)) //Appeared it's more of a slowdown or no effect
  );
end;

procedure TMissionParser.GetDetailsProcessCommand(CommandType: TKMCommandType; ParamList: array of integer; TextParam:string; var MissionDetails: TKMMissionDetails);
begin
  case CommandType of
  ct_SetMap:         MissionDetails.MapPath     := RemoveQuotes(TextParam);
  ct_SetMaxPlayer:   MissionDetails.TeamCount   := ParamList[0];
  ct_SetTactic:      MissionDetails.IsFight     := true;
  ct_SetHumanPlayer: MissionDetails.HumanPlayerID := ParamList[0]+1;
  end;
end;


{Acquire specific map details in a fast way}
function TMissionParser.GetMapDetails(aFileName:string):TKMMapDetails;
var f:file; i,k:integer;
begin
  assignfile(f,aFileName); reset(f,1);
  blockread(f,k,4);
  blockread(f,i,4);
  Assert((k<=MaxMapSize)and(i<=MaxMapSize),'TMissionParser.GetMapDetails - Can''t open the map cos it''s too big.');
  closefile(f);
  Result.MapSize.X:=k;
  Result.MapSize.Y:=i;
end;


function TMissionParser.LoadDATFile(aFileName:string):boolean;
var
  FileText, CommandText, Param, TextParam: string;
  ParamList: array[1..8] of integer;
  k, l: integer;
  CommandType: TKMCommandType;
begin
  Result:=false; //Set it right from the start
  UnloadMission; //Call function which will reset fPlayers and other stuff

  OpenedMissionName:=aFileName; //Used in MAP loading later on

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
      CommandType := GetCommandTypeFromText(CommandText);
      inc(k);
      //Extract parameters
      for l:=1 to 8 do
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

      if ProcessCommand(CommandType,ParamList,TextParam) = false then //A returned value of false indicates an error has occoured and we should exit
      begin
        //Result:=false;
        exit;
      end;
    end
    else
      inc(k);
  until (k>=length(FileText));
  Result:=true; //If we have reach here without exiting then it must have worked
end;

function TMissionParser.ProcessCommand(CommandType: TKMCommandType; ParamList: array of integer; TextParam:string):boolean;
var
  MyStr: string;
  i, MyInt: integer;
  Storehouse:TKMHouseStore; Barracks: TKMHouseBarracks;
begin
  Result:=false; //Set it right from the start
  case CommandType of
  ct_SetMap:         begin
                       MyStr := RemoveQuotes(TextParam);
                       //Check for same filename.map in same folder first - Remake format
                       if CheckFileExists(ChangeFileExt(OpenedMissionName,'.map'),true) then
                         fTerrain.OpenMapFromFile(ChangeFileExt(OpenedMissionName,'.map'))
                       else
                       //Check for KaM format map path
                       if CheckFileExists(ExeDir+MyStr,true) then
                         fTerrain.OpenMapFromFile(ExeDir+MyStr)
                       else
                       begin
                         //Else abort loading and fail
                         DebugScriptError('Map file couldn''t be found');
                         exit;
                         end;
                     end;
  ct_SetMaxPlayer:   begin
                     fPlayers:=TKMAllPlayers.Create(ParamList[0]); //Create players
                     end;    
  ct_SetTactic:      begin
                       MissionMode:=mm_Tactic;
                     end;
  ct_SetCurrPlayer:  begin
                     if InRange(ParamList[0],0,fPlayers.PlayerCount-1) then
                       CurrentPlayerIndex := ParamList[0]+1; //+1 because in DAT players IDs are 0 based, but here they are 1 based
                     end;
  ct_SetHumanPlayer: begin
                     if fPlayers <> nil then
                       if InRange(ParamList[0],0,fPlayers.PlayerCount-1) then
                       begin
                         MyPlayer := fPlayers.Player[ParamList[0]+1];
                         MyPlayer.PlayerType:=pt_Human;
                       end;
                     end;
  ct_AIPlayer:       begin
                     if fPlayers <> nil then
                       if InRange(ParamList[0],0,fPlayers.PlayerCount-1) then
                         fPlayers.Player[ParamList[0]+1].PlayerType:=pt_Computer
                       else //This command doesn't require an ID, just use the current player
                         fPlayers.Player[CurrentPlayerIndex].PlayerType:=pt_Computer;
                     end;
  ct_CenterScreen:   begin
                     fViewPort.SetCenter(ParamList[0],ParamList[1]);
                     //@Krom: This now seems to sometimes not work. (screen is centred top left) Any idea why?
                     //@Lewin: I've no idea. Plz confirm it still has the bug
                     //@Krom: I can't remember reporting this, and I can't remember it happening recently. To be deleted
                     end;
  ct_ClearUp:        begin
                     if ParamList[0] = 255 then
                       fTerrain.RevealWholeMap(TPlayerID(CurrentPlayerIndex))
                     else
                       fTerrain.RevealCircle(KMPointX1Y1(ParamList[0],ParamList[1]),ParamList[2],100,TPlayerID(CurrentPlayerIndex));
                     end;
  ct_SetHouse:       begin
                     if InRange(ParamList[0],0,HOUSE_COUNT-1) then
                       LastHouse := fPlayers.Player[CurrentPlayerIndex].AddHouse(THouseType(ParamList[0]+1), KMPointX1Y1(ParamList[1]+HouseDAT[ParamList[0]+1].EntranceOffsetX,ParamList[2]));
                     end;
  ct_SetHouseDamage: begin
                     if LastHouse <> nil then
                       LastHouse.AddDamage(ParamList[0]);
                     end;
  ct_SetUnit:        begin
                     if InRange(ParamList[0],0,31) then
                       fPlayers.Player[CurrentPlayerIndex].AddUnit(UnitsRemap[ParamList[0]],KMPointX1Y1(ParamList[1],ParamList[2]));
                     end;
  ct_SetUnitByStock: begin
                     if InRange(ParamList[0],0,31) then
                     begin
                       Storehouse:=TKMHouseStore(fPlayers.Player[CurrentPlayerIndex].FindHouse(ht_Store,KMPoint(0,0),1));
                       if Storehouse<>nil then
                         fPlayers.Player[CurrentPlayerIndex].AddUnit(UnitsRemap[ParamList[0]],KMPointY1(Storehouse.GetEntrance(nil)));
                     end;
                     end;
  ct_SetRoad:        begin
                     fPlayers.Player[CurrentPlayerIndex].AddRoad(KMPointX1Y1(ParamList[0],ParamList[1]));
                     end;
  ct_SetField:      begin
                     fPlayers.Player[CurrentPlayerIndex].AddField(KMPointX1Y1(ParamList[0],ParamList[1]),ft_Corn);
                     end;
  ct_Set_Winefield:  begin
                     fPlayers.Player[CurrentPlayerIndex].AddField(KMPointX1Y1(ParamList[0],ParamList[1]),ft_Wine);
                     end;
  ct_SetStock:       begin
                     //This command basically means: Put a storehouse here with road bellow it
                     LastHouse := fPlayers.Player[CurrentPlayerIndex].AddHouse(ht_Store, KMPointX1Y1(ParamList[0]-1,ParamList[1]));
                     fPlayers.Player[CurrentPlayerIndex].AddRoad(KMPointX1Y1(ParamList[0],ParamList[1]+1));
                     fPlayers.Player[CurrentPlayerIndex].AddRoad(KMPointX1Y1(ParamList[0]-1,ParamList[1]+1));
                     fPlayers.Player[CurrentPlayerIndex].AddRoad(KMPointX1Y1(ParamList[0]-2,ParamList[1]+1));
                     end;
  ct_AddWare:        begin
                     MyInt:=ParamList[1];
                     if MyInt = -1 then MyInt:=MAXWORD; //-1 means maximum resources
                     Storehouse:=TKMHouseStore(fPlayers.Player[CurrentPlayerIndex].FindHouse(ht_Store,KMPoint(0,0),1));
                     if (Storehouse<>nil) and (InRange(ParamList[0]+1,1,28)) then Storehouse.AddMultiResource(TResourceType(ParamList[0]+1),MyInt);
                     end;
  ct_AddWareToAll:   begin
                     MyInt:=ParamList[1];
                     if MyInt = -1 then MyInt:=MAXWORD; //-1 means maximum resources
                     for i:=1 to fPlayers.PlayerCount do
                     begin
                       Storehouse:=TKMHouseStore(fPlayers.Player[i].FindHouse(ht_Store,KMPoint(0,0),1));
                       if (Storehouse<>nil) and (InRange(ParamList[0]+1,1,28)) then Storehouse.AddMultiResource(TResourceType(ParamList[0]+1),MyInt);
                     end;
                     end;
  ct_AddWareToSecond:begin
                     MyInt:=ParamList[1];
                     if MyInt = -1 then MyInt:=MAXWORD; //-1 means maximum resources
                     Storehouse:=TKMHouseStore(fPlayers.Player[CurrentPlayerIndex].FindHouse(ht_Store,KMPoint(0,0),2));
                     if (Storehouse<>nil) and (InRange(ParamList[0]+1,1,28)) then Storehouse.AddMultiResource(TResourceType(ParamList[0]+1),MyInt);
                     end;
  ct_AddWeapon:      begin
                     MyInt:=ParamList[1];
                     if MyInt = -1 then MyInt:=MAXWORD; //-1 means maximum weapons
                     Barracks:=TKMHouseBarracks(fPlayers.Player[CurrentPlayerIndex].FindHouse(ht_Barracks,KMPoint(0,0),1));
                     if (Barracks<>nil) and (InRange(ParamList[0]+1,17,27)) then Barracks.AddMultiResource(TResourceType(ParamList[0]+1),MyInt);
                     end;
  ct_BlockHouse:     begin
                     if InRange(ParamList[0],0,HOUSE_COUNT-1) then
                       fPlayers.Player[CurrentPlayerIndex].fMissionSettings.AllowToBuild[ParamList[0]+1]:=false;
                     end;
  ct_ReleaseHouse:   begin
                     if InRange(ParamList[0],0,HOUSE_COUNT-1) then
                       fPlayers.Player[CurrentPlayerIndex].fMissionSettings.BuildReqDone[ParamList[0]+1]:=true;
                     end;
 ct_ReleaseAllHouses:begin
                     for i:=1 to HOUSE_COUNT do
                       fPlayers.Player[CurrentPlayerIndex].fMissionSettings.BuildReqDone[i]:=true;
                     end;
  //To add:
  ct_EnablePlayer:   begin

                     end;
  ct_AddGoal:        begin

                     end;
  ct_AddLostGoal:    begin

                     end;
  ct_SetAlliance:    begin

                     end;
  ct_SetNewRemap:    begin

                     end;
  ct_SetMapColor:    begin

                     end;
  ct_SetGroup:       begin
                       if InRange(ParamList[0],14,23) then //Needs changing to 29 once TPR troops are supported
                         fPlayers.Player[CurrentPlayerIndex].AddGroup(
                         TroopsRemap[ParamList[0]],KMPointX1Y1(ParamList[1],ParamList[2]),TKMDirection(ParamList[3]+1),ParamList[4],ParamList[5]);

                     end;
  ct_SetGroupFood:   begin

                     end;
  ct_SendGroup:      begin

                     end;
  ct_AttackPosition: begin

                     end;
  end;
  Result := true; //Must have worked if we haven't exited by now
end;


procedure TMissionParser.DebugScriptError(ErrorMsg:string);
begin
  //Just an idea, a nice way of debugging script errors. Shows the error to the user so they know exactly what they did wrong.
end;


{TKMMapInfo}

procedure TKMMapsInfo.ScanSingleMapsFolder(Path:string);
var i:integer; SearchRec:TSearchRec; ft:textfile; s:string;
  MissionDetails: TKMMissionDetails;
  MapDetails: TKMMapDetails;
begin
  MapCount:=0;
  if not DirectoryExists(ExeDir+'Maps\') then exit;

  ChDir(ExeDir+'Maps\');
  FindFirst('*', faDirectory, SearchRec);
  repeat
  if (SearchRec.Attr and faDirectory = faDirectory)and(SearchRec.Name<>'.')and(SearchRec.Name<>'..') then
  if fileexists(ExeDir+'\Maps\'+SearchRec.Name+'\'+SearchRec.Name+'.dat') then
  if fileexists(ExeDir+'\Maps\'+SearchRec.Name+'\'+SearchRec.Name+'.map') then
  //if fileexists(ExeDir+'\Maps\'+SearchRec.Name+'\'+SearchRec.Name+'.txt') then
  begin
    inc(MapCount);
    Maps[MapCount].Folder:=SearchRec.Name;
  end;
  until (FindNext(SearchRec)<>0);
  FindClose(SearchRec);

  for i:=1 to MapCount do with Maps[i] do begin

    MissionDetails := fMissionParser.GetMissionDetails(ExeDir+'\Maps\'+Maps[i].Folder+'\'+Maps[i].Folder+'.dat');
    MapDetails := fMissionParser.GetMapDetails(ExeDir+'\Maps\'+Maps[i].Folder+'\'+Maps[i].Folder+'.map');
    IsFight := MissionDetails.IsFight;
    PlayerCount := MissionDetails.TeamCount;

    case MapDetails.MapSize.X*MapDetails.MapSize.Y of
              1.. 48* 48: MapSize:='XS';
       48* 48+1.. 72* 72: MapSize:='S';
       72* 72+1..112*112: MapSize:='M';
      112*112+1..176*176: MapSize:='L';
      176*176+1..256*256: MapSize:='XL';
      256*256+1..320*320: MapSize:='XXL';
      else Assert(false,'Unexpected MapDetail size');
    end;

    Title:=Maps[i].Folder;
    SmallDesc:='-';
    BigDesc:='-';

    if fileexists(ExeDir+'\Maps\'+Maps[i].Folder+'\'+Maps[i].Folder+'.txt') then begin
      assignfile(ft,ExeDir+'\Maps\'+Maps[i].Folder+'\'+Maps[i].Folder+'.txt');
      reset(ft);

      repeat
      readln(ft,s);

      if UpperCase(s)=UpperCase('Title') then
        readln(ft,Title);

      if UpperCase(s)=UpperCase('SmallDesc') then
        readln(ft,SmallDesc);

      if UpperCase(s)=UpperCase('BigDesc') then
        readln(ft,BigDesc);

      until(eof(ft));

      closefile(ft);
    end;
  end;

end;


{ Get map properties}
function TKMMapsInfo.IsFight(ID:integer):boolean;        begin Result:=Maps[ID].IsFight;         end;
function TKMMapsInfo.GetPlayerCount(ID:integer):byte;    begin Result:=Maps[ID].PlayerCount;     end;
function TKMMapsInfo.GetTitle(ID:integer):string;        begin Result:=Maps[ID].Title;           end;

//Remove any EOLs and limit length
function TKMMapsInfo.GetSmallDesc(ID:integer):string;
begin
  Result:=StringReplace(Maps[ID].SmallDesc,#124,' ',[rfReplaceAll]);
  if length(Result)>36 then begin
    setlength(Result,36);
    Result:=Result+' ...';
  end;
end;


function TKMMapsInfo.GetBigDesc(ID:integer):string;      begin Result:=Maps[ID].BigDesc;         end;
function TKMMapsInfo.GetMapSize(ID:integer):string;      begin Result:=Maps[ID].MapSize;         end;
function TKMMapsInfo.GetFolder(ID:integer):string;       begin Result:=Maps[ID].Folder;          end;
function TKMMapsInfo.GetMissionFile(ID:integer):string;  begin Result:=Maps[ID].Folder+'.dat';   end;

function TKMMapsInfo.GetTyp(ID:integer):string;
begin
  if Maps[ID].IsFight then
    Result := 'Fighting'
  else
    Result := 'Town building & fighting';
end;

function TKMMapsInfo.GetWin(ID:integer):string;          begin Result:='No win condition';       end;
function TKMMapsInfo.GetDefeat(ID:integer):string;       begin Result:='No defeat condition';    end;


end.


