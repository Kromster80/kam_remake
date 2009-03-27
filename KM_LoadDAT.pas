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

const
  COMMANDVALUES: array[TKMCommandType] of string = (
    '','SET_MAP','SET_MAX_PLAYER','SET_CURR_PLAYER','SET_HUMAN_PLAYER','SET_HOUSE',
    'SET_TACTIC','SET_AI_PLAYER','ENABLE_PLAYER','SET_NEW_REMAP','SET_MAP_COLOR',
    'CENTER_SCREEN','CLEAR_UP','BLOCK_HOUSE','RELEASE_HOUSE','RELEASE_ALL_HOUSES',
    'ADD_GOAL','ADD_LOST_GOAL','SET_UNIT','SET_STREET','SET_FIELD','SET_WINEFIELD',
    'SET_STOCK','ADD_WARE','SET_ALLIANCE','SET_HOUSE_DAMAGE','SET_UNIT_BY_STOCK',
    'SET_GROUP','SET_GROUP_FOOD','SEND_GROUP','ATTACK_POSITION','ADD_WARE_TO_SECOND',
    'ADD_WARE_TO_ALL','ADD_WEAPON');

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
  TMissionParser = class(TObject)
  private     { Private declarations }
    OpenedMissionName:string;
    CurrentPlayerIndex: integer;
    LastHouse: TKMHouse;
    function ProcessCommand(CommandType: TKMCommandType; ParamList: array of integer; TextParam:string):boolean;
    procedure DebugScriptError(ErrorMsg:string);
    procedure UnloadMission;
  public      { Public declarations }
    constructor Create;
    function LoadDATFile(AFileName:string):boolean;
end;

type
  TKMMapsInfo = class(TObject)
  private
    MapCount:word;
    Maps:array[1..255]of record
      Folder:string; //Map folder
      IsFight:boolean; //Fight or Build map
      PlayerCount:byte;
      Title,SmallDesc,BigDesc:string;
      MapSize:string; //S,M,L,XL
      MissionFile:string; //mission00.dat
    end;
  public
    procedure ScanSingleMapsFolder(Path:string);
    property GetMapCount:word read MapCount;
    function IsFight(ID:integer):boolean;
    function GetPlayerCount(ID:integer):byte;
    function GetTitle(ID:integer):string;
    function GetSmallDesc(ID:integer):string;
    function GetBigDesc(ID:integer):string;
    function GetMapSize(ID:integer):string;
    function GetFolder(ID:integer):string;
    function GetMissionFile(ID:integer):string;
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

function TMissionParser.LoadDATFile(AFileName:string):boolean;
const ENCODED = true; //If false then files will be opened as text
var
  FileText, CommandText, Param, TextParam: string;
  ParamList: array[1..8] of integer;
  i, k, l, FileSize: integer;
  f: file;
  c:array[1..131072] of char;
  CommandType: TKMCommandType;
begin
  Result:=false; //Set it right from the start
  if not CheckFileExists(AFileName) then exit;
  UnloadMission; //Call function which will reset fPlayers and other stuff

  OpenedMissionName:=AFileName; //Used in MAP loading later on

  //Load and decode .DAT file into FileText
  assignfile(f,AFileName); reset(f,1);
  blockread(f,c[1],length(c),FileSize);
  Assert(FileSize<>length(c),'DAT file size is too big, can''t fit into buffer');
  setlength(FileText,FileSize);
  closefile(f);

  //@Lewin: ENCODED := c[1] = chr(206); //That is encoded first char

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
          if StrToIntDef(Param,-1) <> -1 then ParamList[l] := StrToInt(Param)
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
                     //We must extract the file path from the text in quotes
                     if TextParam[1] = '"' then
                     begin
                       i := 2;
                       repeat
                         MyStr := MyStr+TextParam[i];
                         inc(i);
                       until (TextParam[i] = '"') or (i >= Length(TextParam));

                       //fTerrain.OpenMapFromFile(ExeDir+MyStr); Useless
                       if not fTerrain.OpenMapFromFile(ExeDir+MyStr) then //This one is enough
                       begin
                         if not fTerrain.OpenMapFromFile(ChangeFileExt(OpenedMissionName,'.map')) then begin
                           //Result := false;
                           exit;
                         end;
                       end;
                       fViewport.SetZoom:=1;
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
                       Storehouse:=TKMHouseStore(fPlayers.Player[CurrentPlayerIndex].FindHouse(ht_Store,0,0,1));
                       if Storehouse<>nil then
                         fPlayers.Player[CurrentPlayerIndex].AddUnit(UnitsRemap[ParamList[0]],KMPointY1(Storehouse.GetEntrance));
                     end;
                     end;
  ct_SetRoad:        begin
                     fPlayers.Player[CurrentPlayerIndex].AddRoad(KMPointX1Y1(ParamList[0],ParamList[1]),mu_RoadPlan);
                     end;
  ct_SetField:      begin
                     fPlayers.Player[CurrentPlayerIndex].AddRoad(KMPointX1Y1(ParamList[0],ParamList[1]),mu_FieldPlan);
                     end;
  ct_Set_Winefield:  begin
                     fPlayers.Player[CurrentPlayerIndex].AddRoad(KMPointX1Y1(ParamList[0],ParamList[1]),mu_WinePlan);
                     end;
  ct_SetStock:       begin
                     //This command basically means: Put a storehouse here with road bellow it
                     LastHouse := fPlayers.Player[CurrentPlayerIndex].AddHouse(ht_Store, KMPointX1Y1(ParamList[0],ParamList[1]));
                     fPlayers.Player[CurrentPlayerIndex].AddRoad(KMPointX1Y1(ParamList[0],ParamList[1]+1),mu_RoadPlan);
                     fPlayers.Player[CurrentPlayerIndex].AddRoad(KMPointX1Y1(ParamList[0]-1,ParamList[1]+1),mu_RoadPlan);
                     fPlayers.Player[CurrentPlayerIndex].AddRoad(KMPointX1Y1(ParamList[0]-2,ParamList[1]+1),mu_RoadPlan);
                     end;
  ct_AddWare:        begin
                     MyInt:=ParamList[1];
                     if MyInt = -1 then MyInt:=MAXWORD; //-1 means maximum resources
                     Storehouse:=TKMHouseStore(fPlayers.Player[CurrentPlayerIndex].FindHouse(ht_Store,0,0,1));
                     if (Storehouse<>nil) and (InRange(ParamList[0]+1,1,28)) then Storehouse.AddMultiResource(TResourceType(ParamList[0]+1),MyInt);
                     end;
  ct_AddWareToAll:   begin
                     MyInt:=ParamList[1];
                     if MyInt = -1 then MyInt:=MAXWORD; //-1 means maximum resources
                     for i:=1 to fPlayers.PlayerCount do
                     begin
                       Storehouse:=TKMHouseStore(fPlayers.Player[i].FindHouse(ht_Store,0,0,1));
                       if (Storehouse<>nil) and (InRange(ParamList[0]+1,1,28)) then Storehouse.AddMultiResource(TResourceType(ParamList[0]+1),MyInt);
                     end;
                     end;
  ct_AddWareToSecond:begin
                     MyInt:=ParamList[1];
                     if MyInt = -1 then MyInt:=MAXWORD; //-1 means maximum resources
                     Storehouse:=TKMHouseStore(fPlayers.Player[CurrentPlayerIndex].FindHouse(ht_Store,0,0,2));
                     if (Storehouse<>nil) and (InRange(ParamList[0]+1,1,28)) then Storehouse.AddMultiResource(TResourceType(ParamList[0]+1),MyInt);
                     end;
  ct_AddWeapon:      begin
                     MyInt:=ParamList[1];
                     if MyInt = -1 then MyInt:=MAXWORD; //-1 means maximum weapons
                     Barracks:=TKMHouseBarracks(fPlayers.Player[CurrentPlayerIndex].FindHouse(ht_Barracks,0,0,1));
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

                     //@Lewin: Keep it as placeholder, just in case.
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
begin
  MapCount:=0;
  if not DirectoryExists(ExeDir+'Maps\') then exit;

  ChDir(ExeDir+'Maps\');
  FindFirst('*', faDirectory, SearchRec);
  repeat
  if (SearchRec.Attr and faDirectory = faDirectory)and(SearchRec.Name<>'.')and(SearchRec.Name<>'..') then
  if fileexists(ExeDir+'\Maps\'+SearchRec.Name+'\'+SearchRec.Name+'.txt') then begin
    inc(MapCount);
    Maps[MapCount].Folder:=SearchRec.Name;
  end;
  until (FindNext(SearchRec)<>0);
  FindClose(SearchRec);

  for i:=1 to MapCount do with Maps[i] do begin
    assignfile(ft,ExeDir+'\Maps\'+Maps[i].Folder+'\'+Maps[i].Folder+'.txt');
    reset(ft);

    repeat
    readln(ft,s);

    //@Lewin: Most of these options could be read from mission.dat file. Can you make them into LoadDAT ?
    //Some kind of quick parser. e.g. function GetPlayerCount(mission.dat):byte;
    //Also for now we can use map folders (txt+map+dat files), but later we need to ajoin them somehow?

    if UpperCase(s)=UpperCase('IsFight') then begin
      readln(ft,s);
      Assert((UpperCase(s)='TRUE')or(UpperCase(s)='FALSE'),'\Maps\'+Maps[i].Folder+'\Mission.txt'+eol+'Wrong IsFight value');
      IsFight := UpperCase(s)='TRUE';
    end;

    if UpperCase(s)=UpperCase('PlayerCount') then begin
      readln(ft,PlayerCount);
      Assert(InRange(PlayerCount,1,Max_Players),'\Maps\'+Maps[i].Folder+'\Mission.txt'+eol+'Wrong PlayerCount value')
    end;

    if UpperCase(s)=UpperCase('Title') then
      readln(ft,Title);

    if UpperCase(s)=UpperCase('SmallDesc') then
      readln(ft,SmallDesc);

    if UpperCase(s)=UpperCase('BigDesc') then
      readln(ft,BigDesc);

    if UpperCase(s)=UpperCase('MapSize') then begin
      readln(ft,MapSize);
      Assert(InRange(length(MapSize),1,3),'\Maps\'+Maps[i].Folder+'\Mission.txt'+eol+'Wrong MapSize value')
    end;

    until(eof(ft));

    closefile(ft);
  end;
end;


{ Get map properties}
function TKMMapsInfo.IsFight(ID:integer):boolean;        begin Result:=Maps[ID].IsFight;         end;
function TKMMapsInfo.GetPlayerCount(ID:integer):byte;    begin Result:=Maps[ID].PlayerCount;     end;
function TKMMapsInfo.GetTitle(ID:integer):string;        begin Result:=Maps[ID].Title;           end;
function TKMMapsInfo.GetSmallDesc(ID:integer):string;    begin Result:=Maps[ID].SmallDesc;       end;
function TKMMapsInfo.GetBigDesc(ID:integer):string;      begin Result:=Maps[ID].BigDesc;         end;
function TKMMapsInfo.GetMapSize(ID:integer):string;      begin Result:=Maps[ID].MapSize;         end;
function TKMMapsInfo.GetFolder(ID:integer):string;       begin Result:=Maps[ID].Folder;          end;
function TKMMapsInfo.GetMissionFile(ID:integer):string;  begin Result:=Maps[ID].Folder+'.dat';   end;
function TKMMapsInfo.GetWin(ID:integer):string;          begin Result:='No win condition';       end;
function TKMMapsInfo.GetDefeat(ID:integer):string;       begin Result:='No defeat condition';    end;


end.


