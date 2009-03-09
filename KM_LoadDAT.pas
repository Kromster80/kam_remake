unit KM_LoadDAT;
interface
uses
  Windows, Classes, SysUtils, StrUtils, KromUtils, Dialogs, Math;

//@Krom: Why? If you want the value as an integer then just use "Integer(TKMCommandType[i])". That will return it as an integer, with the first one as 0 and so on. Surely that's easier?
//@Lewin: Exactly my point, but just to make sure first element is 0, who knows how we could extend it. See TUnitType for example

type
  TKMCommandType = (ct_Unknown=0,ct_SetMap,ct_SetMaxPlayer,ct_SetCurrPlayer,ct_SetHumanPlayer,ct_SetHouse,
                    ct_SetTactic,ct_AIPlayer,ct_EnablePlayer,ct_SetNewRemap,ct_SetMapColor,ct_CenterScreen,
                    ct_ClearUp,ct_BlockHouse,ct_ReleaseHouse,ct_ReleaseAllHouses,ct_AddGoal,ct_SetUnit,ct_SetRoad);
                    //,ct_,ct_,ct_,ct_,ct_,ct_,ct_,ct_,ct_,ct_,ct_,ct_,ct_,ct_,ct_,ct_,ct_,ct_,ct_,ct_);

const
  COMMANDVALUES: array[TKMCommandType] of string = ('','SET_MAP','SET_MAX_PLAYER','SET_CURR_PLAYER','SET_HUMAN_PLAYER','SET_HOUSE',
                                                    'SET_TACTIC','SET_AI_PLAYER','ENABLE_PLAYER','SET_NEW_REMAP','SET_MAP_COLOR','CENTER_SCREEN','CLEAR_UP','BLOCK_HOUSE','RELEASE_HOUSE','RELEASE_ALL_HOUSES','ADD_GOAL','SET_UNIT','SET_STREET');//,'','','','','','','','','','','','','','','','','','','');
  MAXPARAMS = 8;

type
  TMissionParser = class(TObject)
  private     { Private declarations }
    CurrentPlayerIndex: integer;
    procedure ProcessCommand(CommandType: TKMCommandType; ParamList: array of integer; TextParam:string);
    procedure DebugScriptError(ErrorMsg:string);
    procedure UnloadMission;
  public      { Public declarations }
    constructor Create;
    procedure LoadDATFile(AFileName:string);
end;

var
  fMissionParser: TMissionParser;

implementation
uses KM_Defaults, KM_Users, KM_Terrain, KM_Viewport;

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
  CurrentPlayerIndex := 1; //@Lewin: maybe it's better to set to 0
  //@Krom: But the players are 1 indexed in the users unit... Or do you mean to start it off with an
  //invalid number so that by default commands will be ignored?
  //@Lewin: When mission is unloaded I prefer to keep it with error, so it can't be accidentially used
  //TMissionParser.LoadMission will take care of setting it to PlayerID ;)
end;

procedure TMissionParser.LoadDATFile(AFileName:string);
var
  FileText, CommandText, Param, TextParam: string;
  ParamList: array[1..8] of integer;
  i, k, l, FileSize: integer;
  f: file;
  c:array[1..131072] of char;  
  fl: textfile;
  CommandType: TKMCommandType;
begin
  if not CheckFileExists(AFileName) then exit;
  UnloadMission; //Call function which will reset fPlayers and other stuff

  //Load and decode .DAT file into FileText
  assignfile(f,AFileName); reset(f,1);
  blockread(f,c[1],length(c),FileSize);
  Assert(FileSize<>length(c),'DAT file size is too big, can''t fit into buffer');
  setlength(FileText,FileSize);
  closefile(f);

  i:=1; k:=1;
  repeat
    FileText[k]:=chr(ord(c[i]) xor 239);
    if (FileText[k]=#9)or(FileText[k]=#10)or(FileText[k]=#13) then FileText[k]:=' ';
    inc(i);
    if (k>1)and(((FileText[k-1]=#32)and(FileText[k]=#32))or((FileText[k-1]='!')and(FileText[k]='!'))) then else
    inc(k);
  until(i>FileSize);
  setlength(FileText,k); //Because some extra characters are removed

  //Temporary debugging
  //@Lewin: I guess we should make a sort of switch to enable loading of unciphered DAT files, for debug time
  {assignfile(fl,ExeDir+'Mission.txt');
  rewrite(fl);
  write(fl,FileText);
  closefile(fl);}

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
      //few string params could be handled individually
      ProcessCommand(CommandType,ParamList,TextParam)
    end
    else
      inc(k);
  until (k>=length(FileText));
end;

procedure TMissionParser.ProcessCommand(CommandType: TKMCommandType; ParamList: array of integer; TextParam:string);
var
  MyStr: string;
  i: integer;
begin
  case CommandType of
  ct_SetMap:         begin
                     //We must extract the file path from the text in quotes
                     if TextParam[1] = '"' then
                     begin
                       i := 2;
                       repeat
                         MyStr := MyStr+TextParam[i];
                         inc(i);
                       until TextParam[i] = '"';
                       fTerrain.OpenMapFromFile(ExeDir+MyStr);
                       //if not fTerrain.OpenMapFromFile(ExeDir+MyStr) then BreakWholeLoadingAndReset
                       fViewport.SetZoom:=1;
                     end;
                     end;
  ct_SetMaxPlayer:   begin
                     {"so it checks for data acceptability(is there such a word?) in code"
                     @Krom: No, acceptability is not a word. I would write
                     //"so it checks that the data is acceptabile in code" or something.
                     //English lessions in code? ;)
                     //@Lewin: Sure, why not :-) ToBeDeleted.

                     Tell me, why use StrToIntDef ? What can go wrong if we use StrToInt?
                     I vote for strict decoding without assumtions
                     if ct_SetMaxPlayer=32765 for any reason then raise an error and stop loading
                     instead of assuming it's MaxPlayers or 0
                     same goes for all parameters, some odd values should be ignored and some raise errors

                     @Krom: Aggreed. I will do that from now on. However, sometimes it will not raise an exception.
                     //For example, if the integer is to be used in an array, then it will get some other
                     //weird data out of memory. Therefore I will do range checks (but no EnsureRange)
                     //on things like player numbers which are to be used for arrays.}
                     //@Lewin: Okay, if I have more suggestions I'll write them later on


                     fPlayers:=TKMAllPlayers.Create(ParamList[0]); //Create players
                     end;
  ct_SetCurrPlayer:  begin       
                     if fPlayers <> nil then //@Krom: I guess all of these should be removed too then?
                     //(if not true then it will crash with an error, so they are not needed, right?)
                     //@Lewin: DebugScriptError may come in handy here, but of course it makes little sense
                     //to check if fPlayers <> nil for EVERY single command. Maybe there's a better solution? I don't know.
                       if (ParamList[0] > 0) and (ParamList[0] < fPlayers.PlayerCount-1) then
                         CurrentPlayerIndex := ParamList[0]+1; //+1 because in DAT players IDs are 0 based, but here they are 1 based
                     end;
  ct_SetHumanPlayer: begin
                     if fPlayers <> nil then
                       if (ParamList[0] > 0) and (ParamList[0] < fPlayers.PlayerCount-1) then
                       //@Lewin: try this - if InRange(ParamList[0],0,fPlayers.PlayerCount-1) then
                         MyPlayer := fPlayers.Player[ParamList[0]+1];
                     end;
  ct_CenterScreen:   begin
                     fViewPort.SetCenter(ParamList[0],ParamList[1]);
                     end;
  ct_ClearUp:        begin
                     if fPlayers <> nil then
                       fTerrain.RevealCircle(KMPoint(ParamList[0],ParamList[1]),ParamList[2],100,TPlayerID(CurrentPlayerIndex));
                     end;
  ct_SetHouse:       begin
                     if fPlayers <> nil then
                       if (ParamList[0] >= 0) and (ParamList[0] <= Integer(High(THouseType))-1) then
                         fPlayers.Player[CurrentPlayerIndex].AddHouse(THouseType(ParamList[0]+1), KMPoint(ParamList[1],ParamList[2]));
                     end;
  ct_SetUnit:        begin
                     if fPlayers <> nil then
                       if (ParamList[0] >= 0) and (ParamList[0] <= 24{Integer(High(TUnitType))-1}) then
                         fPlayers.Player[CurrentPlayerIndex].AddUnit(TUnitType(ParamList[0]+1),KMPoint(ParamList[1],ParamList[2]));
                     end;
  ct_SetRoad:        begin
                     if fPlayers <> nil then
                       fPlayers.Player[CurrentPlayerIndex].AddRoad(KMPoint(ParamList[0],ParamList[1]),mu_RoadPlan);
                       //@Krom: How to make these actual roads?
                       //@Lewin: I've added command fPlayers.Player[CurrentPlayerIndex].AddRoad();
                     end;
  //To add:
  ct_SetTactic:      begin

                     end;
  ct_EnablePlayer:   begin

                     end;
  ct_AIPlayer:       begin

                     end;
  end;
end;

procedure TMissionParser.DebugScriptError(ErrorMsg:string);
begin
  //Just an idea, a nice way of debugging script errors. Shows the error to the user so they know exactly what they did wrong.
end;

end.


