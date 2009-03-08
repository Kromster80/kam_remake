unit KM_LoadDAT;
interface
uses
  Windows, Classes, SysUtils, StrUtils, KromUtils, KM_Users, Dialogs, Math, KM_Terrain, KM_Viewport;

//@Krom: Should these be in Defaults, or should they be here because this is the only unit which will use it?
type
  TKMCommandType = (ct_Unknown,ct_SetMap,ct_SetMaxPlayer,ct_SetCurrPlayer,ct_SetHumanPlayer,ct_SetHouse,
                    ct_SetTactic,ct_AIPlayer,ct_EnablePlayer,ct_SetNewRemap,ct_SetMapColor,ct_CenterScreen,ct_ClearUp,ct_BlockHouse,ct_ReleaseHouse,ct_ReleaseAllHouses,ct_AddGoal,ct_SetUnit,ct_SetRoad);//,ct_,ct_,ct_,ct_,ct_,ct_,ct_,ct_,ct_,ct_,ct_,ct_,ct_,ct_,ct_,ct_,ct_,ct_,ct_,ct_);

const
  //@Krom: If you are happy with my system here then let me know and I will add all of the commands
  COMMANDVALUES: array[TKMCommandType] of string = ('','SET_MAP','SET_MAX_PLAYER','SET_CURR_PLAYER','SET_HUMAN_PLAYER','SET_HOUSE',
                                                    'SET_TACTIC','SET_AI_PLAYER','ENABLE_PLAYER','SET_NEW_REMAP','SET_MAP_COLOR','CENTER_SCREEN','CLEAR_UP','BLOCK_HOUSE','RELEASE_HOUSE','RELEASE_ALL_HOUSES','ADD_GOAL','SET_UNIT','SET_STREET');//,'','','','','','','','','','','','','','','','','','','');
  MAXPARAMS = 8;

type
  TMissionPaser = class(TObject)
  private     { Private declarations }
    CurrentPlayerIndex: integer;
    procedure ProcessCommand(CommandType: TKMCommandType; ParamList: array of string);
    procedure DebugScriptError(ErrorMsg:string);
    procedure UnloadMission;
  public      { Public declarations } 
    constructor Create;
    procedure LoadDATFile(AFileName:string);
end;

var
  fMissionPaser: TMissionPaser;

implementation
uses KM_Defaults;

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

{ STRING FUNCTIONS }
//Should be in LewinUtils? ;)

function RightStrTo(s : string; SearchPattern : string) : string;
var
  temp : string;
begin
  Result := '';
  temp := s;
  while length(temp) > 1 do
  begin
    if RightStr(temp, length(SearchPattern)) = SearchPattern then
    begin
      Result := RightStr(s, length(s) - length(temp));
      temp := 'aa';
    end;
    temp := LeftStr(temp, length(temp) - 1);
  end;
end;

function LeftStrTo(s : string; SearchPattern : string) : string;
var
  temp : string;
begin
  Result := '';
  temp := s;
  while length(temp) > 1 do
  begin
    if LeftStr(temp, length(SearchPattern)) = SearchPattern then
    begin
      Result := leftstr(s, length(s) - length(temp));
      temp := 'aa';
    end;
    temp := RightStr(temp, length(temp) - 1);
  end;
end;
{ END STRING FUNCTIONS }


constructor TMissionPaser.Create;
begin
  inherited Create;
end;

procedure TMissionPaser.UnloadMission;
begin
  FreeAndNil(fPlayers);
  CurrentPlayerIndex := 1;
end;

procedure TMissionPaser.LoadDATFile(AFileName:string);
var
  FileText, CommandText: string;
  ParamList: array[1..8] of string;
  i, k, l, FileSize: integer;
  f: file;
  c:array[1..131072] of char;  
  fl: textfile;
  CommandType: TKMCommandType;
begin
  if not CheckFileExists(AFileName) then exit;
  UnloadMission; //Call function which will reset fPlayers and other stuff

  //Load and decode .DAT file into FileText
  FileSize:=GetFileSize(AFileName);
  assignfile(f,AFileName); reset(f,1);
  blockread(f,c,FileSize);
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
        ParamList[l]:='';
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
          repeat
            ParamList[l]:=ParamList[l]+FileText[k];
            inc(k);
          until((k>=length(FileText))or(FileText[k]='!')or(FileText[k]=#32)); //Until we find another ! OR we run out of data
          if FileText[k]=#32 then inc(k);
        end;
      //We now have command text and parameters, so process them
      ProcessCommand(CommandType,ParamList)
    end
    else
      inc(k);
  until (k>=length(FileText));
end;

procedure TMissionPaser.ProcessCommand(CommandType: TKMCommandType; ParamList: array of string);
var
  MyStr: string;
  i: integer;
begin
  //@Krom: Items bellow are just examples, please feel free to redo them, as I'm not sure if I'm doing it the best way. (am I doing too much range checking/debugging?)
  //Look in my mission editor help file if you want info on the commands. (under Appendix -> List of Commands)
  case CommandType of
  ct_SetMap:         begin
                     //We must extract the file path from the text in quotes
                     if ParamList[0][1] = '"' then
                     begin
                       i := 2;
                       repeat
                         MyStr := MyStr+ParamList[0][i];
                         inc(i);
                       until ParamList[0][i] = '"';
                       fTerrain.OpenMapFromFile(ExeDir+MyStr);
                       fViewport.SetZoom:=1;
                     end;
                     end;
  ct_SetMaxPlayer:   begin
                     fPlayers:=TKMAllPlayers.Create(EnsureRange(StrToIntDef(ParamList[0],0),1,MAX_PLAYERS)); //Create players
                     end;
  ct_SetCurrPlayer:  begin
                     if fPlayers <> nil then
                       CurrentPlayerIndex := EnsureRange(StrToIntDef(ParamList[0],0),0,fPlayers.PlayerCount-1)+1;
                     end;
  ct_SetHumanPlayer: begin
                     if fPlayers <> nil then
                       MyPlayer := fPlayers.Player[EnsureRange(StrToIntDef(ParamList[0],0),0,fPlayers.PlayerCount-1)+1];
                     end;
  ct_CenterScreen:   begin
                     fViewPort.SetCenter(StrToIntDef(ParamList[0],0),StrToIntDef(ParamList[1],0));
                     end;
  ct_ClearUp:        begin
                     if fPlayers <> nil then
                       fTerrain.RevealCircle(KMPoint(StrToIntDef(ParamList[0],0),StrToIntDef(ParamList[1],0)),StrToIntDef(ParamList[2],0),100,TPlayerID(CurrentPlayerIndex));
                     end;
  ct_SetHouse:       begin
                     if fPlayers <> nil then
                       fPlayers.Player[CurrentPlayerIndex].AddHouse(THouseType( EnsureRange(StrToIntDef(ParamList[0],0),0,Integer(High(THouseType))-1)+1 ), KMPoint(StrToIntDef(ParamList[1],0),StrToIntDef(ParamList[2],0)));
                     end;
  ct_SetUnit:        begin
                     if fPlayers <> nil then
                       fPlayers.Player[CurrentPlayerIndex].AddUnit(TUnitType( EnsureRange(StrToIntDef(ParamList[0],0),0,24{Integer(High(TUnitType))}-1)+1 ), KMPoint(StrToIntDef(ParamList[1],0),StrToIntDef(ParamList[2],0)));
                     end;   
  ct_SetRoad:        begin
                     if fPlayers <> nil then
                       fPlayers.Player[CurrentPlayerIndex].AddRoadPlan(KMPoint(StrToIntDef(ParamList[0],0),StrToIntDef(ParamList[1],0)),mu_RoadPlan,true); //@Krom: How to make these actual roads?
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

procedure TMissionPaser.DebugScriptError(ErrorMsg:string);
begin
  //Just an idea, a nice way of debugging script errors. Shows the error to the user so they know exactly what they did wrong.
end;

end.


