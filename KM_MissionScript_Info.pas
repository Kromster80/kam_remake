unit KM_MissionScript_Info;
{$I KaM_Remake.inc}
interface
uses
  {$IFDEF MSWindows} Windows, {$ENDIF}
  Classes, KromUtils, SysUtils, Dialogs, Math,
  KM_CommonClasses, KM_Defaults, KM_Points, KM_MissionScript;


type
  TKMMissionInfo = record
    MapPath: string;
    MapSizeX, MapSizeY: Integer;
    MissionMode: TKMissionMode;
    PlayerCount: ShortInt;
    DefaultHuman: ShortInt;
    PlayerHuman: array [0..MAX_PLAYERS-1] of Boolean;
    PlayerAI: array [0..MAX_PLAYERS-1] of Boolean;
    VictoryCond: string;
    DefeatCond: string;
  end;


  TMissionParserInfo = class(TMissionParserCommon)
  private
    fMissionInfo: TKMMissionInfo;
    function LoadMapInfo(const aFileName: string): Boolean;
    procedure ProcessCommand(CommandType: TKMCommandType; const P: array of integer; TextParam:AnsiString);
  public
    property MissionInfo: TKMMissionInfo read fMissionInfo;
    function LoadMission(const aFileName: string): Boolean; override;
  end;


implementation


{ TMissionParserInfo }
function TMissionParserInfo.LoadMission(const aFileName: string):boolean;
const
  MAX_CMD = 2;
var
  FileText: AnsiString;
  CommandText, Param, TextParam: AnsiString;
  ParamList: array[1..MAX_CMD] of Integer;
  k, l, IntParam: integer;
  CommandType: TKMCommandType;
begin
  inherited LoadMission(aFileName);

  Result := false;

  FileText := ReadMissionFile(aFileName);
  if FileText = '' then Exit;

  //For info we need only few commands,
  //it makes sense to skip the rest

  //FileText should now be formatted nicely with 1 space between each parameter/command
  k := 1;
  repeat
    if FileText[k]='!' then
    begin
      for l:=1 to MAX_CMD do
        ParamList[l]:=-1;
      TextParam:='';
      CommandText:='';
      //Extract command until a space
      repeat
        CommandText:=CommandText+FileText[k];
        inc(k);
      until((FileText[k]=#32)or(k>=length(FileText)));

      //Try to make it faster by only processing commands used
      if (CommandText='!SET_MAP')
      or (CommandText='!SET_MAX_PLAYER')
      or (CommandText='!SET_TACTIC')
      or (CommandText='!SET_CURR_PLAYER')
      or (CommandText='!SET_HUMAN_PLAYER')
      or (CommandText='!SET_USER_PLAYER')
      or (CommandText='!SET_AI_PLAYER')
      or (CommandText='!ADD_GOAL')
      or (CommandText='!ADD_LOST_GOAL') then
      begin
        //Now convert command into type
        CommandType := TextToCommandType(CommandText);
        inc(k);
        //Extract parameters
        for l:=1 to MAX_CMD do
          if (k<Length(FileText)) and (FileText[k]<>'!') then
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
    ct_SetMap:          MapPath          := RemoveQuotes(String(TextParam));
    ct_SetMaxPlayer:    PlayerCount      := P[0];
    ct_SetTactic:       MissionMode      := mm_Tactic;
    ct_HumanPlayer:     begin
                          DefaultHuman      := P[0];
                          PlayerHuman[P[0]] := True;
                        end;
    ct_SetCurrPlayer:   fLastPlayer      := P[0];
    ct_UserPlayer:      if P[0] = -1 then
                          PlayerHuman[fLastPlayer] := True
                        else
                          PlayerHuman[P[0]] := True;
    ct_AIPlayer:       if P[0] = -1 then
                          PlayerAI[fLastPlayer] := True
                        else
                          PlayerAI[P[0]] := True;
{                       if TGoalCondition(P[0]) = gc_Time then
                         VictoryCond := VictoryCond + fPlayers[fLastPlayer].AddGoal(glt_Victory,TGoalCondition(P[0]),TGoalStatus(P[1]),P[3],P[2],play_none)
                       else
                         fPlayers[fLastPlayer].AddGoal(glt_Victory,TGoalCondition(P[0]),TGoalStatus(P[1]),0,P[2],TPlayerID(P[3]));
}
    ct_AddGoal:        VictoryCond      := VictoryCond
                                         + GoalConditionStr[TGoalCondition(P[0])] + ' '
                                         + GoalStatusStr[TGoalStatus(P[1])]+', ';
    ct_AddLostGoal:    DefeatCond       := DefeatCond
                                         + GoalConditionStr[TGoalCondition(P[0])] + ' '
                                         + GoalStatusStr[TGoalStatus(P[1])]+', ';
  end;
end;


//Acquire essential terrain details
function TMissionParserInfo.LoadMapInfo(const aFileName: string): Boolean;
var
  F: TKMemoryStream;
  sx,sy: Integer;
begin
  Result := False;
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
    AddError('MissionParser can''t open the map because it''s too big.', True);
    Result := false;
    Exit;
  end;

  fMissionInfo.MapSizeX := sx;
  fMissionInfo.MapSizeY := sy;
  Result := True;
end;


end.
