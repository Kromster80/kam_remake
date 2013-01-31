unit KM_MissionScript_Info;
{$I KaM_Remake.inc}
interface
uses
  Classes, KromUtils, SysUtils,
  KM_CommonClasses, KM_Defaults, KM_Points,
  KM_Goals, KM_MissionScript, KM_Maps;


type
  TKMMissionInfo = class
  public
    MissionMode: TKMissionMode;
    PlayerCount: ShortInt;
    DefaultHuman: ShortInt;
    PlayerHuman: array [0..MAX_PLAYERS-1] of Boolean;
    PlayerAI: array [0..MAX_PLAYERS-1] of Boolean;
  end;


  TMissionParserInfo = class(TMissionParserCommon)
  private
    fMapInfo: TKMapInfo;
    function LoadMapInfo(const aFileName: string): Boolean;
  protected
    function ProcessCommand(CommandType: TKMCommandType; P: array of Integer; TextParam: AnsiString = ''): Boolean; override;
  public
    constructor Create(aStrictParsing: Boolean);
    destructor Destroy; override;

    function LoadMission(const aFileName: string; aMapInfo: TKMapInfo): Boolean; reintroduce;
  end;


implementation


{ TKMMissionInfo }


{ TMissionParserInfo }
constructor TMissionParserInfo.Create(aStrictParsing: Boolean);
begin
  inherited;

end;


destructor TMissionParserInfo.Destroy;
begin

  inherited;
end;


function TMissionParserInfo.LoadMission(const aFileName: string; aMapInfo: TKMapInfo): Boolean;
const
  Commands: array [0..7] of AnsiString = (
    '!SET_MAX_PLAYER', '!SET_TACTIC',
    '!SET_CURR_PLAYER', '!SET_HUMAN_PLAYER', '!SET_USER_PLAYER',
    '!SET_AI_PLAYER', '!ADD_GOAL', '!ADD_LOST_GOAL');
var
  FileText: AnsiString;
begin
  fMapInfo := aMapInfo;
  fMapInfo.Clear;

  inherited LoadMission(aFileName);

  Result := False;

  FileText := ReadMissionFile(aFileName);
  if FileText = '' then
    Exit;

  //For info we need only few commands,
  //it makes sense to skip the rest
  if not TokenizeScript(FileText, 4, Commands) then
    Exit;

  if not LoadMapInfo(ChangeFileExt(fMissionFileName, '.map')) then
    Exit;

  Result := fFatalErrors = '';
end;


function TMissionParserInfo.ProcessCommand(CommandType: TKMCommandType; P: array of Integer; TextParam: AnsiString = ''): Boolean;
begin
  case CommandType of
    ct_SetMaxPlayer:    fMapInfo.PlayerCount      := P[0];
    ct_SetTactic:       fMapInfo.MissionMode      := mm_Tactic;
    ct_HumanPlayer:     begin
                          fMapInfo.DefaultHuman     := P[0];
                          fMapInfo.CanBeHuman[P[0]] := True;
                        end;
    ct_SetCurrPlayer:   fLastPlayer      := P[0];
    ct_UserPlayer:      if P[0] = -1 then
                          fMapInfo.CanBeHuman[fLastPlayer] := True
                        else
                          fMapInfo.CanBeHuman[P[0]] := True;
    ct_AIPlayer:       if P[0] = -1 then
                          fMapInfo.CanBeAI[fLastPlayer] := True
                        else
                          fMapInfo.CanBeAI[P[0]] := True;
    ct_AddGoal:         if fLastPlayer >= 0 then
                          //If the condition is time then P[3] is the time, else it is player ID
                          if TGoalCondition(P[0]) = gc_Time then
                            fMapInfo.AddGoal(glt_Victory, fLastPlayer, TGoalCondition(P[0]), TGoalStatus(P[1]), -1)
                          else
                            fMapInfo.AddGoal(glt_Victory, fLastPlayer, TGoalCondition(P[0]), TGoalStatus(P[1]), P[3]);
    ct_AddLostGoal:     if fLastPlayer >= 0 then
                          //If the condition is time then P[3] is the time, else it is player ID
                          if TGoalCondition(P[0]) = gc_Time then
                            fMapInfo.AddGoal(glt_Survive, fLastPlayer, TGoalCondition(P[0]), TGoalStatus(P[1]), -1)
                          else
                            fMapInfo.AddGoal(glt_Survive, fLastPlayer, TGoalCondition(P[0]), TGoalStatus(P[1]), P[3]);
  end;

  Result := True;
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

  fMapInfo.MapSizeX := sx;
  fMapInfo.MapSizeY := sy;
  Result := True;
end;


end.
