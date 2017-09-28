unit KM_MissionScript_Info;
{$I KaM_Remake.inc}
interface
uses
  KM_MissionScript, KM_Maps;


type
  TKMMissionParsing = (
    pmBase, //Load base map info for SP maplist (player count, tactic, description)
    pmExtra //Load extra map info to be displayed when map is selected (goals, alliances, etc)
    );


  TMissionParserInfo = class(TMissionParserCommon)
  private
    fMapInfo: TKMapInfo; //We are given this structure and asked to fill it
    function LoadMapInfo(const aFileName: string): Boolean;
  protected
    function ProcessCommand(CommandType: TKMCommandType; P: array of Integer; const TextParam: AnsiString = ''): Boolean; override;
  public
    function LoadMission(const aFileName: string; aMapInfo: TKMapInfo; aParsing: TKMMissionParsing): Boolean; reintroduce;
  end;


implementation
uses
  SysUtils,
  KM_Resource,
  KM_CommonClasses, KM_Defaults;


{ TMissionParserInfo }
function TMissionParserInfo.LoadMission(const aFileName: string; aMapInfo: TKMapInfo; aParsing: TKMMissionParsing): Boolean;
const
  CommandsBase: array [0..3] of AnsiString = (
    '!SET_MAX_PLAYER', '!SET_TACTIC', '!SET_CURR_PLAYER', '!SET_USER_PLAYER');
  CommandsExtra: array [0..10] of AnsiString = (
    '!SET_MAX_PLAYER', '!SET_TACTIC',
    '!SET_CURR_PLAYER', '!SET_HUMAN_PLAYER', '!SET_USER_PLAYER',
    '!SET_AI_PLAYER', '!ADD_GOAL', '!ADD_LOST_GOAL', '!SET_ALLIANCE', '!SET_MAP_COLOR', '!SET_RGB_COLOR');
var
  FileText: AnsiString;
begin
  fMapInfo := aMapInfo;

  inherited LoadMission(aFileName);

  Result := False;

  FileText := ReadMissionFile(aFileName);
  if FileText = '' then
    Exit;

  //For info we need only few commands,
  //it makes sense to skip the rest
  case aParsing of
    pmBase:   if not TokenizeScript(FileText, 4, CommandsBase) then Exit;
    pmExtra:  if not TokenizeScript(FileText, 4, CommandsExtra) then Exit;
  end;

  if not LoadMapInfo(ChangeFileExt(fMissionFileName, '.map')) then
    Exit;

  Result := fFatalErrors = '';
end;


function TMissionParserInfo.ProcessCommand(CommandType: TKMCommandType; P: array of Integer; const TextParam: AnsiString = ''): Boolean;
begin
  case CommandType of
    ct_SetMaxPlayer:    fMapInfo.LocCount := P[0];
    ct_SetTactic:       fMapInfo.MissionMode := mm_Tactic;
    ct_SetCurrPlayer:   fLastHand := P[0];
    ct_HumanPlayer:     begin
                          //Default human player can be human, obviously
                          fMapInfo.DefaultHuman     := P[0];
                          fMapInfo.CanBeHuman[P[0]] := True;
                        end;
    ct_UserPlayer:      if P[0] = -1 then
                          fMapInfo.CanBeHuman[fLastHand] := True
                        else
                          fMapInfo.CanBeHuman[P[0]] := True;
    ct_AIPlayer:       if P[0] = -1 then
                          fMapInfo.CanBeAI[fLastHand] := True
                        else
                          fMapInfo.CanBeAI[P[0]] := True;
    ct_AddGoal:         if fLastHand >= 0 then
                          //If the condition is time then P[3] is the time, else it is player ID
                          if TGoalCondition(P[0]) = gc_Time then
                            fMapInfo.AddGoal(glt_Victory, fLastHand, TGoalCondition(P[0]), TGoalStatus(P[1]), -1)
                          else
                            fMapInfo.AddGoal(glt_Victory, fLastHand, TGoalCondition(P[0]), TGoalStatus(P[1]), P[3]);
    ct_AddLostGoal:     if fLastHand >= 0 then
                          //If the condition is time then P[3] is the time, else it is player ID
                          if TGoalCondition(P[0]) = gc_Time then
                            fMapInfo.AddGoal(glt_Survive, fLastHand, TGoalCondition(P[0]), TGoalStatus(P[1]), -1)
                          else
                            fMapInfo.AddGoal(glt_Survive, fLastHand, TGoalCondition(P[0]), TGoalStatus(P[1]), P[3]);
    ct_SetAlliance:     if (fLastHand >= 0) and (P[0] <> fLastHand) then //Can't be enemies with yourself
                          if P[1] = 1 then
                            fMapInfo.Alliances[fLastHand, P[0]] := at_Ally
                          else
                            fMapInfo.Alliances[fLastHand, P[0]] := at_Enemy;
    ct_SetMapColor:     if fLastHand >= 0 then
                          //For now simply use the minimap color for all color, it is too hard to load all 8 shades from ct_SetNewRemap
                          fMapInfo.FlagColors[fLastHand] := gRes.Palettes.DefaultPalette.Color32(P[0]);
    ct_SetRGBColor:     if fLastHand >= 0 then
                          fMapInfo.FlagColors[fLastHand] := P[0] or $FF000000;
  end;

  Result := True;
end;


//Acquire essential terrain details
function TMissionParserInfo.LoadMapInfo(const aFileName: string): Boolean;
var
  F: TKMemoryStream;
  newX, newY: Integer;
begin
  Result := False;
  if not FileExists(aFileName) then Exit;

  F := TKMemoryStream.Create;
  try
    F.LoadFromFile(aFileName);
    F.Read(newX);
    F.Read(newY);
  finally
    F.Free;
  end;

  if (newX > MAX_MAP_SIZE) or (newY > MAX_MAP_SIZE) then
  begin
    AddError('MissionParser can''t open the map because it''s too big.', True);
    Exit;
  end;

  fMapInfo.MapSizeX := newX;
  fMapInfo.MapSizeY := newY;
  Result := True;
end;


end.
