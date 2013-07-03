unit KM_MissionScript;
{$I KaM_Remake.inc}
interface
uses
  Classes, SysUtils,
  KM_Defaults;


type
  TMissionParsingMode = (
                          mpm_Single,
                          mpm_Multi,  //Skip players
                          mpm_Editor  //Ignore errors, load armies differently
                        );

  TKMCommandType = (ct_Unknown=0,ct_SetMap,ct_SetMaxPlayer,ct_SetCurrPlayer,ct_HumanPlayer,ct_UserPlayer,ct_SetHouse,
                    ct_SetTactic,ct_AIPlayer,ct_EnablePlayer,ct_SetNewRemap,ct_SetMapColor,ct_CenterScreen,
                    ct_ClearUp,ct_BlockTrade,ct_BlockHouse,ct_ReleaseHouse,ct_ReleaseAllHouses,ct_AddGoal,ct_AddLostGoal,
                    ct_SetUnit,ct_SetRoad,ct_SetField,ct_SetWinefield,ct_SetStock,ct_AddWare,ct_SetAlliance,
                    ct_SetHouseDamage,ct_SetUnitByStock,ct_SetGroup,ct_SetGroupFood,ct_SendGroup,
                    ct_AttackPosition,ct_AddWareToSecond,ct_AddWareTo,ct_AddWareToLast,ct_AddWareToAll,ct_AddWeapon,ct_AICharacter,
                    ct_AINoBuild,ct_AIAutoRepair,ct_AIAutoAttack,ct_AIAutoDefend,ct_AIStartPosition,ct_AIDefence,ct_AIAttack,ct_CopyAIAttack);

const
  COMMANDVALUES: array [TKMCommandType] of AnsiString = (
    '',
    'SET_MAP',
    'SET_MAX_PLAYER',
    'SET_CURR_PLAYER',
    'SET_HUMAN_PLAYER', //Default human player (name left for compatibility with KaM)
    'SET_USER_PLAYER', //Player can be human
    'SET_HOUSE',
    'SET_TACTIC',
    'SET_AI_PLAYER', //Player can be AI
    'ENABLE_PLAYER',
    'SET_NEW_REMAP',
    'SET_MAP_COLOR',
    'CENTER_SCREEN','CLEAR_UP','BLOCK_TRADE','BLOCK_HOUSE','RELEASE_HOUSE','RELEASE_ALL_HOUSES',
    'ADD_GOAL','ADD_LOST_GOAL','SET_UNIT','SET_STREET','SET_FIELD','SET_WINEFIELD',
    'SET_STOCK','ADD_WARE','SET_ALLIANCE','SET_HOUSE_DAMAGE','SET_UNIT_BY_STOCK',
    'SET_GROUP','SET_GROUP_FOOD','SEND_GROUP','ATTACK_POSITION','ADD_WARE_TO_SECOND',
    'ADD_WARE_TO','ADD_WARE_TO_LAST','ADD_WARE_TO_ALL','ADD_WEAPON','SET_AI_CHARACTER',
    'SET_AI_NO_BUILD','SET_AI_AUTO_REPAIR','SET_AI_AUTO_ATTACK','SET_AI_AUTO_DEFEND','SET_AI_START_POSITION','SET_AI_DEFENSE','SET_AI_ATTACK',
    'COPY_AI_ATTACK');

type
  TMissionParserCommon = class
  protected
    fStrictParsing: Boolean; //Report non-fatal script errors such as SEND_GROUP without defining a group first
    fMissionFileName: string;
    fLastPlayer: TPlayerIndex; //Current Player
    fFatalErrors: string; //Fatal errors descriptions accumulate here
    fMinorErrors: string; //Minor error descriptions accumulate here
    function TextToCommandType(const ACommandText: AnsiString): TKMCommandType;
    function ReadMissionFile(const aFileName: string): AnsiString;
    function TokenizeScript(const aText: AnsiString; aMaxCmd: Byte; aCommands: array of AnsiString): Boolean;
    function ProcessCommand(CommandType: TKMCommandType; P: array of Integer; TextParam: AnsiString = ''): Boolean; virtual; abstract;
    procedure AddError(const ErrorMsg: string; aFatal: Boolean = False);
  public
    constructor Create(aStrictParsing: Boolean);
    property FatalErrors: string read fFatalErrors;
    property MinorErrors: string read fMinorErrors;
    function LoadMission(const aFileName: string): Boolean; overload; virtual;
  end;


implementation


{ TMissionParserCommon }
constructor TMissionParserCommon.Create(aStrictParsing: boolean);
begin
  inherited Create;
  fStrictParsing := aStrictParsing;
end;


function TMissionParserCommon.LoadMission(const aFileName: string):boolean;
begin
  fMissionFileName := aFileName;
  fLastPlayer := -1;

  Result := true;
end;


function TMissionParserCommon.TextToCommandType(const ACommandText: AnsiString): TKMCommandType;
var
  I: TKMCommandType;
begin
  Result := ct_Unknown;

  for I := Low(TKMCommandType) to High(TKMCommandType) do
  begin
    if ACommandText = '!' + COMMANDVALUES[I] then
    begin
      Result := I;
      Break;
    end;
  end;

  //Commented out because it slows down mission scanning
  //if Result = ct_Unknown then gLog.AddToLog(String(ACommandText));
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
      F.SaveToFile(aFileName + '.txt');

    for I := 0 to F.Size - 1 do
      if PByte(Cardinal(F.Memory)+I)^ in [9, 10, 13] then //tab, eol
        PByte(Cardinal(F.Memory)+I)^ := $20; //Space

    Num := 0;
    for I := 0 to F.Size - 1 do
    begin
      PByte(Cardinal(F.Memory)+Num)^ := PByte(Cardinal(F.Memory)+I)^;
      if (Num <= 0)
      or (
          (PWord(Cardinal(F.Memory) + Num-1)^ <> $2020) //Skip double spaces and !!
      and (PWord(Cardinal(F.Memory) + Num-1)^ <> $2121)) then
        Inc(Num);
    end;

    SetLength(Result, Num); //Because some extra characters were removed
    F.Position := 0;
    F.ReadBuffer(Result[1], Num);
  finally
    F.Free;
  end;
  //FileText should now be formatted nicely with 1 space between each parameter/command
end;


function TMissionParserCommon.TokenizeScript(const aText: AnsiString; aMaxCmd: Byte; aCommands: array of AnsiString): Boolean;
var
  CommandText, Param, TextParam: AnsiString;
  ParamList: array of Integer;
  I, K, IntParam: Integer;
  CommandType: TKMCommandType;
  J: Integer;
  DoProcess: Boolean;
begin
  Result := False;
  SetLength(ParamList, aMaxCmd);

  I := 1;
  repeat
    if aText[I] = '!' then
    begin

      //Default uninitialized values
      TextParam := '';
      CommandText := '';
      for K := 0 to aMaxCmd - 1 do
        ParamList[K] := -1;

      //Extract command until a space
      repeat
        CommandText := CommandText + aText[I];
        Inc(I);
      until((aText[I] = #32) or (I >= Length(aText)));

      //We can skip certain commands to speed up the scan
      //for implementations that need only Preview/Info
      DoProcess := Length(aCommands) = 0;
      for J := Low(aCommands) to High(aCommands) do
      if (CommandText = aCommands[J]) then
        DoProcess := True;

      if DoProcess then
      begin
        //Now convert command into type
        CommandType := TextToCommandType(CommandText);
        Inc(I);
        //Extract parameters
        for K := 0 to aMaxCmd - 1 do
          if (I < Length(aText)) and (aText[I] <> '!') then
          begin
            Param := '';
            repeat
              Param := Param + aText[I];
              Inc(I);
            until((I >= Length(aText)) or (aText[I] = '!') or (aText[I] = #32)); //Until we find another ! OR we run out of data

            //Convert to an integer, if possible
            if TryStrToInt(Param, IntParam) then
              ParamList[K] := IntParam
            else
              if K = 0 then
                TextParam := Param; //Accept text for first parameter

            if (I <= Length(aText)) and (aText[I] = #32) then
              Inc(I);
          end;
        //We now have command text and parameters, so process them

        if not ProcessCommand(CommandType, ParamList, TextParam) then
        //A returned value of false indicates an error has occoured and we should exit
          Exit;
      end;
    end
    else
      Inc(I);
  until (I >= Length(aText));

  Result := True;
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


end.
