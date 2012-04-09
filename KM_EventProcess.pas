unit KM_EventProcess;
{$I KaM_Remake.inc}
interface
uses
  Classes, Math, SysUtils, StrUtils,
  KM_CommonClasses, KM_Defaults, KM_Points;


const
  MAX_EVENT_PARAMS = 2;

  //Events are straightforward: trigger -> event
  //When event has occurred it is simply removed

  //Win Defeat conditions are more complicated, they can not be represented as events that easily
  //because they usually consist of several triggers happening in different times and are reversible
  //E.g. for victory we need to: destroy Store, School, Barracks, Town Hall, troops, builders. While
  //at it enemy can rebuild/train them and we would need to add semaphore events for that to work ..

  //In TSK, there are no enemies and you win when you build the tannery.
  //In TPR, you must defeat the enemies AND build the tannery.

type
  //Triggers we handle
  TEventTrigger = (
    etDefeated,       //[] Certain player has been defeated, we rely on Conditions to generate that event
    etTime,           //[Tick] Time has come
    etHouseBuilt);    //[House] Certain house was built

  //Actions we can do
  TEventAction = (
    eaDelayedMessage, //[Delay, MsgIndex] Adds new etTime/eaShowMessage event (usefull to display delayed messages)
    eaShowMessage,    //[MsgIndex]
    eaVictory);       //[]

  TKMTrigger = record
    Trigger: TEventTrigger;
    Player: Integer; //We use Integer because of TryStrToInt
    Params: array [0..MAX_EVENT_PARAMS-1] of Integer;
  end;

  TKMAction = record
    Action: TEventAction;
    Player: Integer; //We use Integer because of TryStrToInt
    Params: array [0..MAX_EVENT_PARAMS-1] of Integer;
  end;

  TKMEventsManager = class;

  //simple Trigger causes an Action
  TKMEvent = class
  private
    fOwner: TKMEventsManager;
    fTrigger: TKMTrigger; //What sets the event off
    fAction: TKMAction; //What happens
  public
    constructor Create(aOwner: TKMEventsManager);

    function Handle(aTrigger: TKMTrigger): Boolean;
    procedure SetParams(aTrigger: TKMTrigger; aAction: TKMAction);
    procedure SaveToList(aList: TStringList);
    function TryLoadFromLine(aLine: AnsiString): Boolean;
    procedure Save(SaveStream: TKMemoryStream);
    procedure Load(LoadStream: TKMemoryStream);
  end;

  //Collection of events
  TKMEventsManager = class
  private
    fEvents: TList;
    function GetEvent(aIndex: Integer): TKMEvent;
    property Events[aIndex: Integer]: TKMEvent read GetEvent;
    procedure Proc(aTrigger: TEventTrigger; aPlayer: TPlayerIndex; aParams: array of Integer);
  public
    constructor Create;
    destructor Destroy; override;

    procedure AddEvent(aTrigger: TKMTrigger; aAction: TKMAction);

    //Process game events
    procedure ProcDefeated(aPlayer: TPlayerIndex);
    procedure ProcHouseBuilt(aHouseType: THouseType; aOwner: TPlayerIndex);
    procedure ProcTime(aTick: Cardinal);

    procedure LoadFromFile(aFileName: string);
    procedure SaveToFile(aFileName: string);

    procedure Save(SaveStream: TKMemoryStream);
    procedure Load(LoadStream: TKMemoryStream);
  end;


var
  fEventsManager: TKMEventsManager;


implementation
uses KM_Game, KM_MessageStack, KM_PlayersCollection, KM_TextLibrary, KM_Log;


const
  //How many parameters each trigger/action has
  StrTriggers: array [TEventTrigger] of string = ('DEFEATED', 'TIME', 'HOUSE_BUILT');
  StrActions: array [TEventAction] of string = ('DELAYED_MESSAGE', 'SHOW_MESSAGE', 'VICTORY');
  TriggerParamCount: array [TEventTrigger] of byte = (0, 1, 1);
  ActionParamCount: array [TEventAction] of byte = (2, 1, 0);

function MakeAction(aAct: TEventAction; aPlayer: TPlayerIndex; aParams: array of Integer): TKMAction;
var
  I: Integer;
begin
  Result.Action := aAct;
  Result.Player := aPlayer;
  for I := 0 to High(aParams) do
    Result.Params[I] := aParams[I];

  //Fill the rest with `empty`
  for I := High(aParams) + 1 to MAX_EVENT_PARAMS - 1 do
    Result.Params[I] := -1;
end;


function MakeTrigger(aTrigger: TEventTrigger; aPlayer: TPlayerIndex; aParams: array of Integer): TKMTrigger;
var
  I: Integer;
begin
  Result.Trigger := aTrigger;
  Result.Player := aPlayer;
  for I := 0 to High(aParams) do
    Result.Params[I] := aParams[I];

  //Fill the rest with `empty`
  for I := High(aParams) + 1 to MAX_EVENT_PARAMS - 1 do
    Result.Params[I] := -1;
end;


{ TKMEventsManager }
constructor TKMEventsManager.Create;
begin
  inherited;

  fEvents := TList.Create;
end;


destructor TKMEventsManager.Destroy;
var I: Integer;
begin
  for I := 0 to fEvents.Count - 1 do
    Events[I].Free;

  fEvents.Free;
  inherited;
end;


//Add new event, used by etDelayedMessage
procedure TKMEventsManager.AddEvent(aTrigger: TKMTrigger; aAction: TKMAction);
var E: TKMEvent;
begin
  E := TKMEvent.Create(Self);
  E.SetParams(aTrigger, aAction);
  fEvents.Add(E);
end;


procedure TKMEventsManager.Proc(aTrigger: TEventTrigger; aPlayer: TPlayerIndex; aParams: array of Integer);
var I: Integer;
begin
  //Process in reverse as we delete handled events
  for I := fEvents.Count - 1 downto 0 do
  if Events[I].Handle(MakeTrigger(aTrigger, aPlayer, aParams)) then
  begin
    Events[I].Free;
    fEvents.Delete(I);
  end;
end;


procedure TKMEventsManager.ProcDefeated(aPlayer: TPlayerIndex);
begin
  Proc(etDefeated, aPlayer, []);
end;


procedure TKMEventsManager.ProcHouseBuilt(aHouseType: THouseType; aOwner: TPlayerIndex);
begin
  Proc(etHouseBuilt, aOwner, [Byte(aHouseType)]);
end;


procedure TKMEventsManager.ProcTime(aTick: Cardinal);
begin
  Proc(etTime, -1, [aTick]);
end;


function TKMEventsManager.GetEvent(aIndex: Integer): TKMEvent;
begin
  Result := fEvents[aIndex];
end;


//Try to load events from text file
procedure TKMEventsManager.LoadFromFile(aFileName: string);
var
  I: Integer;
  E: TKMEvent;
  SL: TStringList;
begin
  if not FileExists(aFileName) then
  begin
    fLog.AddToLog(aFileName + ' was not found. It is okay for mission to have no events.');
    Exit;
  end;

  fEvents.Clear;

  //Read the file line by line and try to add valid events
  SL := TStringList.Create;
  SL.LoadFromFile(aFileName);
  for I := 0 to SL.Count - 1 do
  begin
    E := TKMEvent.Create(Self);
    //Invalid events get discarded to avoid memory leaks
    if E.TryLoadFromLine(AnsiString(SL[I])) then
      fEvents.Add(E)
    else
      E.Free;
  end;
  SL.Free;
end;


procedure TKMEventsManager.Save(SaveStream: TKMemoryStream);
var I: Integer;
begin
  SaveStream.Write(fEvents.Count);
  for I := 0 to fEvents.Count - 1 do
    Events[I].Save(SaveStream);
end;


procedure TKMEventsManager.Load(LoadStream: TKMemoryStream);
var I: Integer; EventCount: Integer;
begin
  fEvents.Clear;

  LoadStream.Read(EventCount);
  for I := 0 to EventCount - 1 do
  begin
    fEvents.Add(TKMEvent.Create(Self));
    Events[I].Load(LoadStream);
  end;
end;


//Save existing events into text file
procedure TKMEventsManager.SaveToFile(aFileName: string);
var
  I: Integer;
  T: TEventTrigger;
  A: TEventAction;
  SL: TStringList;
begin
  SL := TStringList.Create;

  SL.Add('//Events file');
  SL.Add('');
  SL.Add('//Supported triggers:');
  for T := Low(TEventTrigger) to High(TEventTrigger) do
    SL.Add('//' + StrTriggers[T]);
  SL.Add('');
  SL.Add('//Supported actions:');
  for A := Low(TEventAction) to High(TEventAction) do
    SL.Add('//' + StrActions[A]);
  SL.Add('');
  SL.Add('//Syntax:');
  SL.Add('//TRIGGER PLAYER PARAMETER1..N ACTION PLAYER PARAMETER1..N');
  SL.Add('');
  SL.Add('//--------------------------------------------------------');
  SL.Add('');

  for I := 0 to fEvents.Count - 1 do
    Events[I].SaveToList(SL);

  SL.SaveToFile(aFileName);
  SL.Free;
end;


{ TKMEvent }
constructor TKMEvent.Create(aOwner: TKMEventsManager);
begin
  inherited Create;

  fOwner := aOwner;
end;


procedure TKMEvent.SetParams(aTrigger: TKMTrigger; aAction: TKMAction);
begin
  fTrigger := aTrigger;
  fAction := aAction;
end;


//Try to load event from textline, if the line is invalid we return False
function TKMEvent.TryLoadFromLine(aLine: AnsiString): Boolean;
  function GetTrigger(aText: string; out oTrigger: TEventTrigger): Boolean;
  var T: TEventTrigger;
  begin
    Result := False;
    for T := Low(TEventTrigger) to High(TEventTrigger) do
      if aText = StrTriggers[T] then
      begin
        oTrigger := T;
        Result := True;
      end;
  end;
  function GetAction(aText: string; out oAction: TEventAction): Boolean;
  var A: TEventAction;
  begin
    Result := False;
    for A := Low(TEventAction) to High(TEventAction) do
      if aText = StrActions[A] then
      begin
        oAction := A;
        Result := True;
      end;
  end;
var
  Words: array [1 .. (2 + MAX_EVENT_PARAMS) * 2] of string;
  I, N, L, R: Integer;
begin
  Result := False;

  //Skip short lines and comments
  if (Length(aLine) <= 2) or (aLine[1] + aLine[2] = '//') then Exit;

  //Firstly - split line into words
  L := 1;
  for I := 1 to High(Words) do
  begin
    R := PosEx(' ', aLine, L);
    if R <> 0 then
      Words[I] := Copy(aLine, L, R - L)
    else
    begin
      Words[I] := Copy(aLine, L, Length(aLine) - L + 1);
      Break;
    end;
    L := R + 1;
  end;

  //Parse words
  //We can safely write to own fields, because if Result=False the Event will be Freed
  Result := GetTrigger(Words[1], fTrigger.Trigger) and TryStrToInt(Words[2], fTrigger.Player);
  if Result then
    for I := 0 to TriggerParamCount[fTrigger.Trigger] - 1 do
      Result := Result and TryStrToInt(Words[I + 3], fTrigger.Params[I]);

  if not Result then Exit;
  N := 2 + TriggerParamCount[fTrigger.Trigger];

  Result := Result and GetAction(Words[N + 1], fAction.Action) and TryStrToInt(Words[N + 2], fAction.Player);

  if Result then
    for I := 0 to ActionParamCount[fAction.Action] - 1 do
      Result := Result and TryStrToInt(Words[I + N + 3], fAction.Params[I]);
end;


function TKMEvent.Handle(aTrigger: TKMTrigger): Boolean;
var I: Integer;
begin
  Result := (aTrigger.Trigger = fTrigger.Trigger) and (aTrigger.Player = fTrigger.Player);
  for I := 0 to TriggerParamCount[aTrigger.Trigger] - 1 do
    Result := Result and (fTrigger.Params[I] = aTrigger.Params[I]);

  if Result then
  case fAction.Action of
    eaDelayedMessage: fOwner.AddEvent(MakeTrigger(etTime, -1, [fGame.GameTickCount + Cardinal(fAction.Params[0])]), MakeAction(eaShowMessage, fAction.Player, [fAction.Params[1]]));
    eaShowMessage:    if MyPlayer.PlayerIndex = fAction.Player then
                        fGame.fGamePlayInterface.MessageIssue(mkText, fTextLibrary.GetMissionString(fAction.Params[0]), KMPoint(0,0));
    eaVictory:        fGame.PlayerVictory(fAction.Player);
  end;
end;


procedure TKMEvent.Save(SaveStream: TKMemoryStream);
begin
  SaveStream.Write(fTrigger, SizeOf(fTrigger));
  SaveStream.Write(fAction, SizeOf(fAction));
end;


procedure TKMEvent.Load(LoadStream: TKMemoryStream);
begin
  LoadStream.Read(fTrigger, SizeOf(fTrigger));
  LoadStream.Read(fAction, SizeOf(fAction));
end;


//Assemble the Event line and append it to StringList
procedure TKMEvent.SaveToList(aList: TStringList);
var
  S: string;
  I: Integer;
begin
  S := StrTriggers[fTrigger.Trigger] + ' ' + IntToStr(fTrigger.Player);

  for I := 0 to TriggerParamCount[fTrigger.Trigger] - 1 do
    S := S + ' ' + IntToStr(fTrigger.Params[I]);

  S := S + ' ' + StrActions[fAction.Action] + ' ' + IntToStr(fAction.Player);

  for I := 0 to ActionParamCount[fAction.Action] - 1 do
    S := S + ' ' + IntToStr(fAction.Params[I]);

  aList.Add(S);
end;


end.
