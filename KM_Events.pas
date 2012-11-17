unit KM_Events;
{$I KaM_Remake.inc}
interface
uses
  Classes, Math, SysUtils, StrUtils,
  KM_CommonClasses, KM_Defaults, KM_Points;

  //Events are straightforward things: When something happens (Trigger)
  //and pre-conditions are right it fires an event that performes something (Action)
  //When event has occurred it is simply removed from the queue.
  //E.g. after house_built trigger has been fired for the first time it is also
  //going to be fired each next tick if we don't remove it.

  //Win Defeat conditions are more complicated, they can not be represented
  //as events that easily because they usually consist of several triggers
  //happening in different times and are reversible
  //E.g. for victory we need to destroy: Store, School, Barracks, Town Hall,
  //troops, builders. While at it enemy can rebuild/train them and we would
  //need to revert events (add semaphore events) for that to work ..

  //In TSK, there are no enemies and you win when you build the tannery.
  //In TPR, you must defeat the enemies AND build the tannery.

const
  MAX_EVENT_PARAMS = 4;

type
  //Triggers we handle
  TEventTrigger = (
    etDefeated,       //[] Certain player has been defeated, we rely on Conditions to generate that event
    etTime,           //[Tick] Time has come
    etHouseBuilt);    //[House] Certain house was built

  //Actions we can do
  TEventAction = (
    eaDelayedMessage, //[Delay, MsgIndex] Adds new etTime/eaShowMessage event (usefull to display delayed messages)
    eaGiveUnits,      //[UnitType, X, Y, Count] Give player more units
    eaGiveWares,      //[WareType, Count]
    //eaShowAlert,    //[PosX, PosY, AlertType]
    eaShowMessage,    //[MsgIndex]
    eaUnlockHouse,    //[HouseType] Allow player to build certain house type
    eaVictory);       //[]

  //Records must be packed so they are stored identically in MP saves (padding bytes are unknown values)
  TKMTrigger = packed record
    Trigger: TEventTrigger;
    Player: Integer; //We use Integer because of TryStrToInt
    Params: array [0..MAX_EVENT_PARAMS-1] of Integer;
  end;

  //Records must be packed so they are stored identically in MP saves (padding bytes are unknown values)
  TKMAction = packed record
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
    function VerifyEvent: string;
  public
    constructor Create(aOwner: TKMEventsManager);

    function Handle(aTrigger: TKMTrigger): Boolean;
    procedure SetParams(aTrigger: TKMTrigger; aAction: TKMAction);
    procedure SaveToList(aList: TStringList);
    function TryLoadFromLine(aLine: AnsiString; out Res: string): Boolean;
    procedure Save(SaveStream: TKMemoryStream);
    procedure Load(LoadStream: TKMemoryStream);
  end;

  //Collection of events
  TKMEventsManager = class
  private
    fEvents: TList;
    fErrorString: string; //Contains info about found mistakes
    function GetEvent(aIndex: Integer): TKMEvent;
    property Events[aIndex: Integer]: TKMEvent read GetEvent;
    procedure Proc(aTrigger: TEventTrigger; aPlayer: TPlayerIndex; const aParams: array of Integer);
  public
    constructor Create;
    destructor Destroy; override;

    procedure AddEvent(aTrigger: TKMTrigger; aAction: TKMAction);
    property ErrorString: string read fErrorString;

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
uses KM_Houses, KM_Terrain, KM_Game, KM_CommonTypes, KM_PlayersCollection, KM_TextLibrary, KM_Log;


const
  //How many parameters each trigger/action has
  StrTriggers: array [TEventTrigger] of string = ('DEFEATED', 'TIME', 'HOUSE_BUILT');
  StrActions: array [TEventAction] of string = ('DELAYED_MESSAGE', 'GIVE_UNITS', 'GIVE_WARES', 'SHOW_MESSAGE', 'UNLOCK_HOUSE', 'VICTORY');
  TriggerParamCount: array [TEventTrigger] of Byte = (0, 1, 1);
  ActionParamCount: array [TEventAction] of Byte = (2, 4, 2, 1, 1, 0);


//Create action out of supplied parameters
function MakeAction(aAct: TEventAction; aPlayer: TPlayerIndex; const aParams: array of Integer): TKMAction;
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


//Create trigger out of supplied parameters
function MakeTrigger(aTrigger: TEventTrigger; aPlayer: TPlayerIndex; const aParams: array of Integer): TKMTrigger;
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


procedure TKMEventsManager.Proc(aTrigger: TEventTrigger; aPlayer: TPlayerIndex; const aParams: array of Integer);
var
  I: Integer;
begin
  //Process in reverse as we delete handled events
  //(if any event causes another trigger to fire, that is already skipped in this tick - it will be handled next tick)
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
  Res: string;
  ValidEvent: Boolean;
begin
  fErrorString := '';

  if not FileExists(aFileName) then
  begin
    fLog.AddNoTime(aFileName + ' was not found. It is okay for mission to have no events.');
    Exit;
  end;

  fEvents.Clear;

  //Read the file line by line and try to add valid events
  SL := TStringList.Create;
  SL.LoadFromFile(aFileName);
  for I := 0 to SL.Count - 1 do
  begin
    E := TKMEvent.Create(Self);
    ValidEvent := E.TryLoadFromLine(AnsiString(SL[I]), Res);
    //Empty lines or invalid events get discarded to avoid memory leaks
    if ValidEvent then
      fEvents.Add(E)
    else
    begin
      E.Free;
      //If line contained some erroneous data - record it
      //(each Res line ends with eol)
      if Res <> '' then
        fErrorString := fErrorString + 'Line ' + IntToStr(I) + '. ' + Res;
    end;
  end;
  SL.Free;

  if fErrorString <> '' then
    fErrorString := fErrorString + 'Listed lines were skipped';
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
function TKMEvent.TryLoadFromLine(aLine: AnsiString; out Res: string): Boolean;
  //Result string is either empty or contains error text
  function GetTrigger(aText: string; out oTrigger: TEventTrigger): string;
  var T: TEventTrigger;
  begin
    for T := Low(TEventTrigger) to High(TEventTrigger) do
      if aText = StrTriggers[T] then
      begin
        oTrigger := T;
        Result := '';
        Exit;
      end;
    Result := aText + ' is not a valid Trigger|';
  end;
  //Result string is either empty or contains error text
  function GetAction(aText: string; out oAction: TEventAction): string;
  var A: TEventAction;
  begin
    for A := Low(TEventAction) to High(TEventAction) do
      if aText = StrActions[A] then
      begin
        oAction := A;
        Result := '';
        Exit;
      end;
    Result := aText + ' is not a valid Action|';
  end;
var
  Words: array [1 .. (2 + MAX_EVENT_PARAMS) * 2] of string;
  I, N, L, R: Integer;
begin
  Res := '';

  //Skip short lines and comments
  if (Length(aLine) <= 2) or (aLine[1] + aLine[2] = '//') then
  begin
    Result := False;
    Exit;
  end;

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
  Res := GetTrigger(Words[1], fTrigger.Trigger);
  Res := Res + IfThen(not TryStrToInt(Words[2], fTrigger.Player), 'Trigger Player index is invalid|');
  if Res = '' then
    for I := 0 to TriggerParamCount[fTrigger.Trigger] - 1 do
      Res := Res + IfThen(not TryStrToInt(Words[I + 3], fTrigger.Params[I]), 'Trigger parameter is invalid|');

  Result := Res = '';
  if not Result then Exit;

  N := 2 + TriggerParamCount[fTrigger.Trigger];

  Res := Res + GetAction(Words[N + 1], fAction.Action);
  Res := Res + IfThen(not TryStrToInt(Words[N + 2], fAction.Player), 'Action Player index is invalid|');

  if Res = '' then
    for I := 0 to ActionParamCount[fAction.Action] - 1 do
      Res := Res + IfThen(not TryStrToInt(Words[I + N + 3], fAction.Params[I]), 'Action parameter is invalid|');

  Res := Res + VerifyEvent;
  Result := Res = '';
end;


//Check that all parameters are right and return error description
function TKMEvent.VerifyEvent: string;
var
  PlayerExists, PlayerUnused: string;
begin
  Result := '';
  //Only some Triggers need player
  PlayerExists := IfThen(not InRange(fTrigger.Player, 0, fPlayers.Count - 1), 'Trigger player does not exists|');
  PlayerUnused := IfThen(fTrigger.Player <> -1, 'Trigger player should be unused (value -1)|');

  case fTrigger.Trigger of
    etDefeated:   Result := PlayerExists;
    etTime:       Result := PlayerUnused +
                            IfThen(not InRange(fTrigger.Params[0], 0, 24*60*600), 'Time should be within 0..24hours|');
    etHouseBuilt: Result := PlayerExists +
                            IfThen(not (THouseType(fTrigger.Params[0]) in [HOUSE_MIN..HOUSE_MAX]), 'House type is invalid|');
  end;

  PlayerExists := IfThen(not InRange(fAction.Player, 0, fPlayers.Count - 1), 'Action player does not exists|');
  PlayerUnused := IfThen(fAction.Player <> -1, 'Action player should be unused (value -1)|');

  case fAction.Action of
    eaDelayedMessage: Result := Result +
                                PlayerExists +
                                IfThen(not InRange(fAction.Params[0], 0, 24*60*600), 'Time should be within 0..24hours|');
    eaGiveUnits:      Result := Result +
                                PlayerExists +
                                IfThen(not (TUnitType(fAction.Params[0]) in [HUMANS_MIN..HUMANS_MAX]), 'Unit type is invalid|') +
                                IfThen(not fTerrain.TileInMapCoords(fAction.Params[1], fAction.Params[2]), 'Units should be placed within map coords|') +
                                IfThen(not InRange(fAction.Params[3], 1, 99), 'Unit count should be within 1..99|');
    eaGiveWares:      Result := Result +
                                PlayerExists +
                                IfThen(not (TResourceType(fAction.Params[0]) in [WARE_MIN..WARE_MAX]), 'Ware type is invalid|') +
                                IfThen(not InRange(fAction.Params[1], 1, 9999), 'Ware count should be within 1..9999|');
    eaShowMessage:    Result := Result +
                                PlayerExists;
    eaUnlockHouse:    Result := Result +
                                PlayerExists +
                                IfThen(not (THouseType(fAction.Params[0]) in [HOUSE_MIN..HOUSE_MAX]), 'House type is invalid|');
    eaVictory:        Result := Result +
                                PlayerExists;
  end;
end;


//We dont care about MP synchronisation because all the events happen simultaneously
//and are exactly the same for all MP players
function TKMEvent.Handle(aTrigger: TKMTrigger): Boolean;
var
  I: Integer;
  Store: TKMHouseStore;
begin
  Result := (aTrigger.Trigger = fTrigger.Trigger) and (aTrigger.Player = fTrigger.Player);
  for I := 0 to TriggerParamCount[aTrigger.Trigger] - 1 do
    Result := Result and (fTrigger.Params[I] = aTrigger.Params[I]);

  if Result then
  case fAction.Action of
    eaDelayedMessage: fOwner.AddEvent(MakeTrigger(etTime, -1, [fGame.GameTickCount + Cardinal(fAction.Params[0])]), MakeAction(eaShowMessage, fAction.Player, [fAction.Params[1]]));
    eaGiveUnits:      fPlayers[fAction.Player].AddUnitGroup(TUnitType(fAction.Params[0]), KMPoint(fAction.Params[1], fAction.Params[2]), dir_N, Round(Sqrt(fAction.Params[3])), fAction.Params[3]);
    eaGiveWares:      begin
                        Store := TKMHouseStore(fPlayers[fAction.Player].FindHouse(ht_Store));
                        if Store <> nil then
                          Store.ResAddToIn(TResourceType(fAction.Params[0]), fAction.Params[1]);
                      end;
    eaShowMessage:    if MyPlayer.PlayerIndex = fAction.Player then
                        fGame.ShowMessage(mkText, fTextLibrary.GetMissionString(fAction.Params[0]), KMPoint(0,0));
    eaUnlockHouse:    fPlayers[fAction.Player].Stats.HouseGranted[THouseType(fAction.Params[0])] := True;
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
