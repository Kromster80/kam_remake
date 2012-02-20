unit KM_EventProcess;
{$I KaM_Remake.inc}
interface
uses
  Classes, SysUtils,
  KM_Defaults, KM_Points;


const
  MAX_EVENT_PARAMS = 2;

  //Events are straightforward: trigger -> event
  //When event has occurred it is simply removed

  //Win Defeat conditions are more complicated, they can not be represented as events that easily
  //because they usually consist of several triggers happening in different times and are reversible
  //E.g. for victory we need to: destroy Store, School, Barracks, Town Hall, troops, builders. While
  //at it enemy can rebuild/train them and we would need to add semaphore events for that to work ..

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

  //@Lewin: How is it made in Town Tutorial, do we need to defeat both enemies or just build a Tannery?
  //if not - remove eaVictory, as explained above^

  TKMTrigger = record
    Trigger: TEventTrigger;
    Player: TPlayerIndex;
    Params: array [0..MAX_EVENT_PARAMS-1] of Integer;
  end;

  TKMAction = record
    Action: TEventAction;
    Player: TPlayerIndex;
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
    constructor Create(aOwner: TKMEventsManager; aTrigger: TKMTrigger; aAction: TKMAction);
    function Handle(aTrigger: TKMTrigger): Boolean;
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
    procedure ProcTime(aTick: Cardinal);
    procedure ProcHouseBuilt(aHouseType: THouseType; aOwner: TPlayerIndex);

    procedure LoadFromFile(aFilename: string);
    procedure SaveToFile(aFilename: string);
  end;


implementation
uses KM_Game, KM_MessageStack, KM_PlayersCollection, KM_TextLibrary;


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

  AddEvent(MakeTrigger(etTime, -1, [30]), MakeAction(eaShowMessage, 0, [500]));
  AddEvent(MakeTrigger(etTime, -1, [160]), MakeAction(eaShowMessage, 0, [501]));
  AddEvent(MakeTrigger(etTime, -1, [340]), MakeAction(eaShowMessage, 0, [502]));

  AddEvent(MakeTrigger(etHouseBuilt, 0, [Byte(ht_School)]), MakeAction(eaShowMessage, 0, [503]));
  AddEvent(MakeTrigger(etHouseBuilt, 0, [Byte(ht_School)]), MakeAction(eaDelayedMessage, 0, [100, 504]));

  AddEvent(MakeTrigger(etHouseBuilt, 0, [Byte(ht_Inn)]), MakeAction(eaShowMessage, 0, [505]));
  AddEvent(MakeTrigger(etHouseBuilt, 0, [Byte(ht_Quary)]), MakeAction(eaShowMessage, 0, [506]));
  AddEvent(MakeTrigger(etHouseBuilt, 0, [Byte(ht_Woodcutters)]), MakeAction(eaShowMessage, 0, [507]));
  AddEvent(MakeTrigger(etHouseBuilt, 0, [Byte(ht_Sawmill)]), MakeAction(eaShowMessage, 0, [508]));
  AddEvent(MakeTrigger(etHouseBuilt, 0, [Byte(ht_FisherHut)]), MakeAction(eaShowMessage, 0, [509]));
  AddEvent(MakeTrigger(etHouseBuilt, 0, [Byte(ht_Farm)]), MakeAction(eaShowMessage, 0, [510]));
  AddEvent(MakeTrigger(etHouseBuilt, 0, [Byte(ht_Mill)]), MakeAction(eaShowMessage, 0, [511]));
  AddEvent(MakeTrigger(etHouseBuilt, 0, [Byte(ht_Bakery)]), MakeAction(eaShowMessage, 0, [512]));
  AddEvent(MakeTrigger(etHouseBuilt, 0, [Byte(ht_Wineyard)]), MakeAction(eaShowMessage, 0, [513]));
  AddEvent(MakeTrigger(etHouseBuilt, 0, [Byte(ht_Swine)]), MakeAction(eaShowMessage, 0, [514]));
  AddEvent(MakeTrigger(etHouseBuilt, 0, [Byte(ht_Butchers)]), MakeAction(eaShowMessage, 0, [515]));

  AddEvent(MakeTrigger(etHouseBuilt, 0, [Byte(ht_Barracks)]), MakeAction(eaShowMessage, 0, [516]));
  AddEvent(MakeTrigger(etHouseBuilt, 0, [Byte(ht_Barracks)]), MakeAction(eaDelayedMessage, 0, [100, 517]));

  AddEvent(MakeTrigger(etHouseBuilt, 0, [Byte(ht_WeaponWorkshop)]), MakeAction(eaShowMessage, 0, [518]));
  AddEvent(MakeTrigger(etHouseBuilt, 0, [Byte(ht_WeaponWorkshop)]), MakeAction(eaDelayedMessage, 0, [100, 519]));

  AddEvent(MakeTrigger(etHouseBuilt, 0, [Byte(ht_Tannery)]), MakeAction(eaShowMessage, 0, [520]));
  AddEvent(MakeTrigger(etHouseBuilt, 0, [Byte(ht_ArmorWorkshop)]), MakeAction(eaShowMessage, 0, [521]));
  AddEvent(MakeTrigger(etHouseBuilt, 0, [Byte(ht_ArmorWorkshop)]), MakeAction(eaDelayedMessage, 0, [100, 522]));

  AddEvent(MakeTrigger(etDefeated, 1, []), MakeAction(eaShowMessage, 0, [523]));
  AddEvent(MakeTrigger(etDefeated, 2, []), MakeAction(eaShowMessage, 0, [524]));
end;


destructor TKMEventsManager.Destroy;
var I: Integer;
begin
  for I := 0 to fEvents.Count - 1 do
    Events[I].Free;

  fEvents.Free;
  inherited;
end;


procedure TKMEventsManager.AddEvent(aTrigger: TKMTrigger; aAction: TKMAction);
var E: TKMEvent;
begin
  E := TKMEvent.Create(Self, aTrigger, aAction);
  fEvents.Add(E);
end;


procedure TKMEventsManager.Proc(aTrigger: TEventTrigger; aPlayer: TPlayerIndex; aParams: array of Integer);
var I: Integer;
begin
  //Process in reverse as we delete handled events
  for I := fEvents.Count - 1 downto 0 do
  if Events[I].Handle(MakeTrigger(aTrigger, aPlayer, aParams)) then
    fEvents.Delete(I);
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


procedure TKMEventsManager.LoadFromFile(aFilename: string);
begin
  //
end;


procedure TKMEventsManager.SaveToFile(aFilename: string);
begin
  //
end;


{ TKMEvent }
constructor TKMEvent.Create(aOwner: TKMEventsManager; aTrigger: TKMTrigger; aAction: TKMAction);
begin
  inherited Create;

  fOwner := aOwner;
  fTrigger := aTrigger;
  fAction := aAction;
end;


function TKMEvent.Handle(aTrigger: TKMTrigger): Boolean;
var I: Integer;
begin
  Result := (aTrigger.Trigger = fTrigger.Trigger) and (aTrigger.Player = fTrigger.Player);
  for I := 0 to High(aTrigger.Params) do
    Result := Result and (fTrigger.Params[I] = aTrigger.Params[I]);

  if Result then
  case fAction.Action of
    eaDelayedMessage: begin
                        fOwner.AddEvent(MakeTrigger(etTime, -1, [fGame.GameTickCount + fAction.Params[0]]), MakeAction(eaShowMessage, fAction.Player, [fAction.Params[1]]))
                      end;
    eaShowMessage:  if MyPlayer.PlayerIndex = fAction.Player then
                      fGame.fGamePlayInterface.MessageIssue(mkText, fTextLibrary[fAction.Params[0]], KMPoint(0,0));
    eaVictory:      fGame.PlayerVictory(fAction.Player);
  end;
end;


end.
