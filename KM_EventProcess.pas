unit KM_EventProcess;
{$I KaM_Remake.inc}
interface
uses
  Classes, SysUtils,
  KM_Defaults, KM_Points;


const
  MAX_EVENT_PARAMS = 2;

type
  TEventTrigger = (etTime, etHouseBuilt);

  TEventAction = (eaMessage);

  TKMEvent = class
  private
    //What sets the event off (usually time, or house built in tutorial)
    //could be AND-array later if we need (if Trigger[0] and Trigger[1] then)
    fTrigger: TEventTrigger;
    fTriggerParams: array [0..MAX_EVENT_PARAMS-1] of Integer;

    //What happens (player index is another param)
    fAction: TEventAction;
    fActionParams: array [0..MAX_EVENT_PARAMS-1] of Integer;

    //When event has occurred it is simply removed
  public
    constructor Create(aTrigger: TEventTrigger; aTriggerParams: array of Integer; aAct: TEventAction; aActParams: array of Integer);
    function Handle(aTrigger: TEventTrigger; aParams: array of Integer): Boolean;
  end;

  TKMEventsManager = class
  private
    fEvents: TList;

    function GetEvent(aIndex: Integer): TKMEvent;
    property Events[aIndex: Integer]: TKMEvent read GetEvent;
    procedure Proc(aTrigger: TEventTrigger; aParams: array of Integer);
  public
    constructor Create;
    destructor Destroy; override;

    procedure AddEvent(aTrigger: TEventTrigger; aTriggerParams: array of Integer; aAct: TEventAction; aActParams: array of Integer);

    //Process game events
    procedure ProcTime(aTick: Cardinal);
    procedure ProcHouseBuilt(aHouseType: THouseType; aOwner: TPlayerIndex);

    procedure LoadFromFile(aFilename: string);
    procedure SaveToFile(aFilename: string);
  end;


implementation
uses KM_Game, KM_MessageStack, KM_PlayersCollection, KM_TextLibrary;


{ TKMEventsManager }
constructor TKMEventsManager.Create;
begin
  inherited;

  fEvents := TList.Create;

  AddEvent(etHouseBuilt, [Byte(ht_School), 0], eaMessage, [2, 0]);
  AddEvent(etTime, [10, -1], eaMessage, [1, 0]);
  AddEvent(etTime, [20, -1], eaMessage, [2, 0]);
  AddEvent(etTime, [30, -1], eaMessage, [3, 0]);
end;


destructor TKMEventsManager.Destroy;
var I: Integer;
begin
  for I := 0 to fEvents.Count - 1 do
    Events[I].Free;

  fEvents.Free;
  inherited;
end;


procedure TKMEventsManager.AddEvent(aTrigger: TEventTrigger; aTriggerParams: array of Integer; aAct: TEventAction; aActParams: array of Integer);
var E: TKMEvent;
begin
  E := TKMEvent.Create(aTrigger, aTriggerParams, aAct, aActParams);
  fEvents.Add(E);
end;


procedure TKMEventsManager.Proc(aTrigger: TEventTrigger; aParams: array of Integer);
var I: Integer;
begin
  //Process in reverse as we delete handled events
  for I := fEvents.Count - 1 downto 0 do
  if Events[I].Handle(aTrigger, aParams) then
    fEvents.Delete(I);
end;


procedure TKMEventsManager.ProcHouseBuilt(aHouseType: THouseType; aOwner: TPlayerIndex);
begin
  Proc(etHouseBuilt, [Byte(aHouseType), aOwner]);
end;


procedure TKMEventsManager.ProcTime(aTick: Cardinal);
begin
  Proc(etTime, [aTick, -1]);
end;


function TKMEventsManager.GetEvent(aIndex: Integer): TKMEvent;
begin
  Result := fEvents[aIndex];
end;


procedure TKMEventsManager.LoadFromFile(aFilename: string);
begin

end;


procedure TKMEventsManager.SaveToFile(aFilename: string);
begin

end;


{ TKMEvent }
constructor TKMEvent.Create(aTrigger: TEventTrigger; aTriggerParams: array of Integer; aAct: TEventAction; aActParams: array of Integer);
var I: Integer;
begin
  inherited Create;

  Assert(Length(aTriggerParams) = Length(fTriggerParams));

  fTrigger := aTrigger;
  for I := 0 to High(fTriggerParams) do
    fTriggerParams[I] := aTriggerParams[I];

  fAction := aAct;
  for I := 0 to High(fActionParams) do
    fActionParams[I] := aActParams[I];
end;


function TKMEvent.Handle(aTrigger: TEventTrigger; aParams: array of Integer): Boolean;
var I: Integer;
begin
  Result := (fTrigger = aTrigger);
  for I := Low(aParams) to High(aParams) do
    Result := Result and (fTriggerParams[I] = aParams[I]);

  if Result then
  case fAction of
    eaMessage:  if MyPlayer.PlayerIndex = fActionParams[1] then
                  fGame.fGamePlayInterface.MessageIssue(mkText, fTextLibrary[fActionParams[0]], KMPoint(0,0));
  end;
end;


end.
