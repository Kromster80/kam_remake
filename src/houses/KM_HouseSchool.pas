unit KM_HouseSchool;
{$I KaM_Remake.inc}
interface
uses
  Math,
  KM_CommonClasses, KM_Defaults,
  KM_ResHouses, KM_ResWares, KM_Houses;


type
  //School has one unique property - queue of units to be trained, 1 wip + 5 in line
  TKMHouseSchool = class(TKMHouse)
  private
    fUnitWip: Pointer;  //can't replace with TKMUnit since it will lead to circular reference in KM_House-KM_Units
    fHideOneGold: Boolean; //Hide the gold incase Player cancels the training, then we won't need to tweak DeliverQueue order
    fTrainProgress: Byte; //Was it 150 steps in KaM?
    fQueue: array [0..5] of TUnitType;
    function GetQueue(aIndex: Integer): TUnitType; //Used in UI. First item is the unit currently being trained, 1..5 are the actual queue
    procedure CreateUnit; //This should Create new unit and start training cycle
    procedure StartTrainingUnit; //This should Create new unit and start training cycle
    procedure CancelTrainingUnit;
  public
    constructor Create(aUID: Integer; aHouseType: THouseType; PosX, PosY: Integer; aOwner: TKMHandIndex; aBuildState: THouseBuildState);
    constructor Load(LoadStream: TKMemoryStream); override;
    procedure SyncLoad; override;
    procedure DemolishHouse(aFrom: TKMHandIndex; IsSilent: Boolean = False); override;
    procedure ResAddToIn(aWare: TWareType; aCount: Word = 1; aFromScript: Boolean = False); override;
    function AddUnitToQueue(aUnit: TUnitType; aCount: Byte): Byte; //Should add unit to queue if there's a place
    procedure ChangeUnitTrainOrder(aNewPosition: Integer); overload; //Change last unit in queue training order
    procedure ChangeUnitTrainOrder(aOldPosition, aNewPosition: Integer); overload; //Change unit order in queue
    procedure RemUnitFromQueue(aID: Byte); //Should remove unit from queue and shift rest up
    procedure UnitTrainingComplete(aUnit: Pointer); //This should shift queue filling rest with ut_None
    function GetTrainingProgress: Single;
    function QueueCount: Byte;
    function LastUnitPosInQueue: Integer;
    function QueueIsEmpty: Boolean;
    function QueueIsFull: Boolean;
    function QueueLength: Byte;
    property HideOneGold: Boolean read fHideOneGold;
    property Queue[aIndex: Integer]: TUnitType read GetQueue;
    procedure Save(SaveStream: TKMemoryStream); override;
  end;


implementation
uses
  KM_Units, KM_HandsCollection, KM_Hand;


{ TKMHouseSchool }
constructor TKMHouseSchool.Create(aUID: Integer; aHouseType: THouseType; PosX, PosY: Integer; aOwner: TKMHandIndex; aBuildState: THouseBuildState);
var
  I: Integer;
begin
  inherited;

  for I := 0 to High(fQueue) do
    fQueue[I] := ut_None;
end;


constructor TKMHouseSchool.Load(LoadStream: TKMemoryStream);
begin
  inherited;
  LoadStream.Read(fUnitWip, 4);
  LoadStream.Read(fHideOneGold);
  LoadStream.Read(fTrainProgress);
  LoadStream.Read(fQueue, SizeOf(fQueue));
end;


procedure TKMHouseSchool.SyncLoad;
begin
  Inherited;
  fUnitWip := gHands.GetUnitByUID(Cardinal(fUnitWip));
end;


//Remove all queued units first, to avoid unnecessary shifts in queue
procedure TKMHouseSchool.DemolishHouse(aFrom: TKMHandIndex; IsSilent: Boolean = False);
var
  I: Integer;
begin
  for I := 1 to High(fQueue) do
    fQueue[I] := ut_None;
  RemUnitFromQueue(0); //Remove WIP unit

  inherited;
end;


//Add resource as usual and initiate unit training
procedure TKMHouseSchool.ResAddToIn(aWare: TWareType; aCount: Word = 1; aFromScript: Boolean = False);
begin
  inherited;

  if fUnitWip = nil then
    StartTrainingUnit;
end;


//Add units to training queue
//aCount allows to add several units at once (but not more than Schools queue can fit)
//Returns the number of units successfully added to the queue
function TKMHouseSchool.AddUnitToQueue(aUnit: TUnitType; aCount: Byte): Byte;
var
  I, K: Integer;
begin
  Result := 0;
  for K := 1 to Min(aCount, Length(fQueue)) do
  for I := 1 to High(fQueue) do
  if fQueue[I] = ut_None then
  begin
    Inc(Result);
    fQueue[I] := aUnit;
    if I = 1 then
      StartTrainingUnit; //If thats the first unit then start training it
    Break;
  end;
end;


procedure TKMHouseSchool.CancelTrainingUnit;
begin
  SetState(hst_Idle);
  if fUnitWip <> nil then
  begin //Make sure unit started training
    TKMUnit(fUnitWip).CloseUnit(False); //Don't remove tile usage, we are inside the school
    fHideOneGold := False;
  end;
  fUnitWip := nil;
  fQueue[0] := ut_None; //Removed the in training unit
end;

//Change unit priority in training queue
procedure TKMHouseSchool.ChangeUnitTrainOrder(aNewPosition: Integer);
begin
  ChangeUnitTrainOrder(max(LastUnitPosInQueue, 1), aNewPosition);
end;

//Change unit priority in training queue
procedure TKMHouseSchool.ChangeUnitTrainOrder(aOldPosition, aNewPosition: Integer);
var tmpUnit: TUnitType;
  I: Byte;
begin
  Assert((aNewPosition >= 0) and (aOldPosition <= 5));

  if aOldPosition = 0 then Exit;

  // Do not cancel current training process, if unit type is the same.
  // Or set newPos to 1, if there is no training now (no gold, for example)
  if (aNewPosition = 0) and ((fQueue[aOldPosition] = fQueue[0]) or (fQueue[0] = ut_None)) then
    aNewPosition := 1;

  if (fQueue[aOldPosition] = ut_None) or (aOldPosition = aNewPosition) then Exit;

  Assert(aNewPosition < aOldPosition);

  tmpUnit := fQueue[aOldPosition];
  for I := aOldPosition downto max(aNewPosition, 0)+1 do
  begin
    fQueue[I] := fQueue[I-1];
  end;

  if (aNewPosition = 0) then
    CancelTrainingUnit;

  fQueue[aNewPosition] := tmpUnit;

  if (aNewPosition = 0) then
    CreateUnit;
end;


//DoCancelTraining and remove untrained unit
procedure TKMHouseSchool.RemUnitFromQueue(aID: Byte);
var
  I: Integer;
begin
  if fQueue[aID] = ut_None then Exit; //Ignore clicks on empty queue items

  if aID = 0 then
  begin
    CancelTrainingUnit;
    StartTrainingUnit; //Start on the next unit in the queue
  end
  else
  begin
    for I := aID to High(fQueue) - 1 do
      fQueue[I] := fQueue[I+1]; //Shift by one
    fQueue[High(fQueue)] := ut_None; //Set the last one empty
  end;
end;


procedure TKMHouseSchool.CreateUnit;
begin
  fHideOneGold := true;

  //Create the Unit
  fUnitWip := gHands[fOwner].TrainUnit(fQueue[0], GetEntrance);
  TKMUnit(fUnitWip).TrainInHouse(Self); //Let the unit start the training task

  WorkAnimStep := 0;
end;


procedure TKMHouseSchool.StartTrainingUnit;
var I: Integer;
begin
  if fQueue[0] <> ut_None then exit; //If there's currently no unit in training
  if fQueue[1] = ut_None then exit; //If there is a unit waiting to be trained
  if CheckResIn(wt_Gold) = 0 then exit; //There must be enough gold to perform training

  for I := 0 to High(fQueue) - 1 do
    fQueue[I] := fQueue[I+1]; //Shift by one
  fQueue[High(fQueue)] := ut_None; //Set the last one empty

  CreateUnit;
end;


//Unit reports back to School that it was trained
procedure TKMHouseSchool.UnitTrainingComplete(aUnit: Pointer);
begin
  Assert(aUnit = fUnitWip, 'Should be called only by Unit itself when it''s trained');

  fUnitWip := nil;
  fQueue[0] := ut_None; //Clear the unit in training
  //Script command might have taken the gold while we were training, in which case ignore it (either way, gold is consumed)
  if CheckResIn(wt_Gold) > 0 then
  begin
    ResTakeFromIn(wt_Gold); //Do the goldtaking
    gHands[fOwner].Stats.WareConsumed(wt_Gold);
  end;
  fHideOneGold := False;
  fTrainProgress := 0;

  //Attempt to start training next unit in queue
  StartTrainingUnit;
end;


//Return training progress of a unit in 0 - 1 range
function TKMHouseSchool.GetQueue(aIndex: Integer): TUnitType;
begin
  Result := fQueue[aIndex];
end;


function TKMHouseSchool.GetTrainingProgress: Single;
begin
  if fUnitWip = nil then
    Result := 0
  else
    Result := (
              Byte(ha_Work2 in fCurrentAction.SubAction) * 30 +
              Byte(ha_Work3 in fCurrentAction.SubAction) * 60 +
              Byte(ha_Work4 in fCurrentAction.SubAction) * 90 +
              Byte(ha_Work5 in fCurrentAction.SubAction) * 120 +
              Byte(fCurrentAction.State = hst_Work) * WorkAnimStep
              ) / 150;
end;


function TKMHouseSchool.QueueIsEmpty: Boolean;
var
  I: Integer;
begin
  Result := True;
  for I := 0 to High(fQueue) do
    Result := Result and (fQueue[I] = ut_None);
end;


function TKMHouseSchool.QueueCount: Byte;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to High(fQueue) do
    if fQueue[I] <> ut_None then
      Inc(Result);
end;


// Returns position of the last unit in queue.
// If queue is empty, return -1
function TKMHouseSchool.LastUnitPosInQueue: Integer;
var I: Integer;
begin
  Result := -1;
  for I := 0 to High(fQueue) do
    if fQueue[I] <> ut_None then
      Result := I
end;


function TKMHouseSchool.QueueIsFull: Boolean;
begin
  Result := (fQueue[High(fQueue)] <> ut_None);
end;


function TKMHouseSchool.QueueLength: Byte;
begin
  Result := Length(fQueue);
end;


procedure TKMHouseSchool.Save(SaveStream: TKMemoryStream);
begin
  inherited;
  if TKMUnit(fUnitWip) <> nil then
    SaveStream.Write(TKMUnit(fUnitWip).UID) //Store ID, then substitute it with reference on SyncLoad
  else
    SaveStream.Write(Integer(0));
  SaveStream.Write(fHideOneGold);
  SaveStream.Write(fTrainProgress);
  SaveStream.Write(fQueue, SizeOf(fQueue));
end;


end.
