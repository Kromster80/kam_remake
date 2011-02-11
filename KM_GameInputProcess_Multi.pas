unit KM_GameInputProcess_Multi;
{$I KaM_Remake.inc}
interface
uses SysUtils, KM_GameInputProcess, KM_Network, KM_Defaults;

type TKMDataType = (kdp_Commands, kdp_ConfirmCommands);

type TKMDataPacket = record
       PlayerID: byte; //Player who sent the data
       DataType: TKMDataType;
       DataLength: Integer;
       Data: pchar; //e.g. Commands, tick ID that was confirmed. This could be another record for each command type?
     end;


const MAX_SCHEDULE = 32; //How many turns to plan ahead (3.2sec)

type
  TGIPList = class
    private
     fCount:integer;
     fList:array of TGameInputCommand; //1..n
    public
      procedure Clear;
      property Count:integer read fCount;
      procedure Add(aCommand:TGameInputCommand);
      function Get(aIndex:integer):TGameInputCommand;
  end;

type
  TGameInputProcess_Multi = class(TGameInputProcess)
    private
      fNetwork: TKMNetwork;
      fDelay:word; //How many ticks ahead the commands are scheduled
      fPlayersEnabled:array[1..MAX_PLAYERS]of boolean; //See which players confirmations do we need
      //Each player can have any number of commands scheduled for execution in one tick
      fSchedule: array[0..MAX_SCHEDULE-1, 1..MAX_PLAYERS] of TGIPList; //Ring buffer
      //All players must confirm they have the have recieved our GIPList
      fConfirmed: array[0..MAX_SCHEDULE-1, 1..MAX_PLAYERS] of boolean; //Ring buffer
      procedure SendCommands;
    protected
      procedure TakeCommand(aCommand:TGameInputCommand); override;
    public
      constructor Create(aReplayState:TGIPReplayState);
      destructor Destroy; override;
      procedure RecieveCommands(aData:string); //Called by TKMNetwork when it has data for us
      procedure Tick(aTick:cardinal); override;
  end;


implementation
uses KM_Game, KM_CommonTypes;


procedure TGIPList.Clear;
begin
  fCount := 0;
end;


function TGIPList.Get(aIndex:integer):TGameInputCommand;
begin
  Result := fList[aIndex];
end;


procedure TGIPList.Add(aCommand:TGameInputCommand);
begin
  inc(fCount);
  if fCount >= length(fList) then
    setlength(fList, fCount+8);

  fList[fCount] := aCommand;
end;


constructor TGameInputProcess_Multi.Create(aReplayState:TGIPReplayState);
var i,k:integer;
begin
  Inherited Create(aReplayState);
  fDelay := 10;

  //todo: This should not be managed by GIP
  fNetwork := TKMNetwork.Create(MULTIPLE_COPIES);

  //Allocate memory for all commands lists
  for i:=0 to MAX_SCHEDULE-1 do for k:=1 to MAX_PLAYERS do
    fSchedule[i,k] := TGIPList.Create;
end;


destructor TGameInputProcess_Multi.Destroy;
var i,k:integer;
begin
  for i:=0 to MAX_SCHEDULE-1 do for k:=1 to MAX_PLAYERS do
    fSchedule[i,k].Free;
  fNetwork.Free;
  Inherited;
end;


procedure TGameInputProcess_Multi.TakeCommand(aCommand:TGameInputCommand);
var id:integer;
begin
  Assert(fDelay < MAX_SCHEDULE, 'Error, fDelay >= MAX_SCHEDULE');

  id := (fGame.GetTickCount + fDelay) mod MAX_SCHEDULE; //Place in a ring buffer

  fSchedule[id,byte(aCommand.PlayerID)].Add(aCommand);
  FillChar(fConfirmed[id], SizeOf(fConfirmed[id]), #0); //Reset
end;


procedure TGameInputProcess_Multi.SendCommands;
var i,id:integer; Msg:TKMemoryStream;
begin
  id := (fGame.GetTickCount + fDelay) mod MAX_SCHEDULE;

  Msg := TKMemoryStream.Create;
  Msg.Write(id); //Target Tick

  Msg.Write(fSchedule[id,1].Count); //todo: put our player id here

{  for i:=1 to fSchedule[id,1].Count do
  begin
    fSchedule[id,1].Get(i).CommandType

  end;}

  //Now we just wait till the tick and hopefully everyone will confirm
  //to have recieved our commands
end;


procedure TGameInputProcess_Multi.RecieveCommands(aData:string);
begin
  //todo: Decode recieved messages (Commands from other players, Confirmations, Errors)
end;


procedure TGameInputProcess_Multi.Tick(aTick:cardinal);
var i,k,id,CRC:integer; Conf:boolean; ConfCommand: TGameInputCommand;
begin
  Inherited;
  if ReplayState = gipReplaying then exit;

  //Tick is called after all commands from player are taken,
  //upcoming commands will be stacked into next batch
  SendCommands;

  id := fGame.GetTickCount mod MAX_SCHEDULE; //Place in a ring buffer

  ConfCommand := MakeCommand(gic_CRC, [Random(maxint)]);
  TakeCommand(ConfCommand);
  fConfirmed[id,byte(ConfCommand.PlayerID)] := true; //Confirm own commands by self

  Conf := true; //See if all players confirm this tick commands list
  for i:=1 to MAX_PLAYERS do
    Conf := Conf and (fConfirmed[id,i] or not fPlayersEnabled[i]);

  //Handle the issue
  if not Conf then
  begin
    Assert(false,'Unconfirmed command');
    //todo: handle delay with fGame?
  end;

  //Commands are executed in order players go
  CRC := -1;
  if Conf then
  for i:=1 to MAX_PLAYERS do
    for k:=1 to fSchedule[id,i].Count do
    begin
      if fPlayersEnabled[k] then begin //Skip missing players
        if fSchedule[id,i].Get(k).CommandType = gic_CRC then
        begin
          if (CRC <> -1) and (CRC <> fSchedule[id,i].Get(k).Params[1]) then
            assert(false, 'CRC failed for player '+IntToStr(i));
          CRC := fSchedule[id,i].Get(k).Params[1];
        end
        else
        begin
          ExecCommand(fSchedule[id,i].Get(k));
          StoreCommand(fSchedule[id,i].Get(k));
        end;
      end;
      fSchedule[id,i].Clear;
      FillChar(fConfirmed[id], SizeOf(fConfirmed[id]), #0); //Reset
    end;

end;


end.
