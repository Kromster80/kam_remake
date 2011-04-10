unit KM_GameInputProcess_Multi;
{$I KaM_Remake.inc}
interface
uses SysUtils, KM_GameInputProcess, KM_Networking, KM_Defaults, KM_CommonTypes;

const
  MAX_SCHEDULE = 32; //How many turns to plan ahead (3.2sec)

type
  TKMDataType = (kdp_Commands, kdp_ConfirmCommands);


  TGIPList = class
    private
      fCount:integer;
      fList:array of TGameInputCommand; //1..n
    public
      procedure Clear;
      procedure Add(aCommand:TGameInputCommand);
      function  Get(aIndex:integer):TGameInputCommand;
      property  Count:integer read fCount;
      procedure Save(aStream:TKMemoryStream);
  end;


  TGameInputProcess_Multi = class (TGameInputProcess)
    private
      fNetworking:TKMNetworking;
      fDelay:word; //How many ticks ahead the commands are scheduled
      fPlayerEnabled:array[1..MAX_PLAYERS]of boolean; //See which players confirmations do we need

      //Each player can have any number of commands scheduled for execution in one tick
      fSchedule: array[0..MAX_SCHEDULE-1, 1..MAX_PLAYERS] of TGIPList; //Ring buffer

      //All players must confirm they have recieved our GIPList
      fConfirmation: array[0..MAX_SCHEDULE-1, 1..MAX_PLAYERS] of boolean; //Ring buffer
      procedure SendCommands(aTick:cardinal);
    protected
      procedure TakeCommand(aCommand:TGameInputCommand); override;
    public
      constructor Create(aReplayState:TGIPReplayState; aNetworking:TKMNetworking);
      destructor Destroy; override;
      procedure RecieveCommands(const aData:string); //Called by TKMNetwork when it has data for us
      function TickReady(aTick:cardinal):boolean; override;
      procedure Timer(aTick:cardinal); override;
      procedure UpdateState(aTick:cardinal); override;
  end;


implementation
uses KM_Game;


{ TGIPList }
procedure TGIPList.Clear;
begin
  fCount := 0;
end;


procedure TGIPList.Add(aCommand:TGameInputCommand);
begin
  inc(fCount);
  if fCount >= length(fList) then
    setlength(fList, fCount+8);

  fList[fCount] := aCommand;
end;


function TGIPList.Get(aIndex:integer):TGameInputCommand;
begin
  Result := fList[aIndex];
end;


procedure TGIPList.Save(aStream:TKMemoryStream);
var i:integer;
begin
  aStream.Write(fCount);
  for i:=1 to fCount do
  begin
    aStream.Write(fList[i].CommandType, SizeOf(fList[i].CommandType));
    aStream.Write(fList[i].Params[1]);
    aStream.Write(fList[i].Params[2]);
    aStream.Write(fList[i].Params[3]);
    aStream.Write(fList[i].Params[4]);
    //Skip Players ID
  end;               
end;


{ TGameInputProcess_Multi }
constructor TGameInputProcess_Multi.Create(aReplayState:TGIPReplayState; aNetworking:TKMNetworking);
var i,k:integer;
begin
  Inherited Create(aReplayState);
  fNetworking := aNetworking;
  fNetworking.OnCommands := RecieveCommands;
  fDelay := 10; //1sec

  //Allocate memory for all commands lists
  for i:=0 to MAX_SCHEDULE-1 do for k:=1 to MAX_PLAYERS do
    fSchedule[i,k] := TGIPList.Create;
end;


destructor TGameInputProcess_Multi.Destroy;
var i,k:integer;
begin
  for i:=0 to MAX_SCHEDULE-1 do for k:=1 to MAX_PLAYERS do
    fSchedule[i,k].Free;
  Inherited;
end;


//Stack the command into schedule
procedure TGameInputProcess_Multi.TakeCommand(aCommand:TGameInputCommand);
var Tick:integer;
begin
  Assert(fDelay < MAX_SCHEDULE, 'Error, fDelay >= MAX_SCHEDULE');

  Tick := (fGame.GameTickCount + fDelay) mod MAX_SCHEDULE; //Place in a ring buffer

  fSchedule[Tick,byte(aCommand.PlayerID)].Add(aCommand);
  FillChar(fConfirmation[Tick], SizeOf(fConfirmation[Tick]), #0); //Reset
end;


procedure TGameInputProcess_Multi.SendCommands(aTick:cardinal);
var Tick:integer; Msg:TKMemoryStream;
begin
  Tick := (aTick + fDelay) mod MAX_SCHEDULE;

  Msg := TKMemoryStream.Create;
  Msg.Write(byte(kdp_Commands));
  Msg.Write(Tick); //Target Tick

  //todo: not 1
  fSchedule[Tick,1].Save(Msg); //Write all commands to the stream

  //Send the commands
  fNetworking.SendCommands(Msg); //Will send to all the opponents

  //Now we wait for the Tick and hopefully everyone will confirm to have recieved our commands
  Msg.Free;
end;


//Decode recieved messages (Commands from other players, Confirmations, Errors)
procedure TGameInputProcess_Multi.RecieveCommands(const aData:string);
begin
  //Decode header

  //case Command of TKMDataType
end;


//Are all the commands are confirmed?
function TGameInputProcess_Multi.TickReady(aTick:cardinal):boolean;
var i:integer;
begin
  Result := True;
  for i:=1 to MAX_PLAYERS do
    Result := Result and (fConfirmation[aTick,i] or not fPlayerEnabled[i]);
end;


//Timer is called after all commands from player are taken,
//upcoming commands will be stacked into next batch
procedure TGameInputProcess_Multi.Timer(aTick:cardinal);
var i,k,Tick,CRC:integer;
begin
  Inherited;
  Assert(ReplayState <> gipReplaying);

  Tick := aTick mod MAX_SCHEDULE; //Place in a ring buffer

  //Execute commands, in order players go (1,2,3..)
  CRC := -1;
  for i:=1 to MAX_PLAYERS do
    for k:=1 to fSchedule[Tick,i].Count do
    begin
      if fPlayerEnabled[k] then begin //Skip missing players
        if fSchedule[Tick,i].Get(k).CommandType = gic_CRC then
        begin
          if (CRC <> -1) and (CRC <> fSchedule[Tick,i].Get(k).Params[1]) then
            Assert(false, 'CRC failed for player '+IntToStr(i));
          CRC := fSchedule[Tick,i].Get(k).Params[1];
        end
        else
        begin
          ExecCommand(fSchedule[Tick,i].Get(k));
          StoreCommand(fSchedule[Tick,i].Get(k));
        end;
      end;
      fSchedule[Tick,i].Clear;
      FillChar(fConfirmation[Tick], SizeOf(fConfirmation[Tick]), #0); //Reset
    end;
end;


//Make sure commands starting from (aTick) to (aTick+fDelay) are sent to other players
//Since fDelay can change make sure whole range is sent
procedure TGameInputProcess_Multi.UpdateState(aTick:cardinal);
var i:Integer; Signature:TGameInputCommand;
begin
  //todo: Oops. We can't use randoms here in such a fashion
  //Delays may change unevenly between PCs and whole CRC-random thing will break
  Signature := MakeCommand(gic_CRC, [Random(maxint)]); //Current GameTick CRC signature

  //What we haven't confirmed is what we didn't sent it yet
  for i:=aTick to aTick+fDelay do
  if not fConfirmation[i mod MAX_SCHEDULE,1] then //todo: not 1
  begin
    TakeCommand(Signature); //Add signature to unsent pack
    SendCommands(aTick); //Send all commands, now including CRC signature
    fConfirmation[i mod MAX_SCHEDULE,byte(Signature.PlayerID)] := true; //Confirm own commands
  end;
end;




end.
