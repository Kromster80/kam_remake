unit KM_GameInputProcess_Multi;
{$I KaM_Remake.inc}
interface
uses SysUtils, KM_GameInputProcess, KM_Networking, KM_Defaults, KM_CommonTypes;

type TKMDataType = (kdp_Commands, kdp_ConfirmCommands);

const MAX_SCHEDULE = 32; //How many turns to plan ahead (3.2sec)


type
  TGIPList = class
    private
      fCount:integer;
      fList:array of TGameInputCommand; //1..n
    public
      procedure Clear;
      procedure Add(aCommand:TGameInputCommand);
      function  Get(aIndex:integer):TGameInputCommand;
      property Count:integer read fCount;
      procedure Save(aStream:TKMemoryStream);
  end;


type
  TGameInputProcess_Multi = class(TGameInputProcess)
    private
      fNetworking:TKMNetworking;
      fDelay:word; //How many ticks ahead the commands are scheduled
      fPlayersEnabled:array[1..MAX_PLAYERS]of boolean; //See which players confirmations do we need
      //Each player can have any number of commands scheduled for execution in one tick
      fSchedule: array[0..MAX_SCHEDULE-1, 1..MAX_PLAYERS] of TGIPList; //Ring buffer
      //All players must confirm they have recieved our GIPList
      fConfirmed: array[0..MAX_SCHEDULE-1, 1..MAX_PLAYERS] of boolean; //Ring buffer
      procedure SendCommands(aTick:cardinal);
    protected
      procedure TakeCommand(aCommand:TGameInputCommand); override;
    public
      constructor Create(aReplayState:TGIPReplayState; aNetworking:TKMNetworking);
      destructor Destroy; override;
      procedure RecieveCommands(aData:TKMemoryStream); //Called by TKMNetwork when it has data for us
      procedure Timer(aTick:cardinal); override;
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
  fDelay := 10;

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


procedure TGameInputProcess_Multi.TakeCommand(aCommand:TGameInputCommand);
var Tick:integer;
begin
  Assert(fDelay < MAX_SCHEDULE, 'Error, fDelay >= MAX_SCHEDULE');

  Tick := (fGame.GetTickCount + fDelay) mod MAX_SCHEDULE; //Place in a ring buffer

  fSchedule[Tick,byte(aCommand.PlayerID)].Add(aCommand);
  FillChar(fConfirmed[Tick], SizeOf(fConfirmed[Tick]), #0); //Reset
end;


procedure TGameInputProcess_Multi.SendCommands(aTick:cardinal);
var Tick:integer; Msg:TKMemoryStream;
begin
  Tick := (aTick + fDelay) mod MAX_SCHEDULE;

  Msg := TKMemoryStream.Create;
  Msg.Write(byte(kdp_Commands));
  Msg.Write(Tick); //Target Tick

  fSchedule[Tick,1].Save(Msg); //Write all commands to the stream

  //Send the commands
  fNetworking.SendCommands(Msg); //Will send to all the opponents

  //Now we wait for the Tick and hopefully everyone will confirm to have recieved our commands
  Msg.Free;
end;


//Decode recieved messages (Commands from other players, Confirmations, Errors)
procedure TGameInputProcess_Multi.RecieveCommands(aData:TKMemoryStream);
begin
  //Decode header

  //case Command of TKMDataType
end;


//Timer is called after all commands from player are taken,
//upcoming commands will be stacked into next batch
procedure TGameInputProcess_Multi.Timer(aTick:cardinal);
var i,k,Tick,CRC:integer; Conf:boolean; Signature: TGameInputCommand;
begin
  Inherited;
  if ReplayState = gipReplaying then exit;

  //Send commands to other players
  Tick := aTick mod MAX_SCHEDULE; //Place in a ring buffer
  Signature := MakeCommand(gic_CRC, [Random(maxint)]);
  TakeCommand(Signature);
  SendCommands(aTick); //Send all commands including CRC signature
  fConfirmed[Tick,byte(Signature.PlayerID)] := true; //Confirm own commands by self

  //Process recieved and confirmed commands
  Conf := true; //See if all players confirm this tick commands list
  for i:=1 to MAX_PLAYERS do
    Conf := Conf and (fConfirmed[Tick,i] or not fPlayersEnabled[i]);

  //Handle the issue
  if not Conf then
  begin
    Assert(false,'Unconfirmed command');
    //todo: handle delay with fGame?
  end;

  //Execute commands, in order players go (1,2,3..)
  CRC := -1;
  for i:=1 to MAX_PLAYERS do
    for k:=1 to fSchedule[Tick,i].Count do
    begin
      if fPlayersEnabled[k] then begin //Skip missing players
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
      FillChar(fConfirmed[Tick], SizeOf(fConfirmed[Tick]), #0); //Reset
    end;     
end;


end.
