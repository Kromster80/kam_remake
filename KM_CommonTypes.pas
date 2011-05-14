unit KM_CommonTypes;
{$I KaM_Remake.inc}
interface
uses
{$IFDEF MSWindows} MMSystem, {$ENDIF}
Classes, SysUtils, KromUtils;


type
  TKMPoint = record X,Y:word; end;
  TKMPointDir = record Loc:TKMPoint; Dir:word; end;
  TKMPointF = record X,Y:single; end;
  TKMPointI = record X,Y:integer; end; //Allows negative values

const
  NET_ADDRESS_EMPTY = 0;    //Yet undefined
  NET_ADDRESS_ALL = -1;     //Recipient
  NET_ADDRESS_HOST = -2;    //Sender/Recipient
  NET_ADDRESS_SERVER = -3;  //Sender/Recipient

  //Client-Server-Client exchange packets. Each packet is a certain type
type
  TKMessageKind = (
    mk_AskToJoin,       //Client asks Host if he can join
    mk_AllowToJoin,     //Host allows Client to join
    mk_RefuseToJoin,    //Host can refuse when e.g. Nikname is already taken

    mk_IndexOnServer,   //Server tells Client his index
    mk_ClientLost,      //Server tells clients that someone has disconnected

    mk_Disconnect,      //Joiner tells Host that he is leaving the lobby/game deliberately
    mk_HostDisconnect,  //Host tells joiners he is exiting
                        //A. Server runs on the same machine and stops right after
                        //B. Server runs on different machine and assigns Host role to some Client

    mk_AskPingInfo,     //Ask server about ping info
    mk_Ping,            //Server pings Clients
    mk_Pong,            //Clients reply to Server with pong

    mk_PlayersList,     //Host keeps the players list and sends it to everyone on change

    mk_StartingLocQuery,//Joiner asks Host if he can take that starting location
    mk_FlagColorQuery,  //Joiner asks Host if he can take specific color

    mk_MapSelect,       //Host selects the map to play
    mk_ReadyToStart,    //Joiner tells he's ready to play the game
    mk_Start,           //Host says to start the game

    mk_ReadyToPlay,     //Joiner tells Host he has loaded the map and clock can start
    mk_Play,            //Host tells everyone that the game may begin

    mk_Commands,        //Clients exchange commands for next ticks
    mk_Text             //Clients exchange text messages
    );


  TKMPacketFormat = (pfNoData, pfNumber, pfText);

const
  NetPacketType:array[TKMessageKind] of TKMPacketFormat =
  ( pfText,     //mk_AskToJoin
    pfNoData,   //mk_AllowToJoin
    pfText,     //mk_RefuseToJoin
    pfNumber,   //mk_IndexOnServer
    pfNumber,   //mk_ClientLost
    pfNoData,   //mk_Disconnect
    pfNoData,   //mk_HostDisconnect
    pfNoData,   //mk_AskPingInfo
    pfNoData,   //mk_Ping
    pfNoData,   //mk_Pong
    pfText,     //mk_PlayersList
    pfNumber,   //mk_StartingLocQuery
    pfNumber,   //mk_FlagColorQuery
    pfText,     //mk_MapSelect
    pfNoData,   //mk_ReadyToStart
    pfText,     //mk_Start
    pfNoData,   //mk_ReadyToPlay
    pfNoData,   //mk_Play          
    pfText,     //mk_Commands
    pfText      //mk_Text
  );

type
  { Extended with custom Read/Write commands which accept various types without asking for their length}
  TKMemoryStream = class(TMemoryStream)
  public
    procedure Write(const Value:string); reintroduce; overload;
    function Write(const Value:TKMPoint ): Longint; reintroduce; overload;
    function Write(const Value:TKMPointF): Longint; reintroduce; overload;
    function Write(const Value:single   ): Longint; reintroduce; overload;
    function Write(const Value:integer  ): Longint; reintroduce; overload;
    function Write(const Value:cardinal ): Longint; reintroduce; overload;
    function Write(const Value:byte     ): Longint; reintroduce; overload;
    function Write(const Value:boolean  ): Longint; reintroduce; overload;
    function Write(const Value:word     ): Longint; reintroduce; overload;
    function Write(const Value:shortint ): Longint; reintroduce; overload;
    procedure WriteAsText(aText:string);

    procedure Read(out Value:string); reintroduce; overload;
    function Read(out Value:TKMPoint    ): Longint; reintroduce; overload;
    function Read(out Value:TKMPointF   ): Longint; reintroduce; overload;
    function Read(out Value:single      ): Longint; reintroduce; overload;
    function Read(out Value:integer     ): Longint; reintroduce; overload;
    function Read(out Value:cardinal    ): Longint; reintroduce; overload;
    function Read(out Value:byte        ): Longint; reintroduce; overload;
    function Read(out Value:boolean     ): Longint; reintroduce; overload;
    function Read(out Value:word        ): Longint; reintroduce; overload;
    function Read(out Value:shortint    ): Longint; reintroduce; overload;
    function ReadAsText:string;
  end;


  TStringEvent = procedure (const aData: string) of object;
  TStreamEvent = procedure (aData: TKMemoryStream) of object;


type
  {List that clears up its items, used only in Units/Houses}
  TKMList = class(TList)
  public
    procedure Clear; override;
  end;


{Messages}
//number matches pic index in gui.rx
type
  TKMMessageType = (msgUnknown=0, msgText=491, msgHouse, msgUnit, msgHorn, msgQuill, msgScroll);

  TKMMessage = class
  public
    msgType:TKMMessageType;
    msgText:string;
    msgLoc:TKMPoint;
  end;

  TKMMessageList = class
  private
    fCount:cardinal;
    fList:array of TKMMessage; //1..Count
  public
    destructor Destroy; override;
    procedure AddEntry(aMsgTyp:TKMMessageType; aText:string; aLoc:TKMPoint);
    procedure RemoveEntry(aID:cardinal);
    procedure InjectEntry(aID:cardinal; aMsgTyp:TKMMessageType; aText:string);
    function GetMsgPic(aID:cardinal):cardinal;
    function GetMsgType(aID:cardinal):TKMMessageType;
    function GetMsgHasGoTo(aID:cardinal):boolean;
    function GetMsgHasSound(aID:cardinal):boolean;
    function GetText(aID:cardinal):string;
    function GetLoc(aID:cardinal):TKMPoint;
    property Count:cardinal read fCount;
    procedure Save(SaveStream:TKMemoryStream);
    procedure Load(LoadStream:TKMemoryStream);
  end;


  TKMPointList = class
  public
    Count:integer;
    List:array of TKMPoint; //1..Count
    constructor Load(LoadStream:TKMemoryStream); virtual;
    procedure Clearup; virtual;
    procedure AddEntry(aLoc:TKMPoint);
    function  RemoveEntry(aLoc:TKMPoint):cardinal; virtual;
    procedure InjectEntry(ID:integer; aLoc:TKMPoint);
    function  GetRandom:TKMPoint;
    procedure Inverse;
    function  GetTopLeft:TKMPoint;
    function  GetBottomRight:TKMPoint;
    procedure Save(SaveStream:TKMemoryStream); virtual;
  end;


  TKMPointTagList = class(TKMPointList)
  public
    Tag,Tag2:array of integer; //1..Count
    constructor Load(LoadStream:TKMemoryStream); override;
    procedure Clearup; override;
    procedure AddEntry(aLoc:TKMPoint; aTag,aTag2:cardinal); reintroduce;
    function RemoveEntry(aLoc:TKMPoint):cardinal; override;
    procedure Save(SaveStream:TKMemoryStream); override;
  end;


  TKMPointDirList = class //Used for finding fishing places, fighting positions, etc.
  public
    Count:integer;
    List:array of TKMPointDir; //1..Count
    constructor Load(LoadStream:TKMemoryStream);
    procedure Clearup;
    procedure AddEntry(aLoc:TKMPointDir);
    function RemoveEntry(aLoc:TKMPointDir):cardinal;
    procedure InjectEntry(ID:integer; aLoc:TKMPointDir);
    function GetRandom:TKMPointDir;
    function GetNearest(aLoc:TKMPoint):TKMPointDir;
    procedure Save(SaveStream:TKMemoryStream);
  end;


  {This is our custom logging system}
  TKMLog = class
  private
    fl:textfile;
    fLogPath:string;
    fFirstTick:cardinal;
    fPreviousTick:cardinal;
    procedure AddLine(const aText:string);
    procedure AddLineNoTime(const aText:string);
  public
    constructor Create(aPath:string);
    //AppendLog adds the line to Log along with time passed since previous line added
    procedure AppendLog(const aText:string); overload;
    procedure AppendLog(const aText:string; num:integer); overload;
    procedure AppendLog(const aText:string; num:single ); overload;
    procedure AppendLog(num:integer; const aText:string); overload;
    procedure AppendLog(const aText:string; Res:boolean); overload;
    procedure AppendLog(a,b:integer); overload;
    //Add line if TestValue=false
    procedure AssertToLog(TestValue:boolean; const aMessageText:string);
    //AddToLog simply adds the text
    procedure AddToLog(const aText:string);
  end;


  var
    fLog: TKMLog;


implementation
uses KM_Utils;


{Reset log file}
constructor TKMLog.Create(aPath:string);
begin
  Inherited Create;
  fLogPath := aPath;
  fFirstTick := TimeGet();
  fPreviousTick := TimeGet();
  AssignFile(fl, fLogPath);
  Rewrite(fl);
  CloseFile(fl);
  AddLine('Log is up and running');
end;


{Lines are timestamped, each line invokes file open/close for writing,
meaning no lines will be lost if Remake crashes}
procedure TKMLog.AddLine(const aText:string);
begin
  AssignFile(fl, fLogPath);
  Append(fl);
  WriteLn(fl,floattostr((TimeGet() - fFirstTick)/1000)+'s'+#9+
             floattostr((TimeGet() - fPreviousTick)/1000)+'s'+#9+aText);
  CloseFile(fl);
  fPreviousTick := TimeGet();
end;


{Same line but without timestamp}
procedure TKMLog.AddLineNoTime(const aText:string);
begin
  AssignFile(fl, fLogPath);
  Append(fl);
  WriteLn(fl,#9+#9+aText);
  CloseFile(fl);
end;


procedure TKMLog.AppendLog(const aText:string);
begin
  AddLine(aText);
end;


procedure TKMLog.AppendLog(const aText:string; num:integer);
begin
  AddLine(aText+' '+inttostr(num));
end;


procedure TKMLog.AppendLog(const aText:string; num:single);
begin
  AddLine(aText+' '+FloatToStr(num));
end;


procedure TKMLog.AppendLog(num:integer; const aText:string);
begin
  AddLine(inttostr(num)+' '+aText);
end;


procedure TKMLog.AppendLog(const aText:string; Res:boolean);
var s:string;
begin
  if Res then s:='done' else s:='fail';
  AddLine(aText+' ... '+s);
end;


procedure TKMLog.AppendLog(a,b:integer);
begin
  AddLine(inttostr(a)+' : '+inttostr(b));
end;


procedure TKMLog.AssertToLog(TestValue:boolean; const aMessageText:string);
begin
  if TestValue then exit;
  AddLine('ASSERTION FAILED! Msg: ' + aMessageText);
  Assert(false, 'ASSERTION FAILED! Msg: ' + aMessageText);
end;


procedure TKMLog.AddToLog(const aText:string);
begin
  AddLineNoTime(aText);
end;


{ TKMList }
procedure TKMList.Clear;
var i:integer;
begin
  for i:=0 to Count-1 do begin
    TObject(Items[i]).Free;
    Items[i]:=nil;
  end;
  Inherited;
end;


{ TKMemoryStream }
procedure TKMemoryStream.Write(const Value:string);
var i:word;
begin
  i := length(Value);
  Inherited Write(i, SizeOf(i));
  if i=0 then exit;
  Inherited Write(Value[1], i);
end;


function TKMemoryStream.Write(const Value:TKMPoint): Longint;
begin Result := Inherited Write(Value, SizeOf(Value)); end;
function TKMemoryStream.Write(const Value:TKMPointF): Longint;
begin Result := Inherited Write(Value, SizeOf(Value)); end;
function TKMemoryStream.Write(const Value:single): Longint;
begin Result := Inherited Write(Value, SizeOf(Value)); end;
function TKMemoryStream.Write(const Value:integer): Longint;
begin Result := Inherited Write(Value, SizeOf(Value)); end;
function TKMemoryStream.Write(const Value:cardinal): Longint;
begin Result := Inherited Write(Value, SizeOf(Value)); end;
function TKMemoryStream.Write(const Value:byte): Longint;
begin Result := Inherited Write(Value, SizeOf(Value)); end;
function TKMemoryStream.Write(const Value:boolean): Longint;
begin Result := Inherited Write(Value, SizeOf(Value)); end;
function TKMemoryStream.Write(const Value:word): Longint;
begin Result := Inherited Write(Value, SizeOf(Value)); end;
function TKMemoryStream.Write(const Value:shortint): Longint;
begin Result := Inherited Write(Value, SizeOf(Value)); end;


procedure TKMemoryStream.WriteAsText(aText:string);
begin
  Position := 0;
  Write(aText[1], length(aText));
  Position := 0;
end;


procedure TKMemoryStream.Read(out Value:string);
var i:word;
begin
  Read(i, SizeOf(i));
  setlength(Value, i);
  if i=0 then exit;
  Read(Value[1], i);
end;

function TKMemoryStream.Read(out Value:TKMPoint): Longint;
begin Result := Inherited Read(Value, SizeOf(Value)); end;
function TKMemoryStream.Read(out Value:TKMPointF): Longint;
begin Result := Inherited Read(Value, SizeOf(Value)); end;
function TKMemoryStream.Read(out Value:single): Longint;
begin Result := Inherited Read(Value, SizeOf(Value)); end;
function TKMemoryStream.Read(out Value:integer): Longint;
begin Result := Inherited Read(Value, SizeOf(Value)); end;
function TKMemoryStream.Read(out Value:cardinal): Longint;
begin Result := Inherited Read(Value, SizeOf(Value)); end;
function TKMemoryStream.Read(out Value:byte): Longint;
begin Result := Inherited Read(Value, SizeOf(Value)); end;
function TKMemoryStream.Read(out Value:boolean): Longint;
begin Result := Inherited Read(Value, SizeOf(Value)); end;
function TKMemoryStream.Read(out Value:word): Longint;
begin Result := Inherited Read(Value, SizeOf(Value)); end;
function TKMemoryStream.Read(out Value:shortint): Longint;
begin Result := Inherited Read(Value, SizeOf(Value)); end;


function TKMemoryStream.ReadAsText:string;
begin
  SetString(Result, PChar(Memory), Size div SizeOf(Char));
end;


{ TKMMessageList }
destructor TKMMessageList.Destroy;
var i:integer;
begin
  for i := 1 to fCount do
    FreeAndNil(fList[i]);
  Inherited;
end;


procedure TKMMessageList.AddEntry(aMsgTyp:TKMMessageType; aText:string; aLoc:TKMPoint);
begin
  inc(fCount);
  setlength(fList, fCount+1);
  fList[fCount] := TKMMessage.Create;
  fList[fCount].msgType := aMsgTyp;
  fList[fCount].msgText := aText;
  fList[fCount].msgLoc := aLoc;
end;


procedure TKMMessageList.RemoveEntry(aID:cardinal);
var i:cardinal;
begin
  dec(fCount);
  FreeAndNil(fList[aID]); //First remove the deleted message
  for i := aID to fCount do
    fList[i] := fList[i+1]; //Then move the other message up to it
  fList[fCount+1] := nil; //Set the last+1 message to be nil, because the last message already points to it. (don't want duplicate pointers)
  setlength(fList, fCount+1); //to keep it neat
end;


//Might be of use with priority messages
procedure TKMMessageList.InjectEntry(aID:cardinal; aMsgTyp:TKMMessageType; aText:string);
var i:cardinal;
begin
  inc(fCount);
  setlength(fList, fCount+1);
  for i := aID + 1 to fCount do
    fList[i] := fList[i-1];
  fList[aID].msgType := aMsgTyp;
  fList[aID].msgText := aText;
end;

function TKMMessageList.GetMsgPic(aID:cardinal):cardinal;
begin
  if aID in [1..fCount] then
    Result := cardinal(fList[aID].msgType)
  else
    Result := 0;
end;


function TKMMessageList.GetMsgType(aID:cardinal):TKMMessageType;
begin
  if aID in [1..fCount] then
    Result := fList[aID].msgType
  else
    Result := msgUnknown;
end;


function TKMMessageList.GetMsgHasGoTo(aID:cardinal):boolean;
begin
  if aID in [1..fCount] then
    Result := (fList[aID].msgType = msgHouse) or (fList[aID].msgType = msgUnit)
  else
    Result := false;
end;


function TKMMessageList.GetMsgHasSound(aID:cardinal):boolean;
begin
  if aID in [1..fCount] then
    Result := not ((fList[aID].msgType = msgHorn) or (fList[aID].msgType = msgScroll))
    //These are the two multiplayer options (one for sending text messages, one for alliances)
    //unlike other messages they are sticky and can't be possibly Issued
  else
    Result := false;
end;


function TKMMessageList.GetText(aID:cardinal):string;
begin
  if aID in [1..fCount] then
    Result := fList[aID].msgText
  else
    Result := '';
end;


function TKMMessageList.GetLoc(aID:cardinal):TKMPoint;
begin
  if aID in [1..fCount] then
    Result := fList[aID].msgLoc
  else
    Result := KMPoint(0,0);
end;


procedure TKMMessageList.Save(SaveStream:TKMemoryStream);
var i:cardinal;
begin
  SaveStream.Write(fCount);
  for i:=1 to fCount do
  begin
    SaveStream.Write(fList[i].msgType, SizeOf(fList[i].msgType));
    SaveStream.Write(fList[i].msgText);
    SaveStream.Write(fList[i].msgLoc);
  end;
end;


procedure TKMMessageList.Load(LoadStream:TKMemoryStream);
var i:cardinal;
begin
  LoadStream.Read(fCount);
  setlength(fList, fCount+1);

  for i:=1 to fCount do
  begin
    fList[i] := TKMMessage.Create;
    LoadStream.Read(fList[i].msgType, SizeOf(fList[i].msgType));
    LoadStream.Read(fList[i].msgText);
    LoadStream.Read(fList[i].msgLoc);
  end;
end;


{ TKMPointList }
constructor TKMPointList.Load(LoadStream:TKMemoryStream);
var i:integer;
begin
  Inherited Create;
  LoadStream.Read(Count);
  setlength(List,Count+32);
  for i:=1 to Count do
  LoadStream.Read(List[i]);
end;


procedure TKMPointList.Clearup;
begin
  Count := 0;
  setlength(List, 0);
end;


procedure TKMPointList.AddEntry(aLoc:TKMPoint);
begin
  inc(Count);
  if Count>length(List)-1 then setlength(List,Count+32);
  List[Count]:=aLoc;
end;


{Remove point from the list if is there. Return 'true' if succeded}
function TKMPointList.RemoveEntry(aLoc:TKMPoint):cardinal;
var i: integer; Found: boolean;
begin
  Result:=0;
  Found := false;
  for i:=1 to Count do
  begin
    if (KMSamePoint(List[i],aLoc) and (not Found)) then
    begin
      dec(Count);
      Found := true;
      Result:=i;
    end;
    if (Found) and (i < Count) then List[i] := List[i+1];
  end;
end;


{ Insert an entry and check if list is still walkable
  Walkable means that every point is next to neighbour points }
procedure TKMPointList.InjectEntry(ID:integer; aLoc:TKMPoint);
var i:integer;
begin
  AddEntry(List[Count]);
  for i:=Count downto ID+1 do
    List[i]:=List[i-1];
  List[ID]:=aLoc;

  if ID = 1 then Assert(GetLength(List[ID],List[ID+1])<1.5); //Inject first
  if ID = Count then Assert(GetLength(List[ID-1],List[ID])<1.5); //Inject last
end;


function TKMPointList.GetRandom:TKMPoint;
begin
  if Count=0 then Result:=KMPoint(0,0)
             else Result:=List[random(Count)+1];
end;


//Reverse the list
procedure TKMPointList.Inverse;
var i:integer;
begin
  for i:=1 to Count div 2 do
    KMSwapPoints(List[i],List[Count-i+1]); //Do +1 since i starts from 1
end;


function TKMPointList.GetTopLeft:TKMPoint;
var i:integer;
begin
  Result := KMPoint(0,0);
  if Count=0 then exit;
  Result := List[1]; //Something to start with
  for i:=2 to Count do begin
    if List[i].X < Result.X then Result.X := List[i].X;
    if List[i].Y < Result.Y then Result.Y := List[i].Y;
  end;
end;


function TKMPointList.GetBottomRight:TKMPoint;
var i:integer;
begin
  Result := KMPoint(0,0);
  if Count=0 then exit;
  Result := List[1]; //Something to start with
  for i:=2 to Count do begin
    if List[i].X > Result.X then Result.X := List[i].X;
    if List[i].Y > Result.Y then Result.Y := List[i].Y;
  end;
end;


procedure TKMPointList.Save(SaveStream:TKMemoryStream);
var i:integer;
begin
  SaveStream.Write(Count);
  for i:=1 to Count do
  SaveStream.Write(List[i]);
end;


{ TKMPointTagList }
constructor TKMPointTagList.Load(LoadStream:TKMemoryStream);
var i:integer;
begin
  Inherited; //Reads Count

  setlength(Tag,Count+32); //Make space in lists to write data to, otherwise we get "Range Check Error"
  setlength(Tag2,Count+32);
  for i:=1 to Count do
  begin
    LoadStream.Read(Tag[i]);
    LoadStream.Read(Tag2[i]);
  end;
end;


procedure TKMPointTagList.Clearup;
begin
  Inherited;
  setlength(Tag,0);
  setlength(Tag2,0);
end;


procedure TKMPointTagList.AddEntry(aLoc:TKMPoint; aTag,aTag2:cardinal);
begin
  Inherited AddEntry(aLoc);
  if Count>length(Tag)-1 then setlength(Tag,Count+32); //Expand the list
  if Count>length(Tag2)-1 then setlength(Tag2,Count+32); //+32 is just a way to avoid further expansions
  Tag[Count]:=aTag;
  Tag2[Count]:=aTag2;
end;


function TKMPointTagList.RemoveEntry(aLoc:TKMPoint):cardinal;
var i: integer;
begin
  Result := Inherited RemoveEntry(aLoc);

  for i:=Result to Count-1 do
  begin
    Tag[i] := Tag[i+1];
    Tag2[i] := Tag2[i+1];
  end;
end;


procedure TKMPointTagList.Save(SaveStream:TKMemoryStream);
var i:integer;
begin
  Inherited; //Writes Count

  for i:=1 to Count do
  begin
    SaveStream.Write(Tag[i]);
    SaveStream.Write(Tag2[i]);
  end;
end;


{ TKMPointList }
constructor TKMPointDirList.Load(LoadStream:TKMemoryStream);
var i:integer;
begin
  Inherited Create;
  LoadStream.Read(Count);
  setlength(List,Count+32); //Make space in lists to write data to, otherwise we get "Range Check Error"
  for i:=1 to Count do
  begin
    LoadStream.Read(List[i].Loc);
    LoadStream.Read(List[i].Dir);
  end;
end;


procedure TKMPointDirList.Clearup;
begin
  Count:=0;
  setlength(List,0);
end;


procedure TKMPointDirList.AddEntry(aLoc:TKMPointDir);
begin
  inc(Count);
  if Count>length(List)-1 then setlength(List,Count+32);
  List[Count]:=aLoc;
end;


{Remove point from the list if is there. Return 'true' if succeded}
function TKMPointDirList.RemoveEntry(aLoc:TKMPointDir):cardinal;
var i: integer; Found: boolean;
begin
  Result:=0;
  Found := false;
  for i:=1 to Count do
  begin
    if (KMSamePointDir(List[i],aLoc) and (not Found)) then
    begin
      dec(Count);
      Found := true;
      Result:=i;
    end;
    if (Found) and (i < Count) then List[i] := List[i+1];
  end;
end;


{Add an entry at given place an shift everything }
procedure TKMPointDirList.InjectEntry(ID:integer; aLoc:TKMPointDir);
var i:integer;
begin
  AddEntry(List[Count]);
  for i:=Count downto ID+1 do
    List[i]:=List[i-1];
  List[ID]:=aLoc;
end;


function TKMPointDirList.GetRandom:TKMPointDir;
begin
  if Count=0 then Result:=KMPointDir(0,0,0)
             else Result:=List[random(Count)+1];
end;


{  Return closest position  }
function TKMPointDirList.GetNearest(aLoc:TKMPoint):TKMPointDir;
var
  i:integer;
  Test, Nearest:single;
begin
  Nearest := 0;
  for i:=1 to Count do begin
    Test := GetLength(List[i].Loc, aLoc);
    if (i=1) or (Test < Nearest) then begin
      Nearest := Test;
      Result := List[i];
    end;
  end;
end;


procedure TKMPointDirList.Save(SaveStream:TKMemoryStream);
var i:integer;
begin
  SaveStream.Write(Count);
  for i:=1 to Count do
  begin
    SaveStream.Write(List[i].Loc);
    SaveStream.Write(List[i].Dir);
  end;
end;


end.
