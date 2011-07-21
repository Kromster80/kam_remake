unit KM_MessageStack;
{$I KaM_Remake.inc}
interface
uses
  Math, SysUtils, KM_CommonTypes;


{Messages}
//number matches pic index in gui.rx
type
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
    function GetMsgHasGoTo(aID:cardinal):boolean;
    function GetText(aID:cardinal):string;
    function GetLoc(aID:cardinal; out Point:TKMPoint):Boolean;
    property Count:cardinal read fCount;
    procedure Save(SaveStream:TKMemoryStream);
    procedure Load(LoadStream:TKMemoryStream);
  end;


implementation


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
  SetLength(fList, fCount+1);
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
  SetLength(fList, fCount+1); //to keep it neat
end;


//Might be of use with priority messages
procedure TKMMessageList.InjectEntry(aID:cardinal; aMsgTyp:TKMMessageType; aText:string);
var i:cardinal;
begin
  inc(fCount);
  SetLength(fList, fCount+1);
  for i := aID + 1 to fCount do
    fList[i] := fList[i-1];
  fList[aID].msgType := aMsgTyp;
  fList[aID].msgText := aText;
end;


function TKMMessageList.GetMsgPic(aID:cardinal):cardinal;
const MsgIcon: array[TKMMessageType] of word = (491, 492, 493, 495);
begin
  if InRange(aID,1,fCount) then
    Result := MsgIcon[fList[aID].msgType]
  else
    Result := 0;
end;


function TKMMessageList.GetMsgHasGoTo(aID:cardinal):boolean;
begin
  if InRange(aID,1,fCount) then
    Result := (fList[aID].msgType = msgHouse) or (fList[aID].msgType = msgUnit)
  else
    Result := false;
end;


function TKMMessageList.GetText(aID:cardinal):string;
begin
  if InRange(aID,1,fCount) then
    Result := fList[aID].msgText
  else
    Result := '';
end;


//Todo: convert other functions to this pattern
function TKMMessageList.GetLoc(aID:cardinal; out Point:TKMPoint):Boolean;
begin
  if InRange(aID,1,fCount) then begin
    Point := fList[aID].msgLoc;
    Result := true;
  end else
    Result := false;
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
  SetLength(fList, fCount+1);

  for i:=1 to fCount do
  begin
    fList[i] := TKMMessage.Create;
    LoadStream.Read(fList[i].msgType, SizeOf(fList[i].msgType));
    LoadStream.Read(fList[i].msgText);
    LoadStream.Read(fList[i].msgLoc);
  end;
end;


end.
