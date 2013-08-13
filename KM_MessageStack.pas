unit KM_MessageStack;
{$I KaM_Remake.inc}
interface
uses
  Math, SysUtils, KM_CommonClasses, KM_CommonTypes, KM_Points;


type
  //Individual message
  TKMMessage = class
    fKind: TKMMessageKind;
    fLoc: TKMPoint;
    fText: UnicodeString;
  public
    IsRead: Boolean; //Does not gets saved, because it's UI thing
    constructor Create(aKind: TKMMessageKind; aText: string; aLoc: TKMPoint);
    constructor CreateFromStream(LoadStream: TKMemoryStream);

    function Icon: Word;
    function IsGoto: Boolean;
    property Loc: TKMPoint read fLoc;
    property Text: UnicodeString read fText;
    property Kind: TKMMessageKind read fKind;

    procedure Save(SaveStream: TKMemoryStream);
  end;

  TKMMessageList = class
  private
    fCountStack: Integer;
    fCountLog: Integer;
    fListStack: array of TKMMessage; //Messages stacked on right
    fListLog: array of TKMMessage; //Messages shown in log
    function GetMessageStack(aIndex: Integer): TKMMessage;
    function GetMessageLog(aIndex: Integer): TKMMessage;
  public
    destructor Destroy; override;

    property CountStack: Integer read fCountStack;
    property CountLog: Integer read fCountLog;
    property MessagesStack[aIndex: Integer]: TKMMessage read GetMessageStack;
    property MessagesLog[aIndex: Integer]: TKMMessage read GetMessageLog;

    procedure Add(aKind: TKMMessageKind; aText: string; aLoc: TKMPoint);
    procedure RemoveStack(aIndex: Integer);

    procedure Save(SaveStream: TKMemoryStream);
    procedure Load(LoadStream: TKMemoryStream);
  end;


implementation


{ TKMMessage }
constructor TKMMessage.Create(aKind: TKMMessageKind; aText: string; aLoc: TKMPoint);
begin
  inherited Create;
  fKind := aKind;
  fLoc := aLoc;
  fText := aText;
end;


constructor TKMMessage.CreateFromStream(LoadStream: TKMemoryStream);
begin
  inherited Create;
  LoadStream.Read(fLoc);
  LoadStream.ReadW(fText);
  LoadStream.Read(fKind, SizeOf(TKMMessageKind));
  LoadStream.Read(IsRead);
end;


//Check wherever message has a GoTo option
function TKMMessage.IsGoto: Boolean;
begin
  Result := fKind in [mkHouse, mkUnit];
end;


//GUIMain icon index associated with that message kind
function TKMMessage.Icon: Word;
const MsgIcon: array [TKMMessageKind] of Word = (491, 492, 493, 495);
begin
  Result := MsgIcon[fKind];
end;


procedure TKMMessage.Save(SaveStream: TKMemoryStream);
begin
  SaveStream.Write(fLoc);
  SaveStream.WriteW(fText);
  SaveStream.Write(fKind, SizeOf(TKMMessageKind));
  SaveStream.Write(IsRead);
end;


{ TKMMessageList }
destructor TKMMessageList.Destroy;
var
  I: Integer;
begin
  for I := 0 to fCountStack - 1 do
    FreeAndNil(fListStack[I]);
  for I := 0 to fCountLog - 1 do
    FreeAndNil(fListLog[I]);

  inherited;
end;


function TKMMessageList.GetMessageStack(aIndex: Integer): TKMMessage;
begin
  Assert(InRange(aIndex, 0, fCountStack - 1));
  Result := fListStack[aIndex];
end;


function TKMMessageList.GetMessageLog(aIndex: Integer): TKMMessage;
begin
  Assert(InRange(aIndex, 0, fCountLog - 1));
  Result := fListLog[aIndex];
end;


procedure TKMMessageList.Add(aKind: TKMMessageKind; aText: string; aLoc: TKMPoint);
begin
  case aKind of
    mkText, mkQuill:
      begin
        SetLength(fListStack, fCountStack + 1);
        fListStack[fCountStack] := TKMMessage.Create(aKind, aText, aLoc);
        Inc(fCountStack);
      end;
    mkHouse, mkUnit:
      begin
        SetLength(fListLog, fCountLog + 1);
        fListLog[fCountLog] := TKMMessage.Create(aKind, aText, aLoc);
        Inc(fCountLog);
      end;
  end;
end;


procedure TKMMessageList.RemoveStack(aIndex: Integer);
begin
  FreeAndNil(fListStack[aIndex]); //Release the deleted message

  //Move the messages to cover the gap
  if aIndex <> fCountStack - 1 then
    Move(fListStack[aIndex + 1], fListStack[aIndex], (fCountStack - 1 - aIndex) * SizeOf(TKMMessage));

  //Keep it neat
  Dec(fCountStack);
  SetLength(fListStack, fCountStack);
end;


procedure TKMMessageList.Save(SaveStream: TKMemoryStream);
var
  I: Integer;
begin
  SaveStream.Write(fCountStack);
  for I := 0 to fCountStack - 1 do
    MessagesStack[I].Save(SaveStream);

  SaveStream.Write(fCountLog);
  for I := 0 to fCountLog - 1 do
    MessagesLog[I].Save(SaveStream);
end;


procedure TKMMessageList.Load(LoadStream: TKMemoryStream);
var
  I: Integer;
begin
  LoadStream.Read(fCountStack);
  SetLength(fListStack, fCountStack);

  for I := 0 to fCountStack - 1 do
    fListStack[I] := TKMMessage.CreateFromStream(LoadStream);

  LoadStream.Read(fCountLog);
  SetLength(fListLog, fCountLog);

  for I := 0 to fCountLog - 1 do
    fListLog[I] := TKMMessage.CreateFromStream(LoadStream);
end;


end.
