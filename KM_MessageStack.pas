unit KM_MessageStack;
{$I KaM_Remake.inc}
interface
uses
  Math, SysUtils, KM_CommonClasses, KM_Points;


{Messages}
type
  TKMMessageKind = (mkText, mkHouse, mkUnit, mkQuill);

  TKMMessage = class
    fKind: TKMMessageKind;
    fLoc: TKMPoint;
    fText: AnsiString;
  public
    constructor Create(aKind: TKMMessageKind; aText: string; aLoc: TKMPoint);
    constructor CreateFromStream(LoadStream: TKMemoryStream);

    function IsGoto: Boolean;
    function Icon: Word;
    property Loc: TKMPoint read fLoc;
    property Text: AnsiString read fText;
    property Kind: TKMMessageKind read fKind;

    procedure Save(SaveStream: TKMemoryStream);
  end;

  TKMMessageList = class
  private
    fCount: Integer;
    fList: array of TKMMessage; //0..Count-1
    function GetMessage(aIndex: Integer): TKMMessage;
  public
    destructor Destroy; override;

    property Count: Integer read fCount;
    property Messages[aIndex: Integer]: TKMMessage read GetMessage; default;

    procedure AddEntry(aKind: TKMMessageKind; aText: string; aLoc: TKMPoint);
    procedure RemoveEntry(aIndex: Integer);
    procedure InsertEntry(aIndex: Integer; aKind: TKMMessageKind; aText: string; aLoc: TKMPoint);

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
  LoadStream.Read(fText);
  LoadStream.Read(fKind, SizeOf(TKMMessageKind));
end;


//Check wherever message has a GoTo option
function TKMMessage.IsGoto: Boolean;
begin
  Result := fKind in [mkHouse, mkUnit];
end;


//GUIMain icon index associated with that message kind
function TKMMessage.Icon: Word;
const MsgIcon: array [TKMMessageKind] of word = (491, 492, 493, 495);
begin
  Result := MsgIcon[fKind];
end;


procedure TKMMessage.Save(SaveStream: TKMemoryStream);
begin
  SaveStream.Write(fLoc);
  SaveStream.Write(fText);
  SaveStream.Write(fKind, SizeOf(TKMMessageKind));
end;


{ TKMMessageList }
destructor TKMMessageList.Destroy;
var
  I: Integer;
begin
  for I := 0 to fCount - 1 do
    FreeAndNil(fList[I]);
  inherited;
end;


function TKMMessageList.GetMessage(aIndex: Integer): TKMMessage;
begin
  Assert(InRange(aIndex, 0, fCount - 1));
  Result := fList[aIndex];
end;


procedure TKMMessageList.AddEntry(aKind: TKMMessageKind; aText: string; aLoc: TKMPoint);
begin
  SetLength(fList, fCount + 1);
  fList[fCount] := TKMMessage.Create(aKind, aText, aLoc);
  Inc(fCount);
end;


procedure TKMMessageList.RemoveEntry(aIndex: Integer);
begin
  FreeAndNil(fList[aIndex]); //Release the deleted message

  //Move the messges to cover the gap
  if aIndex <> fCount - 1 then
    Move(fList[aIndex + 1], fList[aIndex], (fCount - 1 - aIndex) * SizeOf(TKMMessage));

  //Keep it neat
  Dec(fCount);
  SetLength(fList, fCount);
end;


//Might be of use with priority messages
procedure TKMMessageList.InsertEntry(aIndex: Integer; aKind: TKMMessageKind; aText: string; aLoc: TKMPoint);
begin
  SetLength(fList, fCount + 1);
  if aIndex <> fCount then
    Move(fList[aIndex], fList[aIndex + 1], (fCount - aIndex) * SizeOf(TKMMessage));

  fList[aIndex] := TKMMessage.Create(aKind, aText, aLoc);

  Inc(fCount);
end;


procedure TKMMessageList.Save(SaveStream: TKMemoryStream);
var
  I: Integer;
begin
  SaveStream.Write(fCount);
  for I := 0 to fCount - 1 do
    Messages[I].Save(SaveStream);
end;


procedure TKMMessageList.Load(LoadStream: TKMemoryStream);
var
  I: Integer;
begin
  LoadStream.Read(fCount);
  SetLength(fList, fCount);

  for I := 0 to fCount - 1 do
    fList[I] := TKMMessage.CreateFromStream(LoadStream);
end;


end.
