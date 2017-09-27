unit KM_MessageLog;
{$I KaM_Remake.inc}
interface
uses
  KM_CommonClasses, KM_CommonTypes, KM_Points;


type
  //Individual message
  TKMLogMessage = class
    fKind: TKMMessageKind;
    fLoc: TKMPoint;
    fTextID: Integer;
    fIsReadGIP: Boolean; //This is synced through GIP
    fIsReadLocal: Boolean; //This is used locally so it responds instantly
  public
    constructor Create(aKind: TKMMessageKind; aTextID: Integer; aLoc: TKMPoint);
    constructor CreateFromStream(LoadStream: TKMemoryStream);

    function IsGoto: Boolean;
    function IsRead: Boolean;
    function Text: UnicodeString;
    property Loc: TKMPoint read fLoc;
    property Kind: TKMMessageKind read fKind;

    property IsReadGIP: Boolean write fIsReadGIP;
    property IsReadLocal: Boolean write fIsReadLocal;

    procedure Save(SaveStream: TKMemoryStream);
  end;

  TKMMessageLog = class
  private
    fCountLog: Integer;
    fListLog: array of TKMLogMessage;
    function GetMessageLog(aIndex: Integer): TKMLogMessage;
  public
    destructor Destroy; override;

    property CountLog: Integer read fCountLog;
    property MessagesLog[aIndex: Integer]: TKMLogMessage read GetMessageLog; default;

    procedure Add(aKind: TKMMessageKind; aTextID: Integer; aLoc: TKMPoint);

    procedure Save(SaveStream: TKMemoryStream);
    procedure Load(LoadStream: TKMemoryStream);
  end;


implementation
uses
  SysUtils, Math, KM_ResTexts;


{ TKMLogMessage }
constructor TKMLogMessage.Create(aKind: TKMMessageKind; aTextID: Integer; aLoc: TKMPoint);
begin
  inherited Create;
  fKind := aKind;
  fLoc := aLoc;
  fTextID := aTextID;
end;


constructor TKMLogMessage.CreateFromStream(LoadStream: TKMemoryStream);
begin
  inherited Create;
  LoadStream.Read(fLoc);
  LoadStream.Read(fTextID);
  LoadStream.Read(fKind, SizeOf(TKMMessageKind));
  LoadStream.Read(fIsReadGIP);
end;


//Check wherever message has a GoTo option
function TKMLogMessage.IsGoto: Boolean;
begin
  Result := fKind in [mkHouse, mkUnit];
end;


function TKMLogMessage.IsRead: Boolean;
begin
  Result := fIsReadLocal or fIsReadGIP;
end;


function TKMLogMessage.Text: UnicodeString;
begin
  Result := gResTexts[fTextID];
end;


procedure TKMLogMessage.Save(SaveStream: TKMemoryStream);
begin
  SaveStream.Write(fLoc);
  SaveStream.Write(fTextID);
  SaveStream.Write(fKind, SizeOf(TKMMessageKind));
  SaveStream.Write(fIsReadGIP);
end;


{ TKMMessageLog }
destructor TKMMessageLog.Destroy;
var
  I: Integer;
begin
  for I := 0 to fCountLog - 1 do
    FreeAndNil(fListLog[I]);

  inherited;
end;


function TKMMessageLog.GetMessageLog(aIndex: Integer): TKMLogMessage;
begin
  Assert(InRange(aIndex, 0, fCountLog - 1));
  Result := fListLog[aIndex];
end;


procedure TKMMessageLog.Add(aKind: TKMMessageKind; aTextID: Integer; aLoc: TKMPoint);
begin
  SetLength(fListLog, fCountLog + 1);
  fListLog[fCountLog] := TKMLogMessage.Create(aKind, aTextID, aLoc);
  Inc(fCountLog);
end;


procedure TKMMessageLog.Save(SaveStream: TKMemoryStream);
var
  I: Integer;
begin
  SaveStream.Write(fCountLog);
  for I := 0 to fCountLog - 1 do
    MessagesLog[I].Save(SaveStream);
end;


procedure TKMMessageLog.Load(LoadStream: TKMemoryStream);
var
  I: Integer;
begin
  LoadStream.Read(fCountLog);
  SetLength(fListLog, fCountLog);

  for I := 0 to fCountLog - 1 do
    fListLog[I] := TKMLogMessage.CreateFromStream(LoadStream);
end;


end.
