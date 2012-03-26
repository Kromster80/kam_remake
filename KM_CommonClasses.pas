unit KM_CommonClasses;
{$I KaM_Remake.inc}
interface
uses
  Classes, KromUtils, Math, SysUtils, KM_NetworkTypes, KM_Points;


type
  { Extended with custom Read/Write commands which accept various types without asking for their length}
  TKMemoryStream = class(TMemoryStream)
  public
    procedure Write(const Value: AnsiString); reintroduce; overload;
    {$IFDEF UNICODE}
    procedure Write(const Value: UnicodeString); reintroduce; overload;
    {$ENDIF}
    procedure Write(const Value:TKMPointDir ); reintroduce; overload;
    function Write(const Value:TKMDirection): Longint; reintroduce; overload;
    function Write(const Value:TKMPoint ): Longint; reintroduce; overload;
    function Write(const Value:TKMPointF): Longint; reintroduce; overload;
    function Write(const Value:single   ): Longint; reintroduce; overload;
    function Write(const Value:integer  ): Longint; reintroduce; overload;
    function Write(const Value:cardinal ): Longint; reintroduce; overload;
    function Write(const Value:byte     ): Longint; reintroduce; overload;
    function Write(const Value:boolean  ): Longint; reintroduce; overload;
    function Write(const Value:word     ): Longint; reintroduce; overload;
    function Write(const Value:shortint ): Longint; reintroduce; overload;
    procedure WriteAsText(const aText: string);

    procedure Read(out Value: AnsiString); reintroduce; overload;
    {$IFDEF UNICODE}
    procedure Read(out Value: UnicodeString); reintroduce; overload;
    {$ENDIF}
    procedure Read(out Value:TKMPointDir); reintroduce; overload;
    function Read(out Value:TKMDirection): Longint; reintroduce; overload;
    function Read(out Value:TKMPoint    ): Longint; reintroduce; overload;
    function Read(out Value:TKMPointF   ): Longint; reintroduce; overload;
    function Read(out Value:single      ): Longint; reintroduce; overload;
    function Read(out Value:integer     ): Longint; reintroduce; overload;
    function Read(out Value:cardinal    ): Longint; reintroduce; overload;
    function Read(out Value:byte        ): Longint; reintroduce; overload;
    function Read(out Value:boolean     ): Longint; reintroduce; overload;
    function Read(out Value:word        ): Longint; reintroduce; overload;
    function Read(out Value:shortint    ): Longint; reintroduce; overload;
    procedure ReadAssert(const Value: string);
    function ReadAsText: string;
  end;

  TStreamEvent = procedure (aData: TKMemoryStream) of object;


  //Stores information about a multiplayer game to be sent: host -> server -> queriers
  TMPGameInfo = class
  public
    GameState:TMPGameState;
    PlayerCount: byte;
    Players:string;
    Map:string;
    GameTime:TDateTime;
    function GetFormattedTime:string;
    procedure LoadFromText(aText:string);
    function GetAsText:string;
    function GetAsHTML:string;
  end;


  //List that clears up its items, used only in Units/Houses}
  TKMList = class(TList)
  public
    procedure Clear; override;
  end;

  TKMPointList = class
  private
    fCount: Integer;
    fItems: array of TKMPoint; //0..Count-1
    function GetPoint(aIndex: Integer): TKMPoint;
    procedure SetPoint(aIndex: Integer; const aValue: TKMPoint); //1..Count
  public
    constructor Create;

    property Count: Integer read fCount;
    property Items[aIndex: Integer]: TKMPoint read GetPoint write SetPoint; default;

    procedure Clear; virtual;
    procedure AddEntry(aLoc: TKMPoint);
    function  RemoveEntry(aLoc: TKMPoint): Integer; virtual;
    procedure Insert(ID: Integer; aLoc: TKMPoint);
    function  GetRandom(out Point: TKMPoint): Boolean;
    function  GetClosest(aLoc: TKMPoint; out Point: TKMPoint): Boolean;
    procedure Inverse;
    function  GetTopLeft(out TL: TKMPoint): Boolean;
    function  GetBottomRight(out BR: TKMPoint): Boolean;
    procedure SaveToStream(SaveStream: TKMemoryStream); virtual;
    procedure LoadFromStream(LoadStream: TKMemoryStream); virtual;
  end;


  //todo: Convert to 0..N-1 form
  TKMPointTagList = class(TKMPointList)
  public
    Tag, Tag2: array of Cardinal; //1..Count
    procedure Clear; override;
    procedure AddEntry(aLoc: TKMPoint; aTag,aTag2: Cardinal); reintroduce;
    procedure SortByTag;
    function RemoveEntry(aLoc: TKMPoint): Integer; override;
    procedure SaveToStream(SaveStream: TKMemoryStream); override;
    procedure LoadFromStream(LoadStream: TKMemoryStream); override;
  end;


  TKMPointDirList = class //Used for finding fishing places, fighting positions, etc.
  private
    fItems: array of TKMPointDir; //0..Count-1
    fCount: Integer;
    function GetItem(aIndex: Integer): TKMPointDir;
  public
    procedure Clear;
    procedure AddItem(aLoc: TKMPointDir);

    property Count: Integer read fCount;
    property Items[aIndex: Integer]: TKMPointDir read GetItem; default;

    function GetRandom(out Point: TKMPointDir):Boolean;

    procedure LoadFromStream(LoadStream: TKMemoryStream);
    procedure SaveToStream(SaveStream: TKMemoryStream);
  end;


  //Custom Exception that includes a TKMPoint
  ELocError = class(Exception)
    Loc: TKMPoint;
    constructor Create(const Msg: string; aLoc:TKMPoint);
  end;


implementation
uses KM_Utils;


{ ELocError }
constructor ELocError.Create(const Msg: string; aLoc:TKMPoint);
begin
  Inherited Create(Msg);
  Loc := aLoc;
end;


{ TMPGameInfo }
procedure TMPGameInfo.LoadFromText(aText:string);
var M:TKMemoryStream;
begin
  M := TKMemoryStream.Create;
  try
    M.WriteAsText(aText);
    M.Read(GameState, SizeOf(GameState));
    M.Read(PlayerCount);
    M.Read(Players);
    M.Read(Map);
    M.Read(GameTime, SizeOf(GameTime));
  finally
    M.Free;
  end;
end;


function TMPGameInfo.GetFormattedTime:string;
begin
  if GameTime >= 0 then
    Result := FormatDateTime('hh:nn:ss', GameTime)
  else
    Result := '';
end;


function TMPGameInfo.GetAsText:string;
var M:TKMemoryStream;
begin
  M := TKMemoryStream.Create;

  M.Write(GameState, SizeOf(GameState));
  M.Write(PlayerCount);
  M.Write(Players);
  M.Write(Map);
  M.Write(GameTime, SizeOf(GameTime));

  Result := M.ReadAsText;
  M.Free;
end;


function TMPGameInfo.GetAsHTML:string;
begin
  Result := '';
  Result := Result + Map;
  Result := Result +'<BR>'+ GetFormattedTime;
  Result := Result +'<BR>'+ Players;
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
procedure TKMemoryStream.Write(const Value: AnsiString);
var I: Word;
begin
  I := Length(Value);
  inherited Write(I, SizeOf(I));
  if I = 0 then Exit;
  inherited Write(Pointer(Value)^, I);
end;

{$IFDEF UNICODE}
procedure TKMemoryStream.Write(const Value: UnicodeString);
var I: Word;
begin
  I := Length(Value);
  inherited Write(I, SizeOf(I));
  if I = 0 then Exit;
  inherited Write(Pointer(Value)^, I * SizeOf(Char));
end;
{$ENDIF}

procedure TKMemoryStream.Write(const Value:TKMPointDir);
begin
  Write(Value.Loc);
  inherited Write(Value.Dir, SizeOf(Value.Dir));
end;

function TKMemoryStream.Write(const Value:TKMDirection): Longint;
begin Result := inherited Write(Value, SizeOf(Value)); end;
function TKMemoryStream.Write(const Value:TKMPoint): Longint;
begin Result := inherited Write(Value, SizeOf(Value)); end;
function TKMemoryStream.Write(const Value:TKMPointF): Longint;
begin Result := inherited Write(Value, SizeOf(Value)); end;
function TKMemoryStream.Write(const Value:single): Longint;
begin Result := inherited Write(Value, SizeOf(Value)); end;
function TKMemoryStream.Write(const Value:integer): Longint;
begin Result := inherited Write(Value, SizeOf(Value)); end;
function TKMemoryStream.Write(const Value:cardinal): Longint;
begin Result := inherited Write(Value, SizeOf(Value)); end;
function TKMemoryStream.Write(const Value:byte): Longint;
begin Result := inherited Write(Value, SizeOf(Value)); end;
function TKMemoryStream.Write(const Value:boolean): Longint;
begin Result := inherited Write(Value, SizeOf(Value)); end;
function TKMemoryStream.Write(const Value:word): Longint;
begin Result := inherited Write(Value, SizeOf(Value)); end;
function TKMemoryStream.Write(const Value:shortint): Longint;
begin Result := inherited Write(Value, SizeOf(Value)); end;


procedure TKMemoryStream.WriteAsText(const aText:string);
begin
  Position := 0;
  Write(Pointer(aText)^, Length(aText) * SizeOf(Char));
  Position := 0;
end;


procedure TKMemoryStream.Read(out Value: AnsiString);
var I: Word;
begin
  Read(I, SizeOf(I));
  SetLength(Value, I);
  if I=0 then exit;
  Read(Pointer(Value)^, I);
end;

{$IFDEF UNICODE}
procedure TKMemoryStream.Read(out Value: UnicodeString);
var I: Word;
begin
  Read(I, SizeOf(I));
  SetLength(Value, I);
  if I=0 then exit;
  Read(Pointer(Value)^, I * SizeOf(Char));
end;
{$ENDIF}


procedure TKMemoryStream.Read(out Value:TKMPointDir);
begin
  Read(Value.Loc);
  Read(Value.Dir, SizeOf(Value.Dir));
end;

function TKMemoryStream.Read(out Value:TKMDirection): Longint;
begin Result := Inherited Read(Value, SizeOf(Value)); end;
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


procedure TKMemoryStream.ReadAssert(const Value: string);
var S: string;
begin
  Read(s);
  Assert(s = Value, 'TKMemoryStream.Read <> Value: '+Value);
end;

function TKMemoryStream.ReadAsText: string;
begin
  SetString(Result, PChar(Memory), Size div SizeOf(Char));
end;


{ TKMPointList }
constructor TKMPointList.Create;
begin
  inherited;
end;


procedure TKMPointList.Clear;
begin
  fCount := 0;
end;


procedure TKMPointList.AddEntry(aLoc: TKMPoint);
begin
  if fCount >= Length(fItems) then
    SetLength(fItems, fCount + 32);
  fItems[fCount] := aLoc;
  Inc(fCount);
end;


//Remove point from the list if is there. Return index of removed entry or -1 on failure
function TKMPointList.RemoveEntry(aLoc: TKMPoint): Integer;
var
  I: Integer;
begin
  Result := -1;

  //Scan whole list to detect duplicate entries
  for I := 0 to fCount - 1 do
  if KMSamePoint(fItems[I], aLoc) then
  begin
    Assert(Result = -1, 'Duplicate points in list');
    Result := I;
  end;

  //Remove found entry
  if (Result <> -1) then
  begin
    if (Result <> fCount - 1) then
      Move(fItems[Result+1], fItems[Result], SizeOf(fItems[Result]) * (fCount - 1 - Result));
    Dec(fCount);
  end;
end;


//Insert an entry and check if list is still walkable
//Walkable means that every point is next to neighbour points }
procedure TKMPointList.Insert(ID: Integer; aLoc: TKMPoint);
begin
  Assert(InRange(ID, 0, fCount));

  //Grow the list
  if fCount >= Length(fItems) then
    SetLength(fItems, fCount + 32);

  //Shift items towards end
  if fCount <> 0 then
    Move(fItems[ID], fItems[ID+1], SizeOf(fItems[ID]) * (fCount - ID));

  fItems[ID] := aLoc;
  Inc(fCount);
end;


function TKMPointList.GetRandom(out Point: TKMPoint): Boolean;
begin
  Result := fCount <> 0;
  if Result then
    Point := fItems[KaMRandom(fCount)];
end;


function TKMPointList.GetClosest(aLoc: TKMPoint; out Point: TKMPoint): Boolean;
var
  I: Integer;
begin
  Result := fCount <> 0;
  if Result then
  begin
    Point := fItems[0];
    for I := 1 to fCount - 1 do
    if GetLength(fItems[I], aLoc) < GetLength(Point, aLoc) then
      Point := fItems[I];
    Result := True;
  end;
end;


function TKMPointList.GetPoint(aIndex: Integer): TKMPoint;
begin
  Result := fItems[aIndex];
end;


procedure TKMPointList.SetPoint(aIndex: Integer; const aValue: TKMPoint);
begin
  fItems[aIndex] := aValue;
end;


//Reverse the list
procedure TKMPointList.Inverse;
var
  I: Integer;
begin
  for I := 0 to fCount div 2 - 1 do
    KMSwapPoints(fItems[I], fItems[fCount-1-I]);
end;


//Get top-leftmost coordinates of bounding box around all the points
function TKMPointList.GetTopLeft(out TL: TKMPoint): Boolean;
var
  I: Integer;
begin
  Result := fCount <> 0;

  if Result then
  begin
    TL := fItems[0]; //Something to start with
    for I := 1 to fCount - 1 do
    begin
      if fItems[I].X < TL.X then TL.X := fItems[I].X;
      if fItems[I].Y < TL.Y then TL.Y := fItems[I].Y;
    end;
  end;
end;


//Get bottom-rightmost coordinates of bounding box around all the points
function TKMPointList.GetBottomRight(out BR: TKMPoint): Boolean;
var
  I: Integer;
begin
  Result := fCount <> 0;

  if Result then
  begin
    BR := fItems[0]; //Something to start with
    for I := 1 to fCount - 1 do
    begin
      if fItems[I].X > BR.X then BR.X := fItems[I].X;
      if fItems[I].Y > BR.Y then BR.Y := fItems[I].Y;
    end;
  end;
end;


procedure TKMPointList.SaveToStream(SaveStream: TKMemoryStream);
begin
  SaveStream.Write(fCount);
  if fCount > 0 then
    SaveStream.Write(fItems[0], SizeOf(fItems[0]) * fCount);
end;


procedure TKMPointList.LoadFromStream(LoadStream:TKMemoryStream);
begin
  LoadStream.Read(fCount);
  SetLength(fItems, fCount);
  if fCount > 0 then
    LoadStream.Read(fItems[0], SizeOf(fItems[0]) * fCount);
end;


{ TKMPointTagList }
procedure TKMPointTagList.Clear;
begin
  inherited;
end;


procedure TKMPointTagList.AddEntry(aLoc: TKMPoint; aTag,aTag2: Cardinal);
begin
  inherited AddEntry(aLoc);

  if fCount >= Length(Tag) then  SetLength(Tag, fCount + 32); //Expand the list
  if fCount >= Length(Tag2) then SetLength(Tag2, fCount + 32); //+32 is just a way to avoid further expansions
  Tag[fCount-1]  := aTag;
  Tag2[fCount-1] := aTag2;
end;


function TKMPointTagList.RemoveEntry(aLoc: TKMPoint): Integer;
begin
  Result := inherited RemoveEntry(aLoc);

  //Note that fCount is already decreased by 1
  if (Result <> -1) and (Result <> fCount) then
  begin
    Move(Tag[Result+1], Tag[Result], SizeOf(Tag[Result]) * (fCount - Result));
    Move(Tag2[Result+1], Tag2[Result], SizeOf(Tag2[Result]) * (fCount - Result));
  end;
end;


procedure TKMPointTagList.SaveToStream(SaveStream: TKMemoryStream);
begin
  inherited; //Writes Count

  if fCount > 0 then
  begin
    SaveStream.Write(Tag[0], SizeOf(Tag[0]) * fCount);
    SaveStream.Write(Tag2[0], SizeOf(Tag2[0]) * fCount);
  end;
end;


procedure TKMPointTagList.SortByTag;
var I,K: Integer;
begin
  for I := 0 to fCount - 1 do
    for K := I + 1 to fCount - 1 do
      if Tag[K] < Tag[I] then
      begin
        KMSwapPoints(fItems[I], fItems[K]);
        SwapInt(Tag[I], Tag[K]);
        SwapInt(Tag2[I], Tag2[K]);
      end;
end;


procedure TKMPointTagList.LoadFromStream(LoadStream: TKMemoryStream);
begin
  inherited; //Reads Count

  SetLength(Tag, fCount);
  SetLength(Tag2, fCount);
  if fCount > 0 then
  begin
    LoadStream.Read(Tag[0], SizeOf(Tag[0]) * fCount);
    LoadStream.Read(Tag2[0], SizeOf(Tag2[0]) * fCount);
  end;
end;


{ TKMPointList }
procedure TKMPointDirList.Clear;
begin
  fCount := 0;
end;


procedure TKMPointDirList.AddItem(aLoc: TKMPointDir);
begin
  if fCount >= Length(fItems) then
    SetLength(fItems, fCount + 32);
  fItems[fCount] := aLoc;
  inc(fCount);
end;


function TKMPointDirList.GetItem(aIndex: Integer): TKMPointDir;
begin
  Assert(InRange(aIndex, 0, fCount - 1));
  Result := fItems[aIndex];
end;


function TKMPointDirList.GetRandom(out Point: TKMPointDir):Boolean;
begin
  Result := False;
  if fCount > 0 then begin
    Point := fItems[KaMRandom(fCount)];
    Result := True;
  end;
end;


procedure TKMPointDirList.SaveToStream(SaveStream: TKMemoryStream);
begin
  SaveStream.Write(fCount);
  if fCount > 0 then
    SaveStream.Write(fItems[0], SizeOf(fItems[0]) * fCount);
end;


procedure TKMPointDirList.LoadFromStream(LoadStream: TKMemoryStream);
begin
  LoadStream.Read(fCount);
  SetLength(fItems, fCount);
  if fCount > 0 then
    LoadStream.Read(fItems[0], SizeOf(fItems[0]) * fCount);
end;


end.
