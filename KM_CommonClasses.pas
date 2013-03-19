unit KM_CommonClasses;
{$I KaM_Remake.inc}
interface
uses
  Classes, Math, SysUtils, KM_NetworkTypes, KM_Points;


type
  { Extended with custom Read/Write commands which accept various types without asking for their length}
  TKMemoryStream = class(TMemoryStream)
  public
    procedure Write(const Value: AnsiString); reintroduce; overload;
    {$IFDEF UNICODE}
    //procedure Write(const Value: UnicodeString); reintroduce; overload;
    {$ENDIF}
    procedure Write(const Value:TKMPointDir ); reintroduce; overload;
    function Write(const Value:TKMDirection): Longint; reintroduce; overload;
    function Write(const Value:TKMPoint ): Longint; reintroduce; overload;
    function Write(const Value:TKMPointF): Longint; reintroduce; overload;
    function Write(const Value:Single   ): Longint; reintroduce; overload;
    function Write(const Value:Integer  ): Longint; reintroduce; overload;
    function Write(const Value:Cardinal ): Longint; reintroduce; overload;
    function Write(const Value:Byte     ): Longint; reintroduce; overload;
    function Write(const Value:Boolean  ): Longint; reintroduce; overload;
    function Write(const Value:Word     ): Longint; reintroduce; overload;
    function Write(const Value:ShortInt ): Longint; reintroduce; overload;
    procedure WriteAsText(const aText: AnsiString); deprecated; //todo: Using text for data exchange is flawed idea. remove

    procedure Read(out Value: AnsiString); reintroduce; overload;
    {$IFDEF UNICODE}
    //procedure Read(out Value: UnicodeString); reintroduce; overload;
    {$ENDIF}
    procedure Read(out Value:TKMPointDir); reintroduce; overload;
    function Read(out Value:TKMDirection): Longint; reintroduce; overload;
    function Read(out Value:TKMPoint    ): Longint; reintroduce; overload;
    function Read(out Value:TKMPointF   ): Longint; reintroduce; overload;
    function Read(out Value:Single      ): Longint; reintroduce; overload;
    function Read(out Value:Integer     ): Longint; reintroduce; overload;
    function Read(out Value:Cardinal    ): Longint; reintroduce; overload;
    function Read(out Value:Byte        ): Longint; reintroduce; overload;
    function Read(out Value:Boolean     ): Longint; reintroduce; overload;
    function Read(out Value:Word        ): Longint; reintroduce; overload;
    function Read(out Value:ShortInt    ): Longint; reintroduce; overload;
    procedure ReadAssert(const Value: string);
    function ReadAsText: AnsiString; deprecated; //todo: Using text for data exchange is flawed idea. remove
  end;

  TStreamEvent = procedure (aData: TKMemoryStream) of object;


  //Stores information about a multiplayer game to be sent: host -> server -> queriers
  TMPGameInfo = class
  public
    GameState: TMPGameState;
    PlayerCount: byte;
    Players: AnsiString;
    Description: AnsiString;
    Map: AnsiString;
    GameTime: TDateTime;
    function GetFormattedTime: string;
    procedure LoadFromText(aText: string);
    function GetAsText: string;
    function GetAsHTML: string;
  end;


  //TKMList owns items and frees them when they are deleted from the list
  TKMList = class(TList)
  protected
    //This one function is enough to free all deleted/cleared/rewritten objects
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
  end;

  TKMPointList = class
  private
    fCount: Integer;
    fItems: array of TKMPoint; //0..Count-1
    function GetPoint(aIndex: Integer): TKMPoint;
    procedure SetPoint(aIndex: Integer; const aValue: TKMPoint); //1..Count
  public
    constructor Create;

    property Count: Integer read fCount write fCount;
    property Items[aIndex: Integer]: TKMPoint read GetPoint write SetPoint; default;

    procedure Clear; virtual;
    procedure Copy(aSrc: TKMPointList);
    procedure AddEntry(aLoc: TKMPoint);
    function  RemoveEntry(aLoc: TKMPoint): Integer; virtual;
    procedure DeleteEntry(aIndex: Integer);
    procedure Insert(ID: Integer; aLoc: TKMPoint);
    function  GetRandom(out Point: TKMPoint): Boolean;
    function  GetClosest(aLoc: TKMPoint; out Point: TKMPoint): Boolean;
    function Contains(const aLoc: TKMPoint): Boolean;
    function IndexOf(const aLoc: TKMPoint): Integer;
    procedure Inverse;
    procedure SparseToDense;
    function  GetBounds(out Bounds: TKMRect): Boolean;
    procedure SaveToStream(SaveStream: TKMemoryStream); virtual;
    procedure LoadFromStream(LoadStream: TKMemoryStream); virtual;
  end;


  TKMPointTagList = class(TKMPointList)
  public
    Tag, Tag2: array of Cardinal; //0..Count-1
    procedure Clear; override;
    procedure AddEntry(aLoc: TKMPoint; aTag: Cardinal; aTag2: Cardinal = 0); reintroduce;
    function IndexOf(const aLoc: TKMPoint; aTag: Cardinal; aTag2: Cardinal): Integer;
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
    procedure LoadFromStream(LoadStream: TKMemoryStream); virtual;
    procedure SaveToStream(SaveStream: TKMemoryStream); virtual;
  end;


  TKMPointDirTagList = class(TKMPointDirList)
  public
    Tag, Tag2: array of Cardinal; //0..Count-1
    procedure AddItem(aLoc: TKMPointDir; aTag,aTag2: Cardinal); reintroduce;
    procedure SortByTag;
    procedure SaveToStream(SaveStream: TKMemoryStream); override;
    procedure LoadFromStream(LoadStream: TKMemoryStream); override;
  end;


  //Custom Exception that includes a TKMPoint
  ELocError = class(Exception)
    Loc: TKMPoint;
    constructor Create(const Msg: string; aLoc: TKMPoint);
  end;


implementation
uses KM_Utils;


{ ELocError }
constructor ELocError.Create(const Msg: string; aLoc: TKMPoint);
begin
  inherited Create(Msg);
  Loc := aLoc;
end;


{ TMPGameInfo }
procedure TMPGameInfo.LoadFromText(aText: string);
var M: TKMemoryStream;
begin
  M := TKMemoryStream.Create;
  try
    M.WriteAsText(aText);
  M.Read(GameState, SizeOf(GameState));
  M.Read(PlayerCount);
  M.Read(Players);
  M.Read(Description);
  M.Read(Map);
  M.Read(GameTime, SizeOf(GameTime));
  finally
    M.Free;
  end;
end;


//Return string representation of games length
function TMPGameInfo.GetFormattedTime: string;
begin
  if GameTime >= 0 then
    Result := TimeToString(GameTime)
  else
    Result := '';
end;


function TMPGameInfo.GetAsText: string;
var M: TKMemoryStream;
begin
  M := TKMemoryStream.Create;

  M.Write(GameState, SizeOf(GameState));
  M.Write(PlayerCount);
  M.Write(Players);
  M.Write(Description);
  M.Write(Map);
  M.Write(GameTime, SizeOf(GameTime));

  Result := M.ReadAsText;
  M.Free;
end;


function TMPGameInfo.GetAsHTML: string;
begin
  Result := '';
  Result := Result + Map;
  Result := Result +'<BR>'+ GetFormattedTime;
  Result := Result +'<BR>'+ Players;
end;


{ TKMList }
//We were notified that the item is deleted from the list
procedure TKMList.Notify(Ptr: Pointer; Action: TListNotification);
begin
  if (Action = lnDeleted) then
    TObject(Ptr).Free;
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
{procedure TKMemoryStream.Write(const Value: UnicodeString);
var I: Word;
begin
  I := Length(Value);
  inherited Write(I, SizeOf(I));
  if I = 0 then Exit;
  inherited Write(Pointer(Value)^, I * SizeOf(Char));
end;}
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


procedure TKMemoryStream.WriteAsText(const aText: AnsiString);
begin
  Position := 0;
  Write(Pointer(aText)^, Length(aText) * SizeOf(AnsiChar));
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
{procedure TKMemoryStream.Read(out Value: UnicodeString);
var I: Word;
begin
  Read(I, SizeOf(I));
  SetLength(Value, I);
  if I=0 then exit;
  Read(Pointer(Value)^, I * SizeOf(Char));
end;}
{$ENDIF}


procedure TKMemoryStream.Read(out Value: TKMPointDir);
begin
  Read(Value.Loc);
  Read(Value.Dir, SizeOf(Value.Dir));
end;

function TKMemoryStream.Read(out Value:TKMDirection): Longint;
begin Result := inherited Read(Value, SizeOf(Value)); end;
function TKMemoryStream.Read(out Value:TKMPoint): Longint;
begin Result := inherited Read(Value, SizeOf(Value)); end;
function TKMemoryStream.Read(out Value:TKMPointF): Longint;
begin Result := inherited Read(Value, SizeOf(Value)); end;
function TKMemoryStream.Read(out Value:single): Longint;
begin Result := inherited Read(Value, SizeOf(Value)); end;
function TKMemoryStream.Read(out Value:integer): Longint;
begin Result := inherited Read(Value, SizeOf(Value)); end;
function TKMemoryStream.Read(out Value:cardinal): Longint;
begin Result := inherited Read(Value, SizeOf(Value)); end;
function TKMemoryStream.Read(out Value:byte): Longint;
begin Result := inherited Read(Value, SizeOf(Value)); end;
function TKMemoryStream.Read(out Value:boolean): Longint;
begin Result := inherited Read(Value, SizeOf(Value)); end;
function TKMemoryStream.Read(out Value:word): Longint;
begin Result := inherited Read(Value, SizeOf(Value)); end;
function TKMemoryStream.Read(out Value:shortint): Longint;
begin Result := inherited Read(Value, SizeOf(Value)); end;


procedure TKMemoryStream.ReadAssert(const Value: string);
var S: AnsiString;
begin
  Read(s);
  Assert(s = Value, 'TKMemoryStream.Read <> Value: '+Value);
end;

function TKMemoryStream.ReadAsText: AnsiString;
begin
  SetString(Result, PChar(Memory), Size div SizeOf(AnsiChar));
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
      Result := I;

  //Remove found entry
  if (Result <> -1) then
    DeleteEntry(Result);
end;


procedure TKMPointList.DeleteEntry(aIndex:Integer);
begin
  if not InRange(aIndex, 0, Count-1) then Exit;
  if (aIndex <> fCount - 1) then
    Move(fItems[aIndex+1], fItems[aIndex], SizeOf(fItems[aIndex]) * (fCount - 1 - aIndex));
  Dec(fCount);
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
    if KMLengthSqr(fItems[I], aLoc) < KMLengthSqr(Point, aLoc) then
      Point := fItems[I];
  end;
end;


function TKMPointList.Contains(const aLoc: TKMPoint): Boolean;
begin
  Result := IndexOf(aLoc) <> -1;
end;


function TKMPointList.IndexOf(const aLoc: TKMPoint): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := fCount - 1 downto 0 do
  if KMSamePoint(aLoc, fItems[I]) then
  begin
    Result := I;
    Break;
  end;
end;


procedure TKMPointList.Copy(aSrc: TKMPointList);
begin
  fCount := aSrc.Count;
  SetLength(fItems, fCount);

  Move(aSrc.fItems[0], fItems[0], SizeOf(fItems[0]) * fCount);
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


//Used in JPS pathfinding
procedure TKMPointList.SparseToDense;
var
  I,K,J: Integer;
  Tmp: array of TKMPoint;
  Span: Word;
  C,N: ^TKMPoint;
begin
  K := 0;
  SetLength(Tmp, 8192);
  for I := 0 to fCount - 1 do
  begin
    Tmp[K] := fItems[I];
    Inc(K);

    if (I <> fCount - 1) then
    begin
      C := @fItems[I];
      N := @fItems[I+1];
      Span := Max(Abs(N.X - C.X), Abs(N.Y - C.Y));
      for J := 1 to Span - 1 do
      begin
        Tmp[K].X := C.X + Round((N.X - C.X) / Span * J);
        Tmp[K].Y := C.Y + Round((N.Y - C.Y) / Span * J);
        Inc(K);
      end;
    end;
  end;

  fCount := K;
  SetLength(fItems, fCount);
  Move(Tmp[0], fItems[0], SizeOf(fItems[0]) * fCount);
end;


function TKMPointList.GetBounds(out Bounds: TKMRect): Boolean;
var I: Integer;
begin
  Result := fCount <> 0;

  if Result then
  begin
    //Something to start with
    Bounds.Left   := fItems[0].X;
    Bounds.Top    := fItems[0].Y;
    Bounds.Right  := fItems[0].X;
    Bounds.Bottom := fItems[0].Y;
    for I := 1 to fCount - 1 do
    begin
      if fItems[I].X < Bounds.Left then Bounds.Left := fItems[I].X;
      if fItems[I].Y < Bounds.Top then Bounds.Top := fItems[I].Y;
      if fItems[I].X > Bounds.Right then Bounds.Right := fItems[I].X;
      if fItems[I].Y > Bounds.Bottom then Bounds.Bottom := fItems[I].Y;
    end;
  end;
end;


procedure TKMPointList.SaveToStream(SaveStream: TKMemoryStream);
begin
  SaveStream.Write(fCount);
  if fCount > 0 then
    SaveStream.Write(fItems[0], SizeOf(fItems[0]) * fCount);
end;


procedure TKMPointList.LoadFromStream(LoadStream: TKMemoryStream);
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


procedure TKMPointTagList.AddEntry(aLoc: TKMPoint; aTag: Cardinal; aTag2: Cardinal = 0);
begin
  inherited AddEntry(aLoc);

  if fCount >= Length(Tag) then  SetLength(Tag, fCount + 32); //Expand the list
  if fCount >= Length(Tag2) then SetLength(Tag2, fCount + 32); //+32 is just a way to avoid further expansions
  Tag[fCount-1]  := aTag;
  Tag2[fCount-1] := aTag2;
end;


function TKMPointTagList.IndexOf(const aLoc: TKMPoint; aTag, aTag2: Cardinal): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := fCount - 1 downto 0 do
  if KMSamePoint(aLoc, fItems[I]) and (aTag = Tag[I]) and (aTag2 = Tag2[I]) then
  begin
    Result := I;
    Break;
  end;
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
        KMSwapInt(Tag[I], Tag[K]);
        KMSwapInt(Tag2[I], Tag2[K]);
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
  if fCount > 0 then
  begin
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


procedure TKMPointDirTagList.AddItem(aLoc: TKMPointDir; aTag,aTag2: Cardinal);
begin
  inherited AddItem(aLoc);

  if fCount >= Length(Tag) then  SetLength(Tag, fCount + 32); //Expand the list
  if fCount >= Length(Tag2) then SetLength(Tag2, fCount + 32); //+32 is just a way to avoid further expansions
  Tag[fCount-1]  := aTag;
  Tag2[fCount-1] := aTag2;
end;


procedure TKMPointDirTagList.SortByTag;
var I,K: Integer;
begin
  for I := 0 to fCount - 1 do
  for K := I + 1 to fCount - 1 do
  if Tag[K] < Tag[I] then
  begin
    KMSwapPointDir(fItems[I], fItems[K]);
    KMSwapInt(Tag[I], Tag[K]);
    KMSwapInt(Tag2[I], Tag2[K]);
  end;
end;


procedure TKMPointDirTagList.SaveToStream(SaveStream: TKMemoryStream);
begin
  inherited; //Writes Count

  if fCount > 0 then
  begin
    SaveStream.Write(Tag[0], SizeOf(Tag[0]) * fCount);
    SaveStream.Write(Tag2[0], SizeOf(Tag2[0]) * fCount);
  end;
end;


procedure TKMPointDirTagList.LoadFromStream(LoadStream: TKMemoryStream);
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


end.
