unit KM_BinPacking;
{$I KaM_Remake.inc}
interface
uses
  Classes, Math;

type
  TBinRect = record X, Y, Width, Height: Word; end;

  TIndexItem = record
    ID, X, Y: Word;
  end;

  TIndexSizeArray = array of TIndexItem;

  TBinItem = packed record
    Width, Height: Word;
    Sprites: array of record
      SpriteID: Word;
      PosX, PosY: Word;
    end;
  end;

  TBinArray = array of TBinItem;

  TBin = class
    fChild1: TBin; //
    fChild2: TBin;
    fImageID: Word; //Image that is using this bin (0 if unused)
    fRect: TBinRect; //Our dimensions
    fPad: Byte;
    fNotFit: Cardinal; //Minimum size that does not fit
  public
    constructor Create(aRect: TBinRect; aPad: Byte; aImageID: Word; aNotFit: Cardinal);
    destructor Destroy; override;
    function Insert(aItem: TIndexItem): TBin; //Return bin that has accepted the sprite, or nil of Bin is full
    function Width: Word;
    function Height: Word;
    procedure GetAllItems(var aItems: TBinItem);

    procedure DidNotFit(aItem: TIndexItem);
    function CanFit(aItem: TIndexItem): Boolean;
  end;

  TBinManager = class
  private
    fWidth: Word;
    fHeight: Word;
    fPad: Byte;
    fBins: TList;
    function CreateNew(aWidth: Word; aHeight: Word): TBin;
  public
    constructor Create(aWidth, aHeight: Word; aPad: Byte);
    destructor Destroy; override;
    procedure Insert(aItem: TIndexItem);
    procedure GetAllItems(var aOut: TBinArray);
  end;

  procedure BinPack(aItems: TIndexSizeArray; aMaxSize: Word; aPad: Byte; var aOut: TBinArray);


implementation
uses
  KromUtils;


function BinRect(aX, aY, aWidth, aHeight: Word): TBinRect;
begin
  Result.X := aX;
  Result.Y := aY;
  Result.Width := aWidth;
  Result.Height := aHeight;
end;


procedure BinPack(aItems: TIndexSizeArray; aMaxSize: Word; aPad: Byte; var aOut: TBinArray);
var
  I, K: Integer;
  BinManager: TBinManager;
begin
  Assert(MakePOT(aMaxSize) = aMaxSize);

  //Sort Items by size to improve packing efficiency
  for I := 0 to High(aItems) do
    for K := I + 1 to High(aItems) do
      if (aItems[K].X + aItems[K].Y) > (aItems[I].X + aItems[I].Y) then
      begin
        SwapInt(aItems[I].ID, aItems[K].ID);
        SwapInt(aItems[I].X, aItems[K].X);
        SwapInt(aItems[I].Y, aItems[K].Y);
      end;

  //Do the packing
  BinManager := TBinManager.Create(aMaxSize, aMaxSize, aPad);
  try
    for I := 0 to High(aItems) do
      if (aItems[I].X * aItems[I].Y <> 0) then
        BinManager.Insert(aItems[I]);

    BinManager.GetAllItems(aOut);
  finally
    BinManager.Free;
  end;
end;


{ TBin }
constructor TBin.Create(aRect: TBinRect; aPad: Byte; aImageID: Word; aNotFit: Cardinal);
begin
  inherited Create;

  fRect := aRect; //Our dimensions
  fImageID := aImageID;
  fPad := aPad;
  fNotFit := aNotFit;
end;


destructor TBin.Destroy;
begin
  if fChild1 <> nil then fChild1.Free;
  if fChild2 <> nil then fChild2.Free;

  inherited;
end;


function TBin.CanFit(aItem: TIndexItem): Boolean;
begin
  Result := (aItem.X * aItem.Y < fNotFit);
end;


procedure TBin.DidNotFit(aItem: TIndexItem);
begin
  fNotFit := Min(fNotFit, aItem.X * aItem.Y);
end;


function TBin.Insert(aItem: TIndexItem): TBin;
begin
  //We can't possibly fit the Item (and our Childs can't either)
  if (aItem.X + fPad*2 > fRect.Width) or (aItem.Y + fPad*2 > fRect.Height) or (fImageID <> 0) then
  begin
    Result := nil;
    Exit;
  end;

  //If both childs are nil we can stop recursion and accept the Item
  if (fChild1 = nil) and (fChild2 = nil) then
  begin

    //If we can perfectly fit the Item
    if (fRect.Width = aItem.X + fPad*2) and (fRect.Height = aItem.Y + fPad*2) then
    begin
      fImageID := aItem.ID;
      Result := Self;
      Exit;
    end;

    //Choose axis by which to split (Doc suggest we favor largest free area)
    if (fRect.Width - aItem.X - fPad*2) * fRect.Height > (fRect.Height - aItem.Y - fPad*2) * fRect.Width then
    begin
      //Vertical split
      fChild1 := TBin.Create(BinRect(fRect.X, fRect.Y, aItem.X + fPad*2, fRect.Height), fPad, 0, fNotFit);
      fChild2 := TBin.Create(BinRect(fRect.X + aItem.X + fPad*2, fRect.Y, fRect.Width - aItem.X - fPad*2, fRect.Height), fPad, 0, fNotFit);
    end else
    begin
      //Horizontal split
      fChild1 := TBin.Create(BinRect(fRect.X, fRect.Y, fRect.Width, aItem.Y + fPad*2), fPad, 0, fNotFit);
      fChild2 := TBin.Create(BinRect(fRect.X, fRect.Y + aItem.Y + fPad*2, fRect.Width, fRect.Height - aItem.Y - fPad*2), fPad, 0, fNotFit);
    end;

    //Now let the Child1 handle the Item
    Result := fChild1.Insert(aItem);
    Exit;
  end;

  //We have both Childs initialized, let them try to accept the Item
  Result := fChild1.Insert(aItem);
  if Result = nil then
    Result := fChild2.Insert(aItem);
end;


function TBin.Width: Word;
begin
  Result := fRect.Width;
end;


function TBin.Height: Word;
begin
  Result := fRect.Height;
end;


//Recursively go through all Bins and collect Image info
procedure TBin.GetAllItems(var aItems: TBinItem);
begin
  if (fChild1 <> nil) and (fChild2 <> nil)  then
  begin
    fChild1.GetAllItems(aItems);
    fChild2.GetAllItems(aItems);
  end
  else
    if fImageID <> 0 then
    begin
      SetLength(aItems.Sprites, Length(aItems.Sprites) + 1);
      aItems.Sprites[High(aItems.Sprites)].SpriteID := fImageID;
      aItems.Sprites[High(aItems.Sprites)].PosX := fRect.X + fPad;
      aItems.Sprites[High(aItems.Sprites)].PosY := fRect.Y + fPad;
    end;
end;


{ TBinManager }
constructor TBinManager.Create(aWidth, aHeight: Word; aPad: Byte);
begin
  inherited Create;

  Assert((aWidth > 0) and (aHeight > 0));
  fWidth := aWidth;
  fHeight := aHeight;
  fPad := aPad;

  fBins := TList.Create;
end;


destructor TBinManager.Destroy;
var
  I: Integer;
begin
  for I := 0 to fBins.Count - 1 do
    TBin(fBins[I]).Free;

  fBins.Free;
  inherited;
end;


function TBinManager.CreateNew(aWidth: Word; aHeight: Word): TBin;
begin
  Result := TBin.Create(BinRect(0, 0, aWidth, aHeight), fPad, 0, fWidth*fHeight);
  fBins.Add(Result);
end;


procedure TBinManager.Insert(aItem: TIndexItem);
var
  I: Integer;
  B: TBin;
begin
  //Check all Bins (older Bins may still have space for small items)
  for I := 0 to fBins.Count - 1 do
  if TBin(fBins[I]).CanFit(aItem) then
  begin
    //Try to insert into a bin
    B := TBin(fBins[I]).Insert(aItem);
    if B <> nil then
      Exit //aItem fit
    else
      TBin(fBins[I]).DidNotFit(aItem);
  end;

  //Create new Bin
  if (aItem.X > fWidth) or (aItem.Y > fHeight) then
    //Create new Bin especially for this big item
    B := CreateNew(MakePOT(aItem.X + fPad * 2), MakePOT(aItem.Y + fPad * 2))
  else
    //Use standard Bin size
    B := CreateNew(fWidth, fHeight);

  B.Insert(aItem);
  Assert(B <> nil);
end;


//Write all Bins and images positions into array
procedure TBinManager.GetAllItems(var aOut: TBinArray);
var
  I: Integer;
begin
  //Recursively scan all Bins
  SetLength(aOut, fBins.Count);
  for I := 0 to fBins.Count - 1 do
  begin
    TBin(fBins[I]).GetAllItems(aOut[I]);
    aOut[I].Width := TBin(fBins[I]).Width;
    aOut[I].Height := TBin(fBins[I]).Height;
  end;
end;


end.
