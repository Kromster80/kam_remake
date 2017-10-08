unit KM_ResSpritesEdit;
{$I KaM_Remake.inc}
interface
uses
  {$IFDEF Unix} LCLIntf, LCLType, {$ENDIF}
  Classes, Dialogs, Graphics, Math, Types, StrUtils, SysUtils,
  KM_Defaults, KM_Pics, KM_ResHouses, KM_ResPalettes, KM_ResSprites,
  KM_ResTileset
  {$IFDEF FPC}, zstream {$ENDIF}
  {$IFDEF WDC}, ZLib {$ENDIF};

type
  //Class with additional editing properties
  TKMSpritePackEdit = class(TKMSpritePack)
  private
    fPalettes: TKMResPalettes;
    function GetLoaded: Boolean;
  protected
    procedure Allocate(aCount: Integer); override; //Allocate space for data that is being loaded
    procedure Expand;
  public
    constructor Create(aRT: TRXType; aPalettes: TKMResPalettes);

    property IsLoaded: Boolean read GetLoaded;
    procedure AdjoinHouseMasks(aHouseDat: TKMResHouses);
    procedure GrowHouseMasks(aHouseDat: TKMResHouses);
    procedure SoftWater(aTileset: TKMResTileset);
    procedure Delete(aIndex: Integer);
    procedure LoadFromRXFile(const aFileName: string);
    procedure LoadFromFolder(const aFolder: string);
    procedure SaveToRXXFile(const aFileName: string);
    function TrimSprites: Cardinal; //For debug
    procedure ClearTemp; override;
    procedure GetImageToBitmap(aIndex: Integer; aBmp, aMask: TBitmap);
  end;


implementation
uses
  KM_SoftShadows;


var
  RX5Pal: array [1 .. 40] of TKMPal = (
    pal2_setup,   pal2_setup,   pal2_setup,   pal2_setup,   pal2_setup,
    pal2_setup,   pal_set2,     pal_set2,     pal_set2,     pal_map,
    pal_map,      pal_map,      pal_map,      pal_map,      pal_map,
    pal_map,      pal2_setup,   pal2_setup,   pal2_setup,   pal2_mapgold,
    pal2_mapgold, pal2_mapgold, pal2_mapgold, pal2_mapgold, pal2_setup,
    pal_map,      pal_map,      pal_map,      pal_map,      pal_map,
    pal2_setup,   pal2_setup,   pal2_setup,   pal2_setup,   pal2_setup,
    pal2_setup,   pal2_setup,   pal_lin,      pal_lin,      pal_lin
  );

  // I couldn't find matching palettes for the 17th and 18th entries
  RX6Pal: array [1 .. 20] of TKMPal = (
    pal_set,  pal_set,  pal_set,  pal_set,  pal_set,
    pal_set,  pal_set2, pal_set2, pal_set2, pal_map,
    pal_map,  pal_map,  pal_map,  pal_map,  pal_map,
    pal_map,  pal_lin,  pal_lin,  pal_lin,  pal_lin
  );


{ TKMSpritePackEdit }
//We need to access to palettes to properly Expand RX files
constructor TKMSpritePackEdit.Create(aRT: TRXType; aPalettes: TKMResPalettes);
begin
  inherited Create(aRT);

  fPalettes := aPalettes;
end;


procedure TKMSpritePackEdit.Delete(aIndex: Integer);
begin
  Assert(InRange(aIndex, 1, fRXData.Count));
  fRXData.Flag[aIndex] := 0;
end;


procedure TKMSpritePackEdit.Allocate(aCount: Integer);
begin
  inherited;
  fRXData.Count := aCount;

  SetLength(fRXData.Data,     aCount);
end;


//Convert paletted data into RGBA and select Team color layer from it
procedure TKMSpritePackEdit.Expand;
  function HouseWIP(aID: Integer): TKMPaletteInfo;
  const
    //These are sprites with house building steps
    WIP: array[0..55] of word = (3,4,25,43,44,116,118,119,120,121,123,126,127,136,137,140,141,144,145,148,149,213,214,237,238,241,242,243,246,247,252,253,257,258,275,276,336,338,360,361,365,366,370,371,380,381,399,400,665,666,670,671,1658,1660,1682,1684);
  var
    I: Byte;
  begin
    Result := fPalettes.DefaultPalette;

    for I := 0 to High(WIP) do
    if aID = WIP[I] then
    begin
      Result := fPalettes[pal_lin];
      Exit;
    end;
  end;
var
  H: Integer;
  K, I: Integer;
  Palette: TKMPaletteInfo;
  L: byte;
  Pixel: Integer;
begin
  with fRXData do
  for H := 1 to Count do
  begin
    //Choose proper palette
    case fRT of
      rxHouses:   Palette := HouseWIP(H);
      rxGuiMain:  Palette := fPalettes[RX5Pal[H]];
      else        Palette := fPalettes.DefaultPalette;
    end;

    if Flag[H] = 1 then
    begin
      SetLength(RGBA[H], Size[H].X * Size[H].Y);
      SetLength(Mask[H], Size[H].X * Size[H].Y);

      for I := 0 to Size[H].Y - 1 do
      for K := 0 to Size[H].X - 1 do
      begin
        Pixel := I * Size[H].X + K;
        L := Data[H, Pixel]; //0..255

        //Flagcolors in KaM are 24..30
        //We decode them according to visualization pipeline to greyscale
        //and make a color transparency mask
        if RXInfo[fRT].TeamColors and (L in [24..30])
        and (Palette <> fPalettes[pal_lin])
        and ((fRT <> rxHouses) or (H > 400))  //Skip the Inn Weapon Smithy and the rest
        and ((fRT <> rxGui) or InRange(H, 141, 154) or InRange(H, 521, 550)) then //Unit icons and scrolls
        begin
          //RGBA[H, Pixel] := cardinal(((L - 27) * 42 + 128) * 65793) OR $FF000000;
          //Use black and white background for more saturated colors
          if L < 27 then
            RGBA[H, Pixel] := FLAG_COLOR_DARK
          else
            RGBA[H, Pixel] := FLAG_COLOR_LITE;

          case L of
            24, 30: Mask[H, Pixel] := $60;   //38%
            25, 29: Mask[H, Pixel] := $90;   //56%
            26, 28: Mask[H, Pixel] := $C0;   //75%
            27:     Mask[H, Pixel] := $FF;   //100%
          end;
          HasMask[H] := True;
        end else
          RGBA[H, Pixel] := Palette.Color32(L);
      end;
    end;
  end;
end;


function TKMSpritePackEdit.GetLoaded: Boolean;
begin
  Result := fRXData.Count > 0;
end;


//
procedure TKMSpritePackEdit.AdjoinHouseMasks(aHouseDat: TKMResHouses);
var
  HT: THouseType;
  ID1, ID2: Integer; //RGB and A index
  I, K, Lay, StepCount: Integer;
  T1, T2, tx, ty: Integer;
  Alpha: Byte;
begin
  for HT := HOUSE_MIN to HOUSE_MAX do
  for Lay := 1 to 2 do //House is rendered in two layers since Stone does not covers Wood parts in e.g. Sawmill
  begin
    if Lay = 1 then begin
      ID1 := aHouseDat[HT].WoodPic + 1;
      ID2 := aHouseDat[HT].WoodPal + 1;
      StepCount := aHouseDat[HT].WoodPicSteps;
    end else begin
      ID1 := aHouseDat[HT].StonePic + 1;
      ID2 := aHouseDat[HT].StonePal + 1;
      StepCount := aHouseDat[HT].StonePicSteps;
    end;

    //Fill in alpha RXData
    tx := fRXData.Pivot[ID2].x - fRXData.Pivot[ID1].x;
    ty := fRXData.Pivot[ID2].y - fRXData.Pivot[ID1].y;
    for I := 0 to fRXData.Size[ID2].Y - 1 do
    for K := 0 to fRXData.Size[ID2].X - 1 do
    begin
      T2 := I * fRXData.Size[ID2].X + K;
      T1 := (I + ty) * fRXData.Size[ID1].X + K + tx;

      Alpha := fRXData.RGBA[ID2, T2] and $FF;
      fRXData.Mask[ID1, T1] := 255 - Round(Alpha / StepCount * 255);
    end;

    //Now we can discard building steps sprite
    fRXData.HasMask[ID1] := True;
    fRXData.Flag[ID2] := 0;
  end;
end;


//Grow house building masks to account for blurred shadows edges being visible
procedure TKMSpritePackEdit.GrowHouseMasks(aHouseDat: TKMResHouses);
var
  HT: THouseType;
  ID: Integer; //RGB and A index
  I, K, Lay: Integer;
  T: Integer;
  A, B, C, D: Byte;
begin
  for HT := HOUSE_MIN to HOUSE_MAX do
  for Lay := 1 to 2 do //House is rendered in two layers since Stone does not covers Wood parts in e.g. Sawmill
  begin
    ID := IfThen(Lay = 1, aHouseDat[HT].WoodPic, aHouseDat[HT].StonePic) + 1;

    //Grow the masks
    //Since shadows direction is X+ Y- we can do just one pass into that direction
    for I := fRXData.Size[ID].Y - 1 downto 0 do
    for K := 0 to fRXData.Size[ID].X - 1 do
    begin
      T := I * fRXData.Size[ID].X + K;
      //Check 4 neighbours for now
      if (fRXData.Mask[ID, T] = 255) then
      begin
        A := fRXData.Mask[ID, I * fRXData.Size[ID].X + Min(K + 1, fRXData.Size[ID].X - 1)];
        B := fRXData.Mask[ID, I * fRXData.Size[ID].X + Max(K - 1, 0)];
        C := fRXData.Mask[ID, Min(I + 1, fRXData.Size[ID].Y - 1) * fRXData.Size[ID].X + K];
        D := fRXData.Mask[ID, Max(I - 1, 0) * fRXData.Size[ID].X + K];

        fRXData.Mask[ID, T] := Min(Min(A, B), Min(C, D));
      end;
    end;
  end;
end;


procedure TKMSpritePackEdit.SoftWater(aTileset: TKMResTileset);
var
  I, J, K, T: Integer;
  AR, AG, AB: Cardinal;
  Samples: Cardinal;
  Tmp: array [0 .. 32 * 32 - 1] of Cardinal;

  procedure AddAccum(aColor: Cardinal);
  begin
    if aColor = 0 then Exit;
    AR := AR + aColor and $FF;
    AG := AG + aColor shr 8 and $FF;
    AB := AB + aColor shr 16 and $FF;
    Inc(Samples);
  end;
begin
  for I := 1 to 256 do
  if fRXData.Flag[I + 300] <> 0 then
  begin
    for J := 0 to fRXData.Size[I].Y - 1 do
    for K := 0 to fRXData.Size[I].X - 1 do
    begin
      T := J * fRXData.Size[I].X + K;

      if (fRXData.RGBA[I + 300, T] <> 0) then
      begin
        //We take advantage of the checkerboard pattern to fill missing pixels
        AR := 0;
        AG := 0;
        AB := 0;
        Samples := 0;
        if K > 0 then
          AddAccum(fRXData.RGBA[I, J * fRXData.Size[I].X + K - 1]);
        if K < fRXData.Size[I].X - 1 then
          AddAccum(fRXData.RGBA[I, J * fRXData.Size[I].X + K + 1]);
        if J > 0 then
          AddAccum(fRXData.RGBA[I, (J - 1) * fRXData.Size[I].X + K]);
        if J < fRXData.Size[I].Y - 1 then
          AddAccum(fRXData.RGBA[I, (J + 1) * fRXData.Size[I].X + K]);

        fRXData.RGBA[I, T] := (AR div Samples) or (AG div Samples) shl 8 or (AB div Samples) shr 16 or $FF000000;
      end;
    end;
  end;

  //Soften water
  for I := 301 to 4200 do
  if fRXData.Flag[I] <> 0 then
  begin
    for J := 0 to fRXData.Size[I].Y - 1 do
    for K := 0 to fRXData.Size[I].X - 1 do
    begin
      T := J * fRXData.Size[I].X + K;

      if (fRXData.RGBA[I, T] = 0) then
      begin
        //We take advantage of the checkerboard pattern to fill missing pixels
        AR := 0;
        AG := 0;
        AB := 0;
        Samples := 0;
        if K > 0 then
          AddAccum(fRXData.RGBA[I, J * fRXData.Size[I].X + K - 1]);
        if K < fRXData.Size[I].X - 1 then
          AddAccum(fRXData.RGBA[I, J * fRXData.Size[I].X + K + 1]);
        if J > 0 then
          AddAccum(fRXData.RGBA[I, (J - 1) * fRXData.Size[I].X + K]);
        if J < fRXData.Size[I].Y - 1 then
          AddAccum(fRXData.RGBA[I, (J + 1) * fRXData.Size[I].X + K]);

        if Samples > 0 then
          Tmp[T] := Round(AR / Samples) or (Round(AG / Samples) shl 8) or (Round(AB / Samples) shr 16) or (Samples * $30000000);
      end
      else
        Tmp[T] := (fRXData.RGBA[I, T] and $FFFFFF) or $B0000000;
    end;

    for J := 0 to fRXData.Size[I].Y - 1 do
    for K := 0 to fRXData.Size[I].X - 1 do
    begin
      T := J * fRXData.Size[I].X + K;
      fRXData.RGBA[I, T] := Tmp[T];
      Tmp[T] := 0;
    end;
  end;
end;


//Release RAM that is no longer needed
procedure TKMSpritePackEdit.ClearTemp;
var
  I: Integer;
begin
  inherited;

  for I := 1 to fRXData.Count do
    SetLength(fRXData.Data[I], 0);
end;


//Cut off empty pixels on sides
function TKMSpritePackEdit.TrimSprites: Cardinal;
var
  I,J,K: Integer;
  Right,Left,Bottom,Top: Word;
  OffX, OffY, NewX, NewY: Word;
  FoundPixel: Boolean;
begin
  Result := 0;

  for I := 1 to fRXData.Count do
  if (fRXData.Flag[I] <> 0) then
  begin
    if fRXData.Size[I].X * fRXData.Size[I].Y = 0 then Continue;
    //Check bounds
    Right  := 0;
    Bottom := 0;
    Left   := fRXData.Size[I].X - 1;
    Top    := fRXData.Size[I].Y - 1;
    FoundPixel := False;
    for J := 0 to fRXData.Size[I].Y - 1 do
    for K := 0 to fRXData.Size[I].X - 1 do
    if fRXData.RGBA[I, J * fRXData.Size[I].X + K] and $FF000000 <> 0 then
    begin
      Right  := Max(Right,  K);
      Bottom := Max(Bottom, J);
      Left   := Min(Left,   K);
      Top    := Min(Top,    J);
      FoundPixel := True;
    end;

    if not FoundPixel then //Entire image is transparent
    begin
      fRXData.Size[I].X := 1;
      fRXData.Size[I].Y := 1;
      Continue;
    end;

    Inc(Right);
    Inc(Bottom);
    Assert((Left <= Right) and (Top <= Bottom),'Left > Right or Top > Bottom');
    OffX := Left;
    OffY := Top;
    NewX := Right  - Left;
    NewY := Bottom - Top;

    Result := Result + (fRXData.Size[I].Y * fRXData.Size[I].X) - NewX * NewY;

    //Do the trimming
    for J := 0 to NewY - 1 do
    begin
      Move(
        fRXData.RGBA[I, (J + OffY) * fRXData.Size[I].X + OffX],
        fRXData.RGBA[I, J * NewX],
        NewX * 4); //RGBA is 4 bytes per pixel
      Move(
        fRXData.Mask[I, (J + OffY) * fRXData.Size[I].X + OffX],
        fRXData.Mask[I, J * NewX],
        NewX * 1); //Mask is 1 byte per pixel
    end;

    fRXData.Size[I].X := NewX;
    fRXData.Size[I].Y := NewY;
    fRXData.Pivot[I].X := fRXData.Pivot[I].X + OffX;
    fRXData.Pivot[I].Y := fRXData.Pivot[I].Y + OffY;
  end;
end;


procedure TKMSpritePackEdit.LoadFromRXFile(const aFileName: string);
var
  I: Integer;
  S: TMemoryStream;
begin
  if not FileExists(aFileName) then
  begin
    ShowMessage('File not found: ' + aFileName);
    Exit;
  end;

  S := TMemoryStream.Create;

  S.LoadFromFile(aFileName);
  S.ReadBuffer(fRXData.Count, 4);

  Allocate(fRXData.Count);

  S.ReadBuffer(fRXData.Flag[1], fRXData.Count);

  for I := 1 to fRXData.Count do
    if fRXData.Flag[I] = 1 then
    begin
      S.ReadBuffer(fRXData.Size[I].X, 4);
      S.ReadBuffer(fRXData.Pivot[I].X, 8);
      //Data part of each sprite is 8BPP paletted in KaM RX
      SetLength(fRXData.Data[I], fRXData.Size[I].X * fRXData.Size[I].Y);
      S.ReadBuffer(fRXData.Data[I,0], fRXData.Size[I].X * fRXData.Size[I].Y);
    end;
  S.Free;

  Expand; //Only KaM's rx needs expanding
end;


//Load sprites from folder
procedure TKMSpritePackEdit.LoadFromFolder(const aFolder: string);
var ft: TextFile;
begin
  if not DirectoryExists(aFolder) then Exit;
  if not FileExists(aFolder + IntToStr(Byte(fRT) + 1) + '.txt') then Exit;

  AssignFile(ft, aFolder + IntToStr(Byte(fRT) + 1) + '.txt');
    Reset(ft);
    ReadLn(ft, fRXData.Count);
  CloseFile(ft);

  Allocate(fRXData.Count);

  OverloadFromFolder(aFolder);
end;


procedure TKMSpritePackEdit.SaveToRXXFile(const aFileName: string);
var
  I: Integer;
  InputStream: TMemoryStream;
  OutputStream: TFileStream;
  CompressionStream: TCompressionStream;
begin
  ForceDirectories(ExtractFilePath(aFileName));

  InputStream := TMemoryStream.Create;

  InputStream.Write(fRXData.Count, 4);
  InputStream.Write(fRXData.Flag[1], fRXData.Count);

  for I := 1 to fRXData.Count do
    if fRXData.Flag[I] = 1 then
    begin
      InputStream.Write(fRXData.Size[I].X, 4);
      InputStream.Write(fRXData.Pivot[I].X, 8);
      InputStream.Write(fRXData.RGBA[I, 0], 4 * fRXData.Size[I].X * fRXData.Size[I].Y);
      InputStream.Write(fRXData.HasMask[I], 1);
      if fRXData.HasMask[I] then
        InputStream.Write(fRXData.Mask[I, 0], fRXData.Size[I].X * fRXData.Size[I].Y);
    end;
  OutputStream := TFileStream.Create(aFileName, fmCreate);
  CompressionStream := TCompressionStream.Create(clMax, OutputStream);
  InputStream.Position := 0;
  CompressionStream.CopyFrom(InputStream, InputStream.Size);
  CompressionStream.Free;
  OutputStream.Free;
  InputStream.Free;
end;


procedure TKMSpritePackEdit.GetImageToBitmap(aIndex: Integer; aBmp, aMask: TBitmap);
var
  I, K, W, H: Integer;
  T: Cardinal;
begin
  if fRXData.Flag[aIndex] = 0 then Exit;

  W := fRXData.Size[aIndex].X;
  H := fRXData.Size[aIndex].Y;

  aBmp.SetSize(W, H);
  if aMask <> nil then
    aMask.SetSize(W, H);

  for I := 0 to H - 1 do
  for K := 0 to W - 1 do
  begin
    T := fRXData.RGBA[aIndex, I * W + K];

    aBmp.Canvas.Pixels[K,I] := T and $FFFFFF;

    if (aMask <> nil) and fRXData.HasMask[aIndex] then
    begin
      T := fRXData.Mask[aIndex, I * W + K];

      aMask.Canvas.Pixels[K,I] := T * 65793;
    end;
  end;
end;


end.
