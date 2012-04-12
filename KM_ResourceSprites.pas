unit KM_ResourceSprites;
{$I KaM_Remake.inc}
interface
uses
  {$IFDEF WDC} PNGImage, {$ENDIF}
  {$IFDEF Unix} LCLIntf, LCLType, {$ENDIF}
  Classes, Graphics, Math, SysUtils,
  KM_CommonEvents, KM_Defaults, KM_Pics, KM_ResourceHouse, KM_ResourcePalettes, KM_Render, KM_TextLibrary
  {$IFDEF FPC}, zstream {$ENDIF}
  {$IFDEF WDC}, ZLib {$ENDIF}
  {$IFDEF FPC}, BGRABitmap {$ENDIF};

type
  TRXUsage = (ruMenu, ruGame); //Where sprites are used

  TRXInfo = record
    FileName: string; //Used for logging and filenames
    TeamColors: Boolean; //sprites should be generated with color masks
    Usage: TRXUsage; //Menu and Game sprites are loaded separately
    LoadingTextID: Word;
  end;


var
  RXInfo: array [TRXType] of TRXInfo = (
    (FileName: 'Trees';      TeamColors: False; Usage: ruGame; LoadingTextID: TX_MENU_LOADING_TREES;),
    (FileName: 'Houses';     TeamColors: True;  Usage: ruGame; LoadingTextID: TX_MENU_LOADING_HOUSES;),
    (FileName: 'Units';      TeamColors: True;  Usage: ruGame; LoadingTextID: TX_MENU_LOADING_UNITS;),
    (FileName: 'GUI';        TeamColors: True;  Usage: ruMenu; LoadingTextID: 0;),
    (FileName: 'GUIMain';    TeamColors: False; Usage: ruMenu; LoadingTextID: 0;),
    (FileName: 'GUIMainH';   TeamColors: False; Usage: ruMenu; LoadingTextID: 0;),
    (FileName: 'RemakeMenu'; TeamColors: True;  Usage: ruMenu; LoadingTextID: 0;),
    (FileName: 'Tileset';    TeamColors: False; Usage: ruGame; LoadingTextID: TX_MENU_LOADING_TILESET;),
    (FileName: 'RemakeGame'; TeamColors: True;  Usage: ruGame; LoadingTextID: TX_MENU_LOADING_ADDITIONAL_SPRITES;));

type
  TRXData = record
    Count: Integer;
    Flag: array of Byte; //Sprite is valid
    Size: array of record X,Y: Word; end;
    Pivot: array of record x,y: Integer; end;
    Data: array of array of Byte;
    RGBA: array of array of Cardinal; //Expanded image
    Mask: array of array of Byte; //Mask for team colors
    HasMask: array of Boolean; //Mask for team colors
  end;
  PRXData = ^TRXData;

  //Base class for Sprite loading
  TKMSpritePack = class
  private
    fPalettes: TKMPalettes;
    fRender: TRender;
    fRT: TRXType;
    fRXData: TRXData; //OOP wrapper for global variable
    procedure Allocate(aCount: Integer); //Allocate space for data that is being loaded
    procedure Expand;
    procedure SaveTextureToBMP(aWidth, aHeight: Word; aIndex: Integer; const Data: TCardinalArray; aSaveAlpha: Boolean);
  public
    constructor Create; overload;
    constructor Create(aRT: TRXType; aPalettes: TKMPalettes; aRender: TRender); overload;

    procedure AddImage(aFolder, aFilename: string; aIndex: Integer);
    procedure Delete(aIndex: Integer);
    property RXData: TRXData read fRXData;

    procedure LoadFromRXFile(const aFileName: string);
    procedure LoadFromRXXFile(const aFileName: string);
    procedure LoadFromFolder(const aFolder: string);
    procedure OverloadFromFolder(const aFolder: string);
    procedure MakeGFX(aAlphaShadows: Boolean);
    procedure MakeGFX_AlphaTest(aHouseDat: TKMHouseDatCollection);

    procedure SaveToRXXFile(const aFileName: string);
    procedure ExportToBMP(const aFolder: string);
    function TrimSprites: Cardinal; //For debug

    procedure ClearTemp; //Release non-required data
  end;

  //Overrides for:
  //GUI: Cursors
  //Houses: AlphaTest
  //Tiles: TGA file
  //

  TKMSprites = class
  private
    fAlphaShadows: Boolean; //Remember which state we loaded
    fRender: TRender;
    fPalettes: TKMPalettes;
    fSprites: array[TRXType] of TKMSpritePack;
    fStepProgress: TEvent;
    fStepCaption: TStringEvent;

    function GetRXFileName(aRX: TRXType): string;
    function GetSprites(aRT: TRXType): TKMSpritePack;
  public
    constructor Create(aRender: TRender; aPalettes: TKMPalettes; aStepProgress: TEvent; aStepCaption: TStringEvent);
    destructor Destroy; override;

    procedure LoadMenuResources;
    procedure LoadGameResources(aHouseDat: TKMHouseDatCollection; aTileTex: Cardinal; aAlphaShadows:boolean);
    procedure ClearTemp;

    property Sprites[aRT: TRXType]: TKMSpritePack read GetSprites; default;

    //Used externally to access raw RGBA data (e.g. by ExportAnim)
    procedure LoadSprites(aRT: TRXType; aAlphaShadows: Boolean);
    procedure ExportToBMP(aRT: TRXType);

    property AlphaShadows: Boolean read fAlphaShadows;
    property FileName[aRX: TRXType]: string read GetRXFileName;
  end;


var
  GFXData: array [TRXType] of array of record
    TexID, AltID: Cardinal; //AltID used for team colors
    u1,v1,u2,v2: Single; //Top-Left, Bottom-Right uv coords
    PxWidth, PxHeight: Word;
  end;


implementation
uses KromUtils, KM_Log, StrUtils;


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


{ TKMSpritePack }
constructor TKMSpritePack.Create;
begin
  inherited Create;

end;


//We need to access to palettes to properly Expand RX files
constructor TKMSpritePack.Create(aRT: TRXType; aPalettes: TKMPalettes; aRender: TRender);
begin
  inherited Create;

  fPalettes := aPalettes;
  fRender := aRender;
  fRT := aRT;
  //fRXData := @RXData[fRT];
end;


procedure TKMSpritePack.Delete(aIndex: Integer);
begin
  Assert(aIndex <= fRXData.Count);
  fRXData.Flag[aIndex] := 0;

  while (fRXData.Count > 0) and (fRXData.Flag[fRXData.Count] = 0) do
    Dec(fRXData.Count);
end;


procedure TKMSpritePack.Allocate(aCount: Integer);
begin
  fRXData.Count := aCount;

  aCount := fRXData.Count + 1;
  SetLength(GFXData[fRT],     aCount);
  SetLength(fRXData.Flag,     aCount);
  SetLength(fRXData.Size,     aCount);
  SetLength(fRXData.Pivot,    aCount);
  SetLength(fRXData.Data,     aCount);
  SetLength(fRXData.RGBA,     aCount);
  SetLength(fRXData.Mask,     aCount);
  SetLength(fRXData.HasMask,  aCount);
end;


//Convert paletted data into RGBA and select Team color layer from it
procedure TKMSpritePack.Expand;
  function HousesPal(aID: Integer): TKMPalData;
  const wip: array[0..55] of word = (3,4,25,43,44,116,118,119,120,121,123,126,127,136,137,140,141,144,145,148,149,213,214,237,238,241,242,243,246,247,252,253,257,258,275,276,336,338,360,361,365,366,370,371,380,381,399,400,665,666,670,671,1658,1660,1682,1684);
  var I: Byte;
  begin
    Result := fPalettes.DefDal;

    for I := 0 to High(wip) do
    if aID = wip[I] then
    begin
      Result := fPalettes[pal_lin];
      Exit;
    end;
  end;
var
  H: Integer;
  K, I: Integer;
  Palette: TKMPalData;
  L: byte;
  Pixel: Integer;
begin
  with fRXData do
  for H := 1 to Count do
  begin
    //Choose proper palette
    case fRT of
      rxHouses:   Palette := HousesPal(H);
      rxGuiMain:  Palette := fPalettes[RX5Pal[H]];
      rxGuiMainH: Palette := fPalettes[RX6Pal[H]];
      else        Palette := fPalettes.DefDal;
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

        if RXInfo[fRT].TeamColors and (L in [24..30])
        and ((fRT <> rxHouses) or (H > 400))  //Skip the Inn Weapon Smithy and the rest
        and ((fRT <> rxGui) or InRange(H, 141, 154) or InRange(H, 521, 550)) then //Unit icons and scrolls
        begin
          RGBA[H, Pixel] := cardinal(((L - 27) * 42 + 128) * 65793) OR $FF000000;
          case L of //Maybe it makes sense to convert to 8bit?
            24, 30: Mask[H, Pixel] := $60;   //7  //6
            25, 29: Mask[H, Pixel] := $90;   //11 //9
            26, 28: Mask[H, Pixel] := $C0;   //14 //12
            27:     Mask[H, Pixel] := $FF;   //16 //16
          end;
          HasMask[H] := True;
        end else
          RGBA[H, Pixel] := Palette.Color32(L);
      end;
    end;
  end;
end;


//Cut off empty pixels on sides
function TKMSpritePack.TrimSprites: Cardinal;
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


//Release RAM that is no longer needed
procedure TKMSpritePack.ClearTemp;
var I: Integer;
begin
  for I := 1 to fRXData.Count do
  begin
    SetLength(fRXData.Data[I], 0);
    SetLength(fRXData.RGBA[I], 0);
    SetLength(fRXData.Mask[I], 0);
  end;
end;


procedure TKMSpritePack.LoadFromRXFile(const aFileName: string);
var
  I: Integer;
  S: TMemoryStream;
begin
  if not CheckFileExists(aFileName) then
    Exit;

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
      //Data part of each sprite is 8BPP palleted in KaM RX
      SetLength(fRXData.Data[I], fRXData.Size[I].X * fRXData.Size[I].Y);
      S.ReadBuffer(fRXData.Data[I,0], fRXData.Size[I].X * fRXData.Size[I].Y);
    end;
  S.Free;
  fLog.AppendLog(RXInfo[fRT].FileName + ' -', fRXData.Count);

  Expand; //Only KaM's rx needs expanding
end;


procedure TKMSpritePack.LoadFromRXXFile(const aFileName: string);
var
  i: Integer;
  InputStream: TFileStream;
  DecompressionStream: TDecompressionStream;
begin
  if not CheckFileExists(aFileName) then
    Exit;

  InputStream := TFileStream.Create(aFileName, fmOpenRead or fmShareDenyNone);
  DecompressionStream := TDecompressionStream.Create(InputStream);

  try
    DecompressionStream.Read(fRXData.Count, 4);
    fLog.AppendLog(RXInfo[fRT].FileName + ' -', fRXData.Count);

    if fRXData.Count = 0 then
      Exit;

    Allocate(fRXData.Count);

    DecompressionStream.Read(fRXData.Flag[1], fRXData.Count);

    for I := 1 to fRXData.Count do
      if fRXData.Flag[I] = 1 then
      begin
        DecompressionStream.Read(fRXData.Size[I].X, 4);
        DecompressionStream.Read(fRXData.Pivot[I].X, 8);
        //Data part of each sprite is 32BPP RGBA in Remake RXX files
        SetLength(fRXData.RGBA[I], fRXData.Size[I].X * fRXData.Size[I].Y);
        SetLength(fRXData.Mask[I], fRXData.Size[I].X * fRXData.Size[I].Y);
        DecompressionStream.Read(fRXData.RGBA[I, 0], 4 * fRXData.Size[I].X * fRXData.Size[I].Y);
        DecompressionStream.Read(fRXData.HasMask[I], 1);
        if fRXData.HasMask[I] then
          DecompressionStream.Read(fRXData.Mask[I, 0], fRXData.Size[I].X * fRXData.Size[I].Y);
      end;
  finally
    DecompressionStream.Free;
    InputStream.Free;
  end;
end;


//Load sprites from folder
procedure TKMSpritePack.LoadFromFolder(const aFolder: string);
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


procedure TKMSpritePack.AddImage(aFolder, aFilename: string; aIndex: Integer);
var
  x,y:integer;
  T: Byte;
  ft: TextFile;
  Bmp: TBitmap;
  {$IFDEF WDC}
  p: Cardinal;
  po: TPNGObject;
  {$ENDIF}
  {$IFDEF FPC}
  po: TBGRABitmap;
  {$ENDIF}
begin
  if aIndex >= fRXData.Count then
    Allocate(aIndex);

  fRXData.HasMask[aIndex] := FileExists(aFolder + Copy(aFilename, 1, 6) + 'a.png') or
                             FileExists(aFolder + Copy(aFilename, 1, 6) + 'a.bmp');

  if SameText(RightStr(aFilename, 3), 'png') then
  begin
    {$IFDEF WDC}
    po := TPNGObject.Create;
    po.LoadFromFile(aFolder + aFilename);
    {$ENDIF}
    {$IFDEF FPC}
    po := TBGRABitmap.Create(aFolder + aFilename);
    {$ENDIF}

    fRXData.Flag[aIndex] := 1; //Mark as used (required for saving RXX)
    fRXData.Size[aIndex].X := po.Width;
    fRXData.Size[aIndex].Y := po.Height;

    SetLength(fRXData.RGBA[aIndex], po.Width*po.Height);
    SetLength(fRXData.Mask[aIndex], po.Width*po.Height); //Should allocate space for it's always comes along

    {$IFDEF WDC}
    case po.TransparencyMode of //There are ways to process PNG transparency
      ptmNone:
        for y:=0 to po.Height-1 do for x:=0 to po.Width-1 do
          fRXData.RGBA[aIndex, y*po.Width+x] := cardinal(po.Pixels[x,y]) OR $FF000000;
      ptmBit:
        for y:=0 to po.Height-1 do for x:=0 to po.Width-1 do
          if po.Pixels[x,y] = po.TransparentColor then
            fRXData.RGBA[aIndex, y*po.Width+x] := cardinal(po.Pixels[x,y]) AND $FFFFFF //avoid black edging
          else
            fRXData.RGBA[aIndex, y*po.Width+x] := cardinal(po.Pixels[x,y]) OR $FF000000;
      ptmPartial:
        for y:=0 to po.Height-1 do for x:=0 to po.Width-1 do begin
          p := (po.AlphaScanline[y]^[x]) shl 24; //semitransparency is killed by render later-on
          fRXData.RGBA[aIndex, y*po.Width+x] := cardinal(po.Pixels[x,y]) OR p;
        end;
      else Assert(false, 'Unknown PNG transparency mode')
    end;
    {$ENDIF}
    {$IFDEF FPC}
    for y:=0 to po.Height-1 do for x:=0 to po.Width-1 do
      fRXData.RGBA[aIndex, y*po.Width+x] := cardinal(po.GetPixel(x,y).red) OR (cardinal(po.GetPixel(x,y).green) shl 8) OR
                                           (cardinal(po.GetPixel(x,y).blue) shl 16) OR (cardinal(po.GetPixel(x,y).alpha) shl 24);
    {$ENDIF}
    po.Free;
  end;

  if SameText(RightStr(aFilename, 3), 'bmp') then
  begin
    Bmp :=TBitmap.Create;
    Bmp.LoadFromFile(aFolder + aFilename);

    fRXData.Flag[aIndex] := 1; //Mark as used (required for saving RXX)
    fRXData.Size[aIndex].X := Bmp.Width;
    fRXData.Size[aIndex].Y := Bmp.Height;

    SetLength(fRXData.RGBA[aIndex], Bmp.Width*Bmp.Height);
    SetLength(fRXData.Mask[aIndex], Bmp.Width*Bmp.Height); //Should allocate space for it's always comes along

    for y:=0 to Bmp.Height-1 do for x:=0 to Bmp.Width-1 do
      fRXData.RGBA[aIndex, y*Bmp.Width+x] := cardinal(Bmp.Canvas.Pixels[x,y]) OR $FF000000;
  end;

  //Load and process the mask if it exists
  if fRXData.HasMask[aIndex] then
  begin
    //PNG masks are designed for the artist, they take standard KaM reds so it's easier
    if FileExists(aFolder + Copy(aFilename, 1, 6) + 'a.png') then
    begin
      {$IFDEF WDC}
      po := TPNGObject.Create;
      po.LoadFromFile(aFolder + Copy(aFilename, 1, 6) + 'a.png');
      {$ENDIF}
      {$IFDEF FPC}
      po := TBGRABitmap.Create(aFolder + Copy(aFilename, 1, 6) + 'a.png');
      {$ENDIF}

      if (fRXData.Size[aIndex].X = po.Width) and (fRXData.Size[aIndex].Y = po.Height) then
      begin
        //We don't handle transparency in Masks
        {$IFDEF WDC}
        for y:=0 to po.Height-1 do for x:=0 to po.Width-1 do
        if cardinal(po.Pixels[x,y] AND $FF) <> 0 then
        begin
          T := fRXData.RGBA[aIndex, y*po.Width+x] AND $FF; //Take red component
          fRXData.Mask[aIndex, y*po.Width+x] := Byte(255-Abs(255-T*2));
          fRXData.RGBA[aIndex, y*po.Width+x] := T*65793 OR $FF000000;
        end;
        {$ENDIF}
        {$IFDEF FPC}
        for y:=0 to po.Height-1 do for x:=0 to po.Width-1 do
        if cardinal(po.GetPixel(x,y).red) <> 0 then
        begin
          T := fRXData.RGBA[aIndex, y*po.Width+x] AND $FF; //Take red component
          fRXData.Mask[aIndex, y*po.Width+x] := Byte(255-Abs(255-T*2));
          fRXData.RGBA[aIndex, y*po.Width+x] := T*65793 OR $FF000000;
        end;
        {$ENDIF}
      end;
      po.Free;
    end;

    //BMP masks are not like PNG masks, they take the raw pixel value.
    //We use them for creating soft shadows: BMP masks are exported from Remake then imported again
    if FileExists(aFolder + Copy(aFilename, 1, 6) + 'a.bmp') then
    begin
      Bmp := TBitmap.Create;
      Bmp.LoadFromFile(aFolder + Copy(aFilename, 1, 6) + 'a.bmp');

      if (fRXData.Size[aIndex].X = Bmp.Width) and (fRXData.Size[aIndex].Y = Bmp.Height) then
        for y:=0 to Bmp.Height-1 do for x:=0 to Bmp.Width-1 do
          fRXData.Mask[aIndex, y*Bmp.Width+x] := Byte(Bmp.Canvas.Pixels[x,y] AND $FF); //Take red, although it doesn't matter as we assume the input is greyscale

      Bmp.Free;
    end;
  end;

  //Read pivots
  if FileExists(aFolder + Copy(aFilename, 1, 6) + '.txt') then
  begin
    AssignFile(ft, aFolder + Copy(aFilename, 1, 6) + '.txt');
    Reset(ft);
    ReadLn(ft, fRXData.Pivot[aIndex].X);
    ReadLn(ft, fRXData.Pivot[aIndex].Y);
    CloseFile(ft);
  end;
end;


//Parse all valid files in Sprites folder and load them additionaly to or replacing original sprites
procedure TKMSpritePack.OverloadFromFolder(const aFolder: string);
var
  FileList: TStringList;
  SearchRec: TSearchRec;
  I, ID: Integer;
  RX: Byte;
begin
  RX := Byte(fRT) + 1;

  if not DirectoryExists(aFolder) then Exit;

  FileList := TStringList.Create;
  //PNGs
  FindFirst(aFolder + inttostr(RX)+'_????.png', faAnyFile AND NOT faDirectory, SearchRec);
  repeat
    FileList.Add(SearchRec.Name);
  until (FindNext(SearchRec)<>0);
  FindClose(SearchRec);
  //BMPs
  FindFirst(aFolder + inttostr(RX)+'_????.bmp', faAnyFile AND NOT faDirectory, SearchRec);
  repeat
    FileList.Add(SearchRec.Name);
  until (FindNext(SearchRec)<>0);
  FindClose(SearchRec);

  //#_####.png - Default texture
  //#_####a.png - Flag colors areas
  //#_####.txt - Pivot info

  for I := 0 to FileList.Count - 1 do
  begin
    if TryStrToInt(Copy(FileList.Strings[I], 3, 4), ID) then
      AddImage(aFolder, FileList.Strings[I], ID);
  end;

  FileList.Free;
end;


procedure TKMSpritePack.SaveToRXXFile(const aFileName: string);
var
  i:integer;
  InputStream: TMemoryStream;
  OutputStream: TFileStream;
  CompressionStream: TCompressionStream;
begin
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
  OutputStream := TFileStream.Create(aFileName,fmCreate);
  CompressionStream := TCompressionStream.Create(clMax, OutputStream);
  InputStream.Position := 0;
  CompressionStream.CopyFrom(InputStream, InputStream.Size);
  CompressionStream.Free;
  OutputStream.Free;
  InputStream.Free;
end;


//Export RX to Bitmaps without need to have GraphicsEditor, also this way we preserve image indexes
procedure TKMSpritePack.ExportToBMP(const aFolder: string);
var
  Bmp: TBitmap;
  ID, I, K: Integer;
  SizeX, SizeY: Integer;
begin
  ForceDirectories(aFolder);

  Bmp := TBitmap.Create;
  Bmp.PixelFormat := pf24bit;

  for ID:=1 to fRXData.Count do
  if fRXData.Flag[ID] = 1 then
  begin
    SizeX := fRXData.Size[ID].X;
    SizeY := fRXData.Size[ID].Y;
    Bmp.Width  := SizeX;
    Bmp.Height := SizeY;

    //Export RGB values
    for I := 0 to SizeY - 1 do
    for K := 0 to SizeX - 1 do
      Bmp.Canvas.Pixels[K,I] := fRXData.RGBA[ID, I*SizeX + K] and $FFFFFF;

    //Mark pivot location with a dot
    {K := SizeX + RXData[RT].Pivot[ID].x;
    I := SizeY + RXData[RT].Pivot[ID].y;
    if InRange(I, 0, SizeY-1) and InRange(K, 0, SizeX-1) then
      Bmp.Canvas.Pixels[K,I] := $FF00FF;}

    Bmp.SaveToFile(aFolder + IntToStr(byte(fRT)+1) + '_' + int2fix(ID, 4) + '.bmp');

    //Export Flag values
    if fRXData.HasMask[ID] then
    begin
      for I := 0 to SizeY - 1 do
      for K := 0 to SizeX - 1 do
        Bmp.Canvas.Pixels[K,I] := fRXData.Mask[ID, I*SizeX + K] * 65793;

      Bmp.SaveToFile(aFolder + IntToStr(byte(fRT)+1) + '_' + int2fix(ID, 4) + 'a.bmp');
    end;
  end;

  Bmp.Free;
end;


{ TKMSprites }
constructor TKMSprites.Create(aRender: TRender; aPalettes: TKMPalettes; aStepProgress: TEvent; aStepCaption: TStringEvent);
var
  RT: TRXType;
begin
  inherited Create;
  fRender := aRender;
  fPalettes := aPalettes;

  for RT := Low(TRXType) to High(TRXType) do
    fSprites[RT] := TKMSpritePack.Create(RT, fPalettes, fRender);

  fStepProgress := aStepProgress;
  fStepCaption := aStepCaption;
end;


destructor TKMSprites.Destroy;
var
  RT: TRXType;
begin
  for RT := Low(TRXType) to High(TRXType) do
    fSprites[RT].Free;

  inherited;
end;


//Clear unused RAM
procedure TKMSprites.ClearTemp;
var RT: TRXType;
begin
  for RT := Low(TRXType) to High(TRXType) do
    fSprites[RT].ClearTemp;
end;


function TKMSprites.GetRXFileName(aRX: TRXType): string;
begin
  Result := RXInfo[aRX].FileName;
end;


function TKMSprites.GetSprites(aRT: TRXType): TKMSpritePack;
begin
  Result := fSprites[aRT];
end;


procedure TKMSprites.LoadMenuResources;
var
  RT: TRXType;
begin
  for RT := Low(TRXType) to High(TRXType) do
    if (RXInfo[RT].Usage = ruMenu) and (RT <> rxTiles) then
    begin
      fStepCaption('Reading ' + RXInfo[RT].FileName + ' ...');
      LoadSprites(RT, False); //Menu resources never need alpha shadows
      fSprites[RT].MakeGFX(False);
      fStepProgress;
    end;
end;


procedure TKMSprites.LoadGameResources(aHouseDat: TKMHouseDatCollection; aTileTex: Cardinal; aAlphaShadows:boolean);
var RT: TRXType;
begin
  //Remember which version we load
  fAlphaShadows := aAlphaShadows;

  for RT := Low(TRXType) to High(TRXType) do
  if (RXInfo[RT].Usage = ruGame) and (RT <> rxTiles) then
  begin
    fStepCaption(fTextLibrary[RXInfo[RT].LoadingTextID]);
    fLog.AppendLog('Reading ' + RXInfo[RT].FileName + '.rx');
    LoadSprites(RT, aAlphaShadows);
    fSprites[RT].MakeGFX(aAlphaShadows);
  end;
  //Alpha_tested sprites for houses. They come after MakeGFX cos they will replace above data
  fSprites[rxHouses].MakeGFX_AlphaTest(aHouseDat);
end;


//Try to load RXX first, then RX, then use Folder
procedure TKMSprites.LoadSprites(aRT: TRXType; aAlphaShadows: Boolean);
begin
  if aAlphaShadows and FileExists(ExeDir + 'data\sprites\' + RXInfo[aRT].FileName + '_a.rxx') then
  begin
    fSprites[aRT].LoadFromRXXFile(ExeDir + 'data\sprites\' + RXInfo[aRT].FileName + '_a.rxx');
    //fSprites[aRT].OverloadFromFolder(ExeDir + 'Sprites\');
    //Don't need trimming either
  end
  else
  if FileExists(ExeDir + 'data\sprites\' + RXInfo[aRT].FileName + '.rxx') then
  begin
    fSprites[aRT].LoadFromRXXFile(ExeDir + 'data\sprites\' + RXInfo[aRT].FileName + '.rxx');
    //fSprites[aRT].OverloadFromFolder(ExeDir + 'Sprites\');
    //Don't need trimming either
  end
  else
  if FileExists(ExeDir + 'data\gfx\res\' + RXInfo[aRT].FileName + '.rx') then
  begin
    fSprites[aRT].LoadFromRXFile(ExeDir + 'data\gfx\res\' + RXInfo[aRT].FileName + '.rx');
    fSprites[aRT].OverloadFromFolder(ExeDir + 'Sprites\');
    fLog.AddToLog('Trimmed ' + IntToStr(fSprites[aRT].TrimSprites));
  end
  else
  if DirectoryExists(ExeDir + 'Sprites\') then
  begin
    fSprites[aRT].LoadFromFolder(ExeDir + 'Sprites\');
    fLog.AddToLog('Trimmed ' + IntToStr(fSprites[aRT].TrimSprites));
  end;
end;


{Take RX data and make nice textures out of it.
Textures should be POT to improve performance and avoid driver bugs
In result we have GFXData filled.}
procedure TKMSpritePack.MakeGFX(aAlphaShadows: Boolean);
var
  ci,j,i,k,LeftIndex,RightIndex,TexCount,SpanCount:integer;
  AllocatedRAM,RequiredRAM,ColorsRAM:cardinal;
  WidthPOT,HeightPOT:word;
  TD:array of cardinal;
  TA:array of cardinal;
  HasMsk:boolean;
  TexType:TTexFormat;
begin
  if SKIP_RENDER then Exit;
  if fRXData.Count = 0 then Exit;

  if aAlphaShadows and (fRT in [rxTrees,rxHouses,rxUnits,rxGui,rxGame]) then
    TexType := tf_NormalAlpha
  else
    TexType := tf_Normal;

  LeftIndex := 0;
  AllocatedRAM := 0;
  RequiredRAM := 0;
  ColorsRAM := 0;
  TexCount := 0;
  repeat
    inc(LeftIndex);

    WidthPOT  := fRXData.Size[LeftIndex].X;
    HeightPOT := MakePOT(fRXData.Size[LeftIndex].Y);
    SpanCount := 1;

    //Pack textures with same POT height into rows to save memory
    //This also means fewer textures for GPU RAM == better performance
    while((LeftIndex+SpanCount<fRXData.Count)and //Keep packing until end of sprites
          (
            (HeightPOT=MakePOT(fRXData.Size[LeftIndex+SpanCount].Y)) //Pack if HeightPOT matches
        or((HeightPOT>=MakePOT(fRXData.Size[LeftIndex+SpanCount].Y))AND(WidthPOT+fRXData.Size[LeftIndex+SpanCount].X<MakePOT(WidthPOT)))
          )and
          (WidthPOT+fRXData.Size[LeftIndex+SpanCount].X<=MAX_TEX_RESOLUTION)) //Pack until max Tex_Resolution approached
    do begin
      inc(WidthPOT,fRXData.Size[LeftIndex+SpanCount].X);
      if (fRT=rxGuiMain)and(RX5Pal[LeftIndex]<>RX5Pal[LeftIndex+SpanCount]) then break; //Don't align RX5 images for they use all different palettes
      if (fRT=rxGuiMainH)and(RX6Pal[LeftIndex]<>RX6Pal[LeftIndex+SpanCount]) then break; //Don't align RX6 images for they use all different palettes
      inc(SpanCount);
    end;

    RightIndex := LeftIndex+SpanCount-1;
    WidthPOT := MakePOT(WidthPOT);
    SetLength(TD,WidthPOT*HeightPOT+1);
    SetLength(TA,WidthPOT*HeightPOT+1);

    for i := 0 to HeightPOT - 1 do
    begin
      ci := 0;
      for j := LeftIndex to RightIndex do
        for k := 0 to fRXData.Size[j].X - 1 do
        begin
          if i < fRXData.Size[j].Y then
          begin
            //CopyMemory(TD[(i-1)*WidthPOT+ci-1], fRXData.RGBA[j,(i-1)*fRXData.Size[j].X+k-1], )
            TD[i*WidthPOT+ci] := fRXData.RGBA[j,i*fRXData.Size[j].X+k];
            TA[i*WidthPOT+ci] := (fRXData.Mask[j,i*fRXData.Size[j].X+k] SHL 24) OR $FFFFFF;
          end;
          inc(ci);
        end;
    end;

    HasMsk:=false;
    for j:=LeftIndex to RightIndex do
      HasMsk := HasMsk or fRXData.HasMask[j];

    //If we need to prepare textures for TeamColors          //special fix for iron mine logo
    if MAKE_TEAM_COLORS and RXInfo[fRT].TeamColors and (not ((fRT=rxGui)and InRange(49,LeftIndex,RightIndex))) then
    begin
      GFXData[fRT,LeftIndex].TexID := fRender.GenTexture(WidthPOT,HeightPOT,@TD[0],TexType);
      //TeamColors are done through alternative plain colored texture
      if HasMsk then begin
        GFXData[fRT,LeftIndex].AltID := fRender.GenTexture(WidthPOT,HeightPOT,@TA[0],tf_AltID);
        inc(ColorsRAM, (WidthPOT*HeightPOT) div 2); //GL_ALPHA4
      end;
    end
    else
      GFXData[fRT, LeftIndex].TexID := fRender.GenTexture(WidthPOT, HeightPOT, @TD[0], TexType);

    SaveTextureToBMP(WidthPOT, HeightPOT, GFXData[fRT, LeftIndex].TexID, @TD[0], False);
    if HasMsk then
      SaveTextureToBMP(WidthPOT, HeightPOT, GFXData[fRT, LeftIndex].TexID, @TA[0], False);

    SetLength(TD, 0);
    SetLength(TA, 0);

    K := 0;
    for j:=LeftIndex to RightIndex do begin //Hack to test AlphaTest
      GFXData[fRT, J].TexID := GFXData[fRT, LeftIndex].TexID;
      GFXData[fRT, J].AltID := GFXData[fRT, LeftIndex].AltID;
      GFXData[fRT, J].u1 := K / WidthPOT;
      GFXData[fRT, J].v1 := 0;
      Inc(K, fRXData.Size[J].X);
      GFXData[fRT, J].u2 := K / WidthPOT;
      GFXData[fRT, J].v2 := fRXData.Size[J].Y / HeightPOT;
      GFXData[fRT, J].PxWidth := fRXData.Size[J].X;
      GFXData[fRT, J].PxHeight := fRXData.Size[J].Y;
      Inc(RequiredRAM, fRXData.Size[J].X * fRXData.Size[J].Y * 4);
    end;

    Inc(AllocatedRAM, WidthPOT * HeightPOT * 4);
    Inc(LeftIndex, SpanCount - 1);
    Inc(TexCount);

  until (LeftIndex >= fRXData.Count);

  fLog.AppendLog(IntToStr(TexCount) + ' Textures created');
  fLog.AddToLog(IntToStr(AllocatedRAM div 1024) + '/' + IntToStr((AllocatedRAM - RequiredRAM) div 1024) + ' Kbytes allocated/wasted for units GFX when using Packing');
  fLog.AddToLog(IntToStr(ColorsRAM div 1024) + ' KBytes for team colors');
end;



{Take RX data and make nice textures out of it.
Textures should be POT to improve performance and avoid drivers bugs
In result we have GFXData filled.}
procedure TKMSpritePack.MakeGFX_AlphaTest(aHouseDat: TKMHouseDatCollection);
var
  HT: THouseType;
  ID1, ID2: Integer; //RGB and A index
  I, K, Lay, StepCount: Integer;
  T, tx, ty: Integer;
  alpha: Byte;
  WidthPOT, HeightPOT: Integer;
  TD: array of Cardinal;
begin
  if SKIP_RENDER then Exit;

  for HT := Low(THouseType) to High(THouseType) do
  if aHouseDat[HT].IsValid then
  for Lay := 1 to 2 do //House is rendered in two layers since Stone does not covers Wood parts in e.g. Sawmill
  begin
    if Lay = 1 then begin
      ID1 := aHouseDat[HT].WoodPic+1;
      ID2 := aHouseDat[HT].WoodPal+1;
      StepCount := aHouseDat[HT].WoodPicSteps;
    end else begin
      ID1 := aHouseDat[HT].StonePic+1;
      ID2 := aHouseDat[HT].StonePal+1;
      StepCount := aHouseDat[HT].StonePicSteps;
    end;

    Assert((fRXData.Size[ID1].X >= fRXData.Size[ID2].X) and (fRXData.Size[ID1].Y >= fRXData.Size[ID2].Y),
            Format('Mismatched sprites %d:%d - %d:%d', [Byte(fRT), ID1, Byte(fRT), ID2]));

    WidthPOT  := MakePOT(fRXData.Size[ID1].X);
    HeightPOT := MakePOT(fRXData.Size[ID1].Y);
    SetLength(TD, WidthPOT*HeightPOT);

    //Fill in colors RXData
    for I := 0 to fRXData.Size[ID1].Y - 1 do
      for K := 0 to fRXData.Size[ID1].X - 1 do
        TD[I * WidthPOT + K] := fRXData.RGBA[ID1, I * fRXData.Size[ID1].X + K];

    //Apply mask to where colors are (yes, it needs to be done in 2 steps, since offsets can mismatch)
    tx := fRXData.Pivot[ID2].x - fRXData.Pivot[ID1].x;
    ty := (fRXData.Pivot[ID2].y - fRXData.Pivot[ID1].y)*WidthPOT;
    for i := 0 to fRXData.Size[ID2].Y-1 do
    for k := 0 to fRXData.Size[ID2].X-1 do
    begin
      t := i*WidthPOT+k + tx + ty; //Shift by pivot, always positive

      Alpha := fRXData.RGBA[ID2, i * fRXData.Size[ID2].X + k] AND $FF;

      //Steps are going in normal order 1..n, but that last step has Alpha=0
      if TD[t] <> 0 then
        if Alpha <> 0 then //Default case
          TD[t] := TD[t] AND ($FFFFFF OR (255-round(Alpha*(255/StepCount))) shl 24)
        else
          TD[t] := TD[t] AND $01FFFFFF; //Place it as last step
    end;

    GFXData[fRT, ID1].TexID := fRender.GenTexture(WidthPOT, HeightPOT, @TD[0], tf_AlphaTest);

    SaveTextureToBMP(WidthPOT, HeightPOT, GFXData[fRT, ID1].TexID, @TD[0], True);

    SetLength(TD, 0);
    GFXData[fRT,ID1].AltID := 0;
    GFXData[fRT,ID1].u1    := 0;
    GFXData[fRT,ID1].v1    := 0;
    GFXData[fRT,ID1].u2    := fRXData.Size[ID1].X/WidthPOT;
    GFXData[fRT,ID1].v2    := fRXData.Size[ID1].Y/HeightPOT;
    GFXData[fRT,ID1].PxWidth := fRXData.Size[ID1].X;
    GFXData[fRT,ID1].PxHeight:= fRXData.Size[ID1].Y;
  end;
end;


procedure TKMSpritePack.SaveTextureToBMP(aWidth, aHeight: Word; aIndex: Integer; const Data: TCardinalArray; aSaveAlpha: Boolean);
var
  I, K: Word;
  Bmp: TBitmap;
  Folder: string;
begin
  if WriteAllTexturesToBMP then
  begin
    Folder := ExeDir + 'Export\GenTextures\';
    ForceDirectories(Folder);
    Bmp := TBitmap.Create;
    Bmp.PixelFormat := pf24bit;
    Bmp.Width  := aWidth;
    Bmp.Height := aHeight;

    for I := 0 to aHeight - 1 do
      for K := 0 to aWidth - 1 do
        Bmp.Canvas.Pixels[K, I] := ((PCardinal(Cardinal(@Data[0]) + (I * aWidth + K) * 4))^) AND $FFFFFF; //Ignore alpha
    Bmp.SaveToFile(Folder + Int2Fix(aIndex, 4) + '.bmp');

    //these Alphas are worth looking at
    if aSaveAlpha then
    begin
      for I := 0 to aHeight - 1 do
        for K := 0 to aWidth - 1 do
          Bmp.Canvas.Pixels[K, I] := ((PCardinal(Cardinal(@Data[0]) + (I * aWidth + K) * 4))^) SHR 24 * 65793; // convert A to RGB Greyscale
      Bmp.SaveToFile(Folder + Int2Fix(aIndex, 4) + 'a.bmp');
    end;

    Bmp.Free;
  end;
end;


procedure TKMSprites.ExportToBMP(aRT: TRXType);
begin
  LoadSprites(aRT, True); //BMP can't show the alpha channel so don't load alpha shadows
  fSprites[aRT].ExportToBMP(ExeDir + 'Export\' + RXInfo[aRT].FileName + '.rx\');
  ClearTemp;
end;


end.
