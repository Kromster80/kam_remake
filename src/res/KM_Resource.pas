unit KM_Resource;
{$I KaM_Remake.inc}
interface
uses
  {$IFDEF Unix} LCLIntf, LCLType, {$ENDIF}
  Classes, Graphics, SysUtils,
  KM_CommonTypes, KM_Defaults, KM_Pics,
  KM_Render,
  KM_ResCursors,
  KM_ResFonts,
  KM_ResHouses,
  KM_ResLocales,
  KM_ResMapElements,
  KM_ResPalettes,
  KM_ResSound,
  KM_ResSprites,
  KM_ResTileset,
  KM_ResUnits,
  KM_ResWares;


type
  TResourceLoadState = (rlsNone, rlsMenu, rlsAll); //Resources are loaded in 2 steps, for menu and the rest

  TKMResource = class
  private
    fDataState: TResourceLoadState;

    fCursors: TKMResCursors;
    fFonts: TKMResFonts;
    fHouses: TKMResHouses;
    fUnits: TKMResUnits;
    fPalettes: TKMResPalettes;
    fWares: TKMResWares;
    fSounds: TKMResSounds;
    fSprites: TKMResSprites;
    fTileset: TKMResTileset;
    fMapElements: TKMResMapElements;

    procedure StepRefresh;
    procedure StepCaption(const aCaption: UnicodeString);
  public
    OnLoadingStep: TEvent;
    OnLoadingText: TUnicodeStringEvent;

    constructor Create(aOnLoadingStep: TEvent; aOnLoadingText: TUnicodeStringEvent);
    destructor Destroy; override;

    function GetDATCRC: Cardinal;

    procedure LoadMainResources(aLocale: AnsiString = ''; aLoadFullFonts: Boolean = True);
    procedure LoadLocaleResources(aLocale: AnsiString = '');
    procedure LoadGameResources(aAlphaShadows: boolean);
    procedure LoadLocaleFonts(aLocale: AnsiString; aLoadFullFonts: Boolean);

    property DataState: TResourceLoadState read fDataState;
    property Cursors: TKMResCursors read fCursors;
    property Houses: TKMResHouses read fHouses;
    property MapElements: TKMResMapElements read fMapElements;
    property Palettes: TKMResPalettes read fPalettes;
    property Fonts: TKMResFonts read fFonts;
    property Wares: TKMResWares read fWares;
    property Sounds: TKMResSounds read fSounds;
    property Sprites: TKMResSprites read fSprites;
    property Tileset: TKMResTileset read fTileset;
    property Units: TKMResUnits read fUnits;

    function IsMsgHouseUnnocupied(aMsgId: Word): Boolean;

    procedure ExportTreeAnim;
    procedure ExportHouseAnim;
    procedure ExportUnitAnim;
  end;


var
  gRes: TKMResource;


implementation
uses
  KromUtils, KM_Log, KM_Points, KM_ResTexts, KM_ResKeys;


{ TKMResource }
constructor TKMResource.Create(aOnLoadingStep: TEvent; aOnLoadingText: TUnicodeStringEvent);
begin
  inherited Create;

  fDataState := rlsNone;
  gLog.AddTime('Resource loading state - None');

  OnLoadingStep := aOnLoadingStep;
  OnLoadingText := aOnLoadingText;
end;


destructor TKMResource.Destroy;
begin
  FreeAndNil(fCursors);
  FreeAndNil(fHouses);
  FreeAndNil(gResLocales);
  FreeAndNil(fMapElements);
  FreeAndNil(fPalettes);
  FreeAndNil(fFonts);
  FreeAndNil(fWares);
  FreeAndNil(fSprites);
  FreeAndNil(fSounds);
  FreeAndNil(gResTexts);
  FreeAndNil(fTileset);
  FreeAndNil(fUnits);
  FreeAndNil(gResKeys);
  inherited;
end;


procedure TKMResource.StepRefresh;
begin
  if Assigned(OnLoadingStep) then OnLoadingStep;
end;


procedure TKMResource.StepCaption(const aCaption: UnicodeString);
begin
  if Assigned(OnLoadingText) then OnLoadingText(aCaption);
end;


//CRC of data files that can cause inconsitencies
function TKMResource.GetDATCRC: Cardinal;
begin
  Result := fHouses.CRC xor
            fUnits.CRC xor
            fMapElements.CRC xor
            fTileset.CRC;
end;


procedure TKMResource.LoadMainResources(aLocale: AnsiString = ''; aLoadFullFonts: Boolean = True);
begin
  StepCaption('Reading palettes ...');
  fPalettes := TKMResPalettes.Create;
  //We are using only default palette in the game for now, so no need to load all palettes
  fPalettes.LoadDefaultPalette(ExeDir + 'data' + PathDelim + 'gfx' + PathDelim);
  gLog.AddTime('Reading palettes', True);

  fSprites := TKMResSprites.Create(StepRefresh, StepCaption);

  fCursors := TKMResCursors.Create;
  fSprites.LoadMenuResources;
  fCursors.MakeCursors(fSprites[rxGui]);
  fCursors.Cursor := kmc_Default;

  gResKeys := TKMKeyLibrary.Create;

  // Locale info is needed for DAT export and font loading
  LoadLocaleResources(aLocale);

  StepCaption('Reading fonts ...');
  fFonts := TKMResFonts.Create;
  if aLoadFullFonts or gResLocales.LocaleByCode(aLocale).NeedsFullFonts then
    fFonts.LoadFonts(fll_Full)
  else
    fFonts.LoadFonts(fll_Minimal);
  gLog.AddTime('Read fonts is done');

  fTileset := TKMResTileset.Create(ExeDir + 'data'+PathDelim+'defines'+PathDelim+'pattern.dat');
  fTileset.TileColor := fSprites.Sprites[rxTiles].GetSpriteColors(248); //Tiles 249..256 are road overlays

  fMapElements := TKMResMapElements.Create;
  fMapElements.LoadFromFile(ExeDir + 'data'+PathDelim+'defines'+PathDelim+'mapelem.dat');

  fSprites.ClearTemp;

  fWares := TKMResWares.Create;
  fHouses := TKMResHouses.Create;
  fUnits := TKMResUnits.Create;

  StepRefresh;
  gLog.AddTime('ReadGFX is done');
  fDataState := rlsMenu;
  gLog.AddTime('Resource loading state - Menu');
end;


procedure TKMResource.LoadLocaleResources(aLocale: AnsiString = '');
begin
  FreeAndNil(gResLocales);
  FreeAndNil(gResTexts);
  FreeAndNil(fSounds);

  gResLocales := TKMLocales.Create(ExeDir + 'data' + PathDelim + 'locales.txt', aLocale);

  gResTexts := TKMTextLibraryMulti.Create;
  gResTexts.LoadLocale(ExeDir + 'data' + PathDelim + 'text' + PathDelim + 'text.%s.libx');

  fSounds := TKMResSounds.Create(gResLocales.UserLocale, gResLocales.FallbackLocale, gResLocales.DefaultLocale);
end;


procedure TKMResource.LoadLocaleFonts(aLocale: AnsiString; aLoadFullFonts: Boolean);
begin
  if (Fonts.LoadLevel <> fll_Full)
  and (aLoadFullFonts or gResLocales.LocaleByCode(aLocale).NeedsFullFonts) then
    Fonts.LoadFonts(fll_Full);
end;



procedure TKMResource.LoadGameResources(aAlphaShadows: Boolean);
begin
  if (fDataState <> rlsAll) or (aAlphaShadows <> fSprites.AlphaShadows) then
  begin
    fSprites.LoadGameResources(aAlphaShadows);
    fSprites.ClearTemp;
  end;

  fDataState := rlsAll;
  gLog.AddTime('Resource loading state - Game');
end;


function TKMResource.IsMsgHouseUnnocupied(aMsgId: Word): Boolean;
begin
  Result := (aMsgId >= TX_MSG_HOUSE_UNOCCUPIED__22) and (aMsgId <= TX_MSG_HOUSE_UNOCCUPIED__22 + 22);
end;


//Export Units graphics categorized by Unit and Action
procedure TKMResource.ExportUnitAnim;
var
  Folder: string;
  Bmp: TBitmap;
  U: TUnitType;
  A: TUnitActionType;
  D: TKMDirection;
  R: TWareType;
  T: TKMUnitThought;
  i,ci:integer;
  sy,sx,y,x:integer;
  Used:array of Boolean;
  RXData: TRXData;
begin
  fSprites.LoadSprites(rxUnits, False); //BMP can't show alpha shadows anyways
  RXData := fSprites[rxUnits].RXData;

  Folder := ExeDir + 'Export'+PathDelim+'UnitAnim'+PathDelim;
  ForceDirectories(Folder);

  Bmp := TBitmap.Create;
  Bmp.PixelFormat := pf24bit;

  if fUnits = nil then
    fUnits := TKMResUnits.Create;

  for U := WARRIOR_MIN to WARRIOR_MAX do
  for A := Low(TUnitActionType) to High(TUnitActionType) do
  for D := dir_N to dir_NW do
  if fUnits[U].UnitAnim[A,D].Step[1] <> -1 then
  for i := 1 to fUnits[U].UnitAnim[A, D].Count do
  begin
    ForceDirectories(Folder + fUnits[U].GUIName + PathDelim + UnitAct[A] + PathDelim);

    if fUnits[U].UnitAnim[A,D].Step[i] + 1 <> 0 then
    begin
      ci := fUnits[U].UnitAnim[A,D].Step[i] + 1;

      sx := RXData.Size[ci].X;
      sy := RXData.Size[ci].Y;
      Bmp.Width := sx;
      Bmp.Height := sy;

      for y:=0 to sy-1 do
      for x:=0 to sx-1 do
        Bmp.Canvas.Pixels[x,y] := RXData.RGBA[ci, y*sx+x] AND $FFFFFF;

      if sy > 0 then
        Bmp.SaveToFile(Folder +
          fUnits[U].GUIName + PathDelim + UnitAct[A] + PathDelim +
          'Dir' + IntToStr(Byte(D)) + '_' + int2fix(i, 2) + '.bmp');
    end;
  end;

  CreateDir(Folder + '_Unused');
  SetLength(Used, Length(RXData.Size));

  //Exclude actions
  for U := Low(TUnitType) to High(TUnitType) do
  for A := Low(TUnitActionType) to High(TUnitActionType) do
  for D := dir_N to dir_NW do
  if fUnits[U].UnitAnim[A,D].Step[1] <> -1 then
  for i := 1 to fUnits[U].UnitAnim[A,D].Count do
    Used[fUnits[U].UnitAnim[A,D].Step[i]+1] := fUnits[U].UnitAnim[A,D].Step[i]+1 <> 0;

  //Exclude serfs carrying stuff
  for R := Low(TWareType) to High(TWareType) do
  if R in [WARE_MIN..WARE_MAX] then
  for D := dir_N to dir_NW do
  if fUnits.SerfCarry[R,D].Step[1] <> -1 then
  for i := 1 to fUnits.SerfCarry[R,D].Count do
    Used[fUnits.SerfCarry[R,D].Step[i]+1] := fUnits.SerfCarry[R,D].Step[i]+1 <> 0;

  for T := Low(TKMUnitThought) to High(TKMUnitThought) do
  for i := ThoughtBounds[T,1] to  ThoughtBounds[T,2] do
    Used[I+1] := True;

  for ci:=1 to length(Used)-1 do
  if not Used[ci] then
  begin
    sx := RXData.Size[ci].X;
    sy := RXData.Size[ci].Y;
    Bmp.Width := sx;
    Bmp.Height := sy;

    for y:=0 to sy-1 do for x:=0 to sx-1 do
      Bmp.Canvas.Pixels[x,y] := RXData.RGBA[ci, y*sx+x] AND $FFFFFF;

    if sy>0 then Bmp.SaveToFile(Folder + '_Unused'+PathDelim+'_'+int2fix(ci,4) + '.bmp');
  end;

  fSprites.ClearTemp;
  Bmp.Free;
end;


//Export Houses graphics categorized by House and Action
procedure TKMResource.ExportHouseAnim;
var
  Folder: string;
  Bmp: TBitmap;
  HD: TKMResHouses;
  ID: THouseType;
  Ac: THouseActionType;
  Q, Beast, i, k, ci: Integer;
  sy, sx, y, x: Integer;
  RXData: TRXData;
begin
  fSprites.LoadSprites(rxHouses, False); //BMP can't show alpha shadows anyways
  RXData := fSprites[rxHouses].RXData;

  Folder := ExeDir + 'Export'+PathDelim+'HouseAnim'+PathDelim;
  ForceDirectories(Folder);

  Bmp := TBitmap.Create;
  Bmp.PixelFormat := pf24bit;

  HD := TKMResHouses.Create;

  ci:=0;
  for ID := HOUSE_MIN to HOUSE_MAX do
    for Ac:=ha_Work1 to ha_Flag3 do
      for k:=1 to HD[ID].Anim[Ac].Count do
      begin
        ForceDirectories(Folder+HD[ID].HouseName+'_'+HouseAction[Ac]+PathDelim);
        if HD[ID].Anim[Ac].Step[k] <> -1 then
          ci := HD[ID].Anim[Ac].Step[k]+1;

        sx := RXData.Size[ci].X;
        sy := RXData.Size[ci].Y;
        Bmp.Width:=sx;
        Bmp.Height:=sy;

        for y:=0 to sy-1 do for x:=0 to sx-1 do
          Bmp.Canvas.Pixels[x,y] := RXData.RGBA[ci,y*sx+x] AND $FFFFFF;

        if sy>0 then Bmp.SaveToFile(
        Folder+HD[ID].HouseName+'_'+HouseAction[Ac]+PathDelim+'_'+int2fix(k,2)+'.bmp');
      end;

  ci:=0;
  for Q:=1 to 2 do
  begin
    if Q=1 then ID:=ht_Swine
           else ID:=ht_Stables;
    CreateDir(Folder+'_'+HD[ID].HouseName+PathDelim);
    for Beast:=1 to 5 do
      for i:=1 to 3 do
        for k:=1 to HD.BeastAnim[ID,Beast,i].Count do
        begin
          CreateDir(Folder+'_'+HD[ID].HouseName+PathDelim+int2fix(Beast,2)+PathDelim);
          if HD.BeastAnim[ID,Beast,i].Step[k]+1<>0 then
            ci := HD.BeastAnim[ID,Beast,i].Step[k]+1;

          sx:=RXData.Size[ci].X;
          sy:=RXData.Size[ci].Y;
          Bmp.Width:=sx;
          Bmp.Height:=sy;

          for y:=0 to sy-1 do for x:=0 to sx-1 do
            Bmp.Canvas.Pixels[x,y] := RXData.RGBA[ci,y*sx+x] AND $FFFFFF;

          if sy>0 then Bmp.SaveToFile(Folder+'_'+HD[ID].HouseName+PathDelim+int2fix(Beast,2)+PathDelim+'_'+int2fix(i,1)+'_'+int2fix(k,2)+'.bmp');
        end;
  end;

  HD.Free;
  fSprites.ClearTemp;
  Bmp.Free;
end;


//Export Trees graphics categorized by ID
procedure TKMResource.ExportTreeAnim;
var
  RXData: TRXData;
  Folder: string;
  Bmp: TBitmap;
  I, K, L, M: Integer;
  SpriteID: Integer;
  SizeY,SizeX: Integer;
begin
  fSprites.LoadSprites(rxTrees, False);
  RXData := fSprites[rxTrees].RXData;

  Folder := ExeDir + 'Export'+PathDelim+'TreeAnim'+PathDelim;
  ForceDirectories(Folder);

  Bmp := TBitmap.Create;
  Bmp.PixelFormat := pf24bit;

  for I := 0 to fMapElements.Count - 1 do
  if (gMapElements[I].Anim.Count > 0) and (gMapElements[I].Anim.Step[1] > 0) then
  begin
    for K := 1 to gMapElements[I].Anim.Count do
    if gMapElements[I].Anim.Step[K]+1 <> 0 then
    begin
      SpriteID := gMapElements[I].Anim.Step[K]+1;

      SizeX := RXData.Size[SpriteID].X;
      SizeY := RXData.Size[SpriteID].Y;
      Bmp.Width := SizeX;
      Bmp.Height := SizeY;

      for L := 0 to SizeY - 1 do
      for M := 0 to SizeX - 1 do
        Bmp.Canvas.Pixels[M,L] := RXData.RGBA[SpriteID, L * SizeX + M] and $FFFFFF;

      //We can insert field here and press Export>TreeAnim. Rename each folder after export to 'Cuttable',
      //'Quad' and etc.. there you'll have it. Note, we use 1..254 counting, JBSnorro uses 0..253 counting
      if SizeX * SizeY > 0 then
        Bmp.SaveToFile(Folder + Format('%.3d_%.2d(%.2d)', [I, K, SpriteID]) + '.bmp');
    end;
  end;

  fSprites.ClearTemp;
  Bmp.Free;
end;


end.
