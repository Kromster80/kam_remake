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

    procedure LoadMainResources(const aLocale: AnsiString = ''; aLoadFullFonts: Boolean = True);
    procedure LoadLocaleResources(const aLocale: AnsiString = '');
    procedure LoadGameResources(aAlphaShadows: Boolean; aForceReload: Boolean = False);
    procedure LoadLocaleFonts(const aLocale: AnsiString; aLoadFullFonts: Boolean);

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

    procedure UpdateStateIdle;

    function IsMsgHouseUnnocupied(aMsgId: Word): Boolean;

    procedure ExportTreeAnim;
    procedure ExportHouseAnim;
    procedure ExportUnitAnim(aUnitFrom, aUnitTo: TUnitType; aExportUnused: Boolean = False);
  end;


var
  gRes: TKMResource;


implementation
uses
  TypInfo, KromUtils, KM_Log, KM_Points, KM_ResTexts, KM_ResKeys;


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


procedure TKMResource.UpdateStateIdle;
begin
  fSprites.UpdateStateIdle;
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


procedure TKMResource.LoadMainResources(const aLocale: AnsiString = ''; aLoadFullFonts: Boolean = True);
begin
  StepCaption('Reading palettes ...');
  fPalettes := TKMResPalettes.Create;
  //We are using only default palette in the game for now, so no need to load all palettes
  fPalettes.LoadDefaultPalette(ExeDir + 'data' + PathDelim + 'gfx' + PathDelim);
  gLog.AddTime('Reading palettes', True);


  fSprites := TKMResSprites.Create(StepRefresh, StepCaption);

  fCursors := TKMResCursors.Create;

  fUnits := TKMResUnits.Create; // Load units prior to Sprites, as we could use it on SoftenShadows override for png in Sprites folder
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

  StepRefresh;
  gLog.AddTime('ReadGFX is done');
  fDataState := rlsMenu;
  gLog.AddTime('Resource loading state - Menu');
end;


procedure TKMResource.LoadLocaleResources(const aLocale: AnsiString = '');
begin
  FreeAndNil(gResLocales);
  FreeAndNil(gResTexts);
  FreeAndNil(fSounds);

  gResLocales := TKMLocales.Create(ExeDir + 'data' + PathDelim + 'locales.txt', aLocale);

  gResTexts := TKMTextLibraryMulti.Create;
  gResTexts.LoadLocale(ExeDir + 'data' + PathDelim + 'text' + PathDelim + 'text.%s.libx');

  fSounds := TKMResSounds.Create(gResLocales.UserLocale, gResLocales.FallbackLocale, gResLocales.DefaultLocale);
end;


procedure TKMResource.LoadLocaleFonts(const aLocale: AnsiString; aLoadFullFonts: Boolean);
begin
  if (Fonts.LoadLevel <> fll_Full)
    and (aLoadFullFonts or gResLocales.LocaleByCode(aLocale).NeedsFullFonts) then
    Fonts.LoadFonts(fll_Full);
end;



procedure TKMResource.LoadGameResources(aAlphaShadows: Boolean; aForceReload: Boolean = False);
var
  DoForceReload: Boolean;
begin
  DoForceReload := aForceReload or (aAlphaShadows <> fSprites.AlphaShadows);
  if (fDataState <> rlsAll) or DoForceReload then
  begin
    fSprites.LoadGameResources(aAlphaShadows, DoForceReload);
    if not DoForceReload then
    begin
      fDataState := rlsAll;
      fSprites.ClearTemp;
    end;
  end;

  gLog.AddTime('Resource loading state - Game');
end;


function TKMResource.IsMsgHouseUnnocupied(aMsgId: Word): Boolean;
begin
  Result := (aMsgId >= TX_MSG_HOUSE_UNOCCUPIED__22) and (aMsgId <= TX_MSG_HOUSE_UNOCCUPIED__22 + 22);
end;



//Export Units graphics categorized by Unit and Action
procedure TKMResource.ExportUnitAnim(aUnitFrom, aUnitTo: TUnitType; aExportUnused: Boolean = False);
var
  FullFolder,Folder: string;
  U: TUnitType;
  A: TUnitActionType;
  Anim: TKMAnimLoop;
  D: TKMDirection;
  R: TWareType;
  T: TKMUnitThought;
  i,ci:integer;
  sy,sx,y,x:integer;
  Used:array of Boolean;
  RXData: TRXData;
  SpritePack: TKMSpritePack;
  SList: TStringList;
  FolderCreated: Boolean;
begin
  fSprites.LoadSprites(rxUnits, False); //BMP can't show alpha shadows anyways
  SpritePack := fSprites[rxUnits];
  RXData := SpritePack.RXData;

  Folder := ExeDir + 'Export' + PathDelim + 'UnitAnim' + PathDelim;
  ForceDirectories(Folder);

  SList := TStringList.Create;
  try
    if fUnits = nil then
      fUnits := TKMResUnits.Create;

    for U := aUnitFrom to aUnitTo do
      for A := Low(TUnitActionType) to High(TUnitActionType) do
      begin
        FolderCreated := False;
        for D := dir_N to dir_NW do
          if fUnits[U].UnitAnim[A,D].Step[1] <> -1 then
            for i := 1 to fUnits[U].UnitAnim[A, D].Count do
            begin
              ci := fUnits[U].UnitAnim[A,D].Step[i] + 1;
              if ci <> 0 then
              begin
                if not FolderCreated then
                begin
                  //Use default locale for Unit GUIName, as translation could be not good for file system (like russian 'Крестьянин/Винодел' with slash in it)
                  FullFolder := Folder + gResTexts.DefaultTexts[fUnits[U].GUITextID] + PathDelim + UnitAct[A] + PathDelim;
                  ForceDirectories(FullFolder);
                  FolderCreated := True;
                end;
                SpritePack.ExportFullImageData(FullFolder, ci, SList);
              end;
            end;
      end;

    SetLength(Used, Length(RXData.Size));

    //Exclude actions
    for U := Low(TUnitType) to High(TUnitType) do
      for A := Low(TUnitActionType) to High(TUnitActionType) do
        for D := dir_N to dir_NW do
          if fUnits[U].UnitAnim[A,D].Step[1] <> -1 then
          for i := 1 to fUnits[U].UnitAnim[A,D].Count do
          begin
            ci := fUnits[U].UnitAnim[A,D].Step[i]+1;
            Used[ci] := ci <> 0;
          end;

    if ut_Serf in [aUnitFrom..aUnitTo] then
      //serfs carrying stuff
      for R := WARE_MIN to WARE_MAX do
      begin
        FolderCreated := False;
        for D := dir_N to dir_NW do
        begin
          Anim := fUnits.SerfCarry[R, D];
          for i := 1 to Anim.Count do
          begin
            ci := Anim.Step[i]+1;
            if ci <> 0 then
            begin
              Used[ci] := True;
              if ut_Serf in [aUnitFrom..aUnitTo] then
              begin
                if not FolderCreated then
                begin
                  //Use default locale for Unit GUIName, as translation could be not good for file system (like russian 'Крестьянин/Винодел' with slash in it)
                  FullFolder := Folder + gResTexts.DefaultTexts[fUnits[ut_Serf].GUITextID] + PathDelim + 'Delivery' + PathDelim
                                  + GetEnumName(TypeInfo(TWareType), Integer(R)) + PathDelim;
                  ForceDirectories(FullFolder);
                  FolderCreated := True;
                end;
                SpritePack.ExportFullImageData(FullFolder, ci, SList);
              end;
            end;
          end;
        end;
      end;

    FullFolder := Folder + 'Thoughts' + PathDelim;
    ForceDirectories(FullFolder);
    for T := th_Eat to High(TKMUnitThought) do
      for I := ThoughtBounds[T,1] to  ThoughtBounds[T,2] do
      begin
        SpritePack.ExportFullImageData(FullFolder, I+1, SList);
        Used[I+1] := True;
      end;

    if not aExportUnused then Exit;

    FullFolder := Folder + '_Unused' + PathDelim;
    ForceDirectories(FullFolder);

    for ci := 1 to length(Used)-1 do
      if not Used[ci] then
        SpritePack.ExportFullImageData(FullFolder, ci, SList);
  finally
    fSprites.ClearTemp;
    SList.Free;
  end;
end;


//Export Houses graphics categorized by House and Action
procedure TKMResource.ExportHouseAnim;
var
  FullFolder,Folder: string;
  HD: TKMResHouses;
  ID: THouseType;
  Ac: THouseActionType;
  Q, Beast, I, K, ci: Integer;
  RXData: TRXData;
  SpritePack: TKMSpritePack;
  SList: TStringList;
begin
  fSprites.LoadSprites(rxHouses, False); //BMP can't show alpha shadows anyways
  SpritePack := fSprites[rxHouses];
  RXData := SpritePack.RXData;
  SList := TStringList.Create;

  Folder := ExeDir + 'Export' + PathDelim + 'HouseAnim' + PathDelim;
  ForceDirectories(Folder);

  HD := TKMResHouses.Create;

  for ID := HOUSE_MIN to HOUSE_MAX do
    for Ac := ha_Work1 to ha_Flag3 do
      for K := 1 to HD[ID].Anim[Ac].Count do
      begin
        FullFolder := Folder + HD[ID].HouseName + PathDelim + HouseAction[Ac] + PathDelim;
        ForceDirectories(FullFolder);
        ci := HD[ID].Anim[Ac].Step[K] + 1;
        if ci <> 0 then
          SpritePack.ExportFullImageData(FullFolder, ci, SList);
      end;

  for Q := 1 to 2 do
  begin
    if Q = 1 then
      ID := ht_Swine
    else
      ID := ht_Stables;
    ForceDirectories(Folder + '_' + HD[ID].HouseName+PathDelim);
    for Beast := 1 to 5 do
      for I := 1 to 3 do
        for K := 1 to HD.BeastAnim[ID,Beast,I].Count do
        begin
          FullFolder := Folder + HD[ID].HouseName + PathDelim + 'Beast' + PathDelim + int2fix(Beast,2) + PathDelim;
          ForceDirectories(FullFolder);
          ci := HD.BeastAnim[ID,Beast,I].Step[K]+1;
          if ci <> 0 then
            SpritePack.ExportFullImageData(FullFolder, ci, SList);
        end;
  end;

  HD.Free;
  fSprites.ClearTemp;
  SList.Free;
end;


//Export Trees graphics categorized by ID
procedure TKMResource.ExportTreeAnim;
var
  RXData: TRXData;
  FullFolder, Folder: string;
  I, K: Integer;

  SpriteID: Integer;
  SpritePack: TKMSpritePack;
  SList: TStringList;
begin
  fSprites.LoadSprites(rxTrees, False);
  SpritePack := fSprites[rxTrees];
  RXData := SpritePack.RXData;

  Folder := ExeDir + 'Export' + PathDelim + 'TreeAnim' + PathDelim;
  ForceDirectories(Folder);

  SList := TStringList.Create;

  for I := 0 to fMapElements.Count - 1 do
  if (gMapElements[I].Anim.Count > 0) and (gMapElements[I].Anim.Step[1] > 0) then
  begin
    for K := 1 to gMapElements[I].Anim.Count do
    begin
      SpriteID := gMapElements[I].Anim.Step[K] + 1;
      if SpriteID <> 0 then
      begin
        if gMapElements[I].Anim.Count > 1 then
        begin
          FullFolder := Folder + IntToStr(I) + PathDelim;
          ForceDirectories(FullFolder);
        end else
          FullFolder := Folder;
        SpritePack.ExportFullImageData(FullFolder, SpriteID, SList);
      end;
    end;
  end;

  fSprites.ClearTemp;
  SList.Free;
end;


end.
