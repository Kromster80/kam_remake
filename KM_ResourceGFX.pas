unit KM_ResourceGFX;
{$I KaM_Remake.inc}
interface
uses
  {$IFDEF WDC} PNGImage, {$ENDIF}
  {$IFDEF MSWindows} Windows, {$ENDIF}
  {$IFDEF Unix} LCLIntf, LCLType, {$ENDIF}
  Classes, Forms, Graphics, Math, SysUtils,
  KM_CommonTypes, KM_Defaults,
  KM_ResourceCursors,
  KM_ResourceFonts,
  KM_ResourceHouse,
  KM_ResourcePalettes,
  KM_ResourceResource,
  KM_ResourceTileset,
  KM_ResourceUnit
  {$IFDEF WDC}, ZLibEx {$ENDIF}
  {$IFDEF FPC}, BGRABitmap {$ENDIF};


type
  TDataLoadingState = (dls_None, dls_Menu, dls_All); //Resources are loaded in 2 steps, for menu and the rest

  TRXType = (
    rxTrees,
    rxHouses,
    rxUnits,
    rxGui,
    rxGuiMain,
    rxGuiMainH,
    rxMenu, //Remake menu elements
    rxTiles, //Tiles
    rxGame); //Remake game sprites

  TResource = class
  private
    fDataState: TDataLoadingState;
    fCursors: TKMCursors;
    fResourceFont: TResourceFont;
    fHouseDat: TKMHouseDatCollection;
    fUnitDat: TKMUnitDatCollection;
    fPalettes: TKMPalettes;
    fResources: TKMResourceCollection;
    fTileset: TKMTileset;

    procedure StepRefresh;
    procedure StepCaption(const aCaption: string);

    function LoadMapElemDAT(const FileName: string): Boolean;
    function LoadPatternDAT(const FileName: string): Boolean;

    procedure AllocateRX(aRT: TRXType; Count: Integer = 0);
    function LoadRX(aRT: TRXType): Boolean;
    procedure OverloadRX(aRT: TRXType);
    procedure ExpandRX(aRT: TRXType);
    procedure MakeGFX(aRT: TRXType);
    procedure MakeGFX_AlphaTest(aRT: TRXType);
    procedure ClearUnusedGFX(aRT: TRXType);
  public
    OnLoadingStep: TNotifyEvent;
    OnLoadingText: TStringEvent;

    constructor Create(aLS: TNotifyEvent; aLT: TStringEvent);
    destructor Destroy; override;
    procedure LoadMenuResources(const aLocale: string);
    procedure LoadGameResources;

    property DataState: TDataLoadingState read fDataState;
    property Cursors: TKMCursors read fCursors;
    property HouseDat: TKMHouseDatCollection read fHouseDat;
    property UnitDat: TKMUnitDatCollection read fUnitDat;
    property Palettes: TKMPalettes read fPalettes;
    property ResourceFont: TResourceFont read fResourceFont;
    property Resources: TKMResourceCollection read fResources;
    property Tileset: TKMTileset read fTileset;

    //procedure ExportRX2BMP(RXid:integer);
    //procedure ExportTreeAnim2BMP;
    //procedure ExportHouseAnim2BMP;
    //procedure ExportUnitAnim2BMP;
  end;

  var
    fResource: TResource;

    //todo: Switch from 1..9 to TRXType
  RXData: array [1..9]of record
    Qty: Integer;
    Flag: array of Byte;
    Size: array of record X,Y: Word; end;
    Pivot: array of record x,y: Integer; end;
    Data: array of array of Byte;
    RGBA: array of array of Cardinal; //Expanded image
    Mask: array of array of Cardinal; //Mask for team colors
    HasMask: array of Boolean; //Mask for team colors
    HasTeamColors: Boolean;
  end;

  GFXData: array [1..9] of array of record
    TexID,AltID: Cardinal; //AltID used for team colors
    u1,v1,u2,v2: Single;
    PxWidth,PxHeight: Word;
  end;

    procedure ExportRX2BMP(RXid:integer);
    procedure ExportTreeAnim2BMP;
    procedure ExportHouseAnim2BMP;
    procedure ExportUnitAnim2BMP;


implementation
uses KromUtils, KM_Render, KM_Log;


type
  TRXLocation = (rlFile, rlFolder); //Location of sprites, RXfiles or folder with pngs
  TRXUsage = (ruMenu, ruGame); //Where sprites are used

  TRXInfo = record
    Title: AnsiString; //Used for logging and filenames
    TeamColors: Boolean; //sprites should be generated with color masks
    AlphaTest: Boolean; //Alphatested gradients, used only for houses yet
    LoadFrom: TRXLocation;
    Usage: TRXUsage; //Menu and Game sprites are loaded separately
    OverrideCount: Word;
  end;


var
  //This is internal detail, that noone should know or care outside of this unit
  RXInfo: array [TRXType] of TRXInfo = (
    (
      Title: 'Trees';
      TeamColors: False;
      AlphaTest: False;
      LoadFrom: rlFile;
      Usage: ruGame;
      OverrideCount: 0
    ),
    (
      Title: 'Houses';
      TeamColors: True;
      AlphaTest: True;
      LoadFrom: rlFile;
      Usage: ruGame;
      OverrideCount: 0
    ),
    (
      Title: 'Units';
      TeamColors: True;
      AlphaTest: False;
      LoadFrom: rlFile;
      Usage: ruGame;
      OverrideCount: 7885 //Clip to 7885 sprites until we add TPR ballista/catapult support
    ),
    (
      Title: 'GUI';
      TeamColors: True;
      AlphaTest: False;
      LoadFrom: rlFile;
      Usage: ruMenu;
      OverrideCount: 0 //Required for unit scrolls and icons
    ),
    (
      Title: 'GUIMain';
      TeamColors: False;
      AlphaTest: False;
      LoadFrom: rlFile;
      Usage: ruMenu;
      OverrideCount: 0
    ),
    (
      Title: 'GUIMainH';
      TeamColors: False;
      AlphaTest: False;
      LoadFrom: rlFile;
      Usage: ruMenu;
      OverrideCount: 0
    ),
    (
      Title: 'RemakeMenu';
      TeamColors: False;
      AlphaTest: False;
      LoadFrom: rlFolder;
      Usage: ruMenu;
      OverrideCount: 17
    ),
    (
      Title: 'Tileset';
      TeamColors: False;
      AlphaTest: False;
      LoadFrom: rlFolder;
      Usage: ruGame;
      OverrideCount: 256
    ),
    (
      Title: 'RemakeGame';
      TeamColors: True;
      AlphaTest: False;
      LoadFrom: rlFolder;
      Usage: ruGame;
      OverrideCount: 300
    ));


{ TResource }
constructor TResource.Create(aLS: TNotifyEvent; aLT: TStringEvent);
begin
  Inherited Create;
  fDataState := dls_None;
  fLog.AppendLog('Resource loading state - None');

  OnLoadingStep := aLS;
  OnLoadingText := aLT;
end;


destructor TResource.Destroy;
begin
  if fHouseDat <> nil then FreeAndNil(fHouseDat);
  if fUnitDat <> nil then FreeAndNil(fUnitDat);
  if fPalettes <> nil then FreeAndNil(fPalettes);
  if fResourceFont <> nil then FreeAndNil(fResourceFont);
  if fResources <> nil then FreeAndNil(fResources);
  if fTileset <> nil then FreeAndNil(fTileset);
  if fCursors <> nil then FreeAndNil(fCursors);
  Inherited;
end;


procedure TResource.StepRefresh;
begin
  if Assigned(OnLoadingStep) then OnLoadingStep(Self);
end;


procedure TResource.StepCaption(const aCaption: string);
begin
  if Assigned(OnLoadingText) then OnLoadingText(aCaption);
end;


procedure TResource.LoadMenuResources(const aLocale: string);
var RT: TRXType;
begin
  Assert(fRender <> nil, 'fRender should be init before ReadGFX to be able access OpenGL');

  StepCaption('Reading palettes ...');
  fPalettes := TKMPalettes.Create;
  fPalettes.LoadPalettes;
  fLog.AppendLog('Reading palettes', True);

  for RT := Low(TRXType) to High(TRXType) do
  if (RXInfo[RT].Usage = ruMenu) and (RXInfo[RT].LoadFrom = rlFile) then
  begin
    StepCaption('Reading ' + RXInfo[RT].Title + ' ...');
    LoadRX(RT);
    OverloadRX(RT); //Load RX data overrides

    if RT = rxGui then
    begin
      fCursors := TKMCursors.Create;
      fCursors.MakeCursors;
      fCursors.Cursor := kmc_Default;
    end;

    MakeGFX(RT);
    ClearUnusedGFX(RT);

    StepRefresh;
  end;

  for RT := Low(TRXType) to High(TRXType) do
  if (RXInfo[RT].Usage = ruMenu) and (RXInfo[RT].LoadFrom = rlFolder) then
  begin
    StepCaption('Reading ' + RXInfo[RT].Title + ' ...');

    AllocateRX(RT, RXInfo[RT].OverrideCount);
    OverloadRX(RT); //Load sprites from PNGs
    MakeGFX(RT);
    ClearUnusedGFX(RT);
  end;

  StepCaption('Reading fonts ...');
  fResourceFont := TResourceFont.Create;
  fResourceFont.LoadFonts(aLocale);
  fLog.AppendLog('Read fonts is done');

  StepRefresh;
  fLog.AppendLog('ReadGFX is done');
  fDataState := dls_Menu;
  fLog.AppendLog('Resource loading state - Menu');
end;


procedure TResource.LoadGameResources;
var i:integer; RT: TRXType;
begin
  if fDataState = dls_All then Exit;

  Assert(fRender <> nil, 'fRender inits OpenGL and we need OpenGL to make textures');

  LoadMapElemDAT(ExeDir + 'data\defines\mapelem.dat');
  LoadPatternDAT(ExeDir + 'data\defines\pattern.dat');

  fResources := TKMResourceCollection.Create;
  fHouseDat := TKMHouseDatCollection.Create;
  fUnitDat := TKMUnitDatCollection.Create;

  for RT := Low(TRXType) to High(TRXType) do
  if (RXInfo[RT].Usage = ruGame) and (RXInfo[RT].LoadFrom = rlFile) then
  begin
    //Skip loading for performance reasons during debug
    if ((RT = rxHouses) and not MAKE_HOUSE_SPRITES) or
       ((RT = rxUnits) and not MAKE_UNIT_SPRITES) then Continue;

    StepCaption(RXInfo[RT].Title);

    fLog.AppendLog('Reading ' + RXInfo[RT].Title + '.rx');
    if LoadRX(RT) then
    begin
      OverloadRX(RT); //Updated sprites
      MakeGFX(RT);

      //Alpha_tested sprites for houses. They come after MakeGFX cos they will replace above data
      if RXInfo[RT].AlphaTest then
        MakeGFX_AlphaTest(RT);

      ClearUnusedGFX(RT);
    end;
  end;

  fLog.AppendLog('Reading folder');
  for RT := Low(TRXType) to High(TRXType) do
  if (RXInfo[RT].Usage = ruGame) and (RXInfo[RT].LoadFrom = rlFolder) then
  begin
    StepCaption(RXInfo[RT].Title);
    AllocateRX(RT, RXInfo[RT].OverrideCount);
    OverloadRX(RT); //Load custom PNGs
    MakeGFX(RT);
    ClearUnusedGFX(RT);
  end;

  fLog.AppendLog('Preparing MiniMap colors...');
  StepCaption(RXInfo[rxTiles].Title);
  fTileset := TKMTileset.Create(ExeDir + 'Resource\');
  AllocateRX(rxTiles, RXInfo[rxTiles].OverrideCount);
  //Generate UV coords
  for i:=0 to 255 do
  with GFXData[8, i+1] do
  begin
    TexID := fTileset.TextT;
    v1 := (i div 16  ) / 16; //There are 16 tiles across the line
    u1 := (i mod 16  ) / 16;
    v2 := (i div 16+1) / 16;
    u2 := (i mod 16+1) / 16;
    PxWidth := 32;
    PxHeight := 32;
  end;

  fDataState := dls_All;
  fLog.AppendLog('Resource loading state - Game');
end;


//Reading map elements (has animation data)
function TResource.LoadMapElemDAT(const FileName: string): Boolean;
var ii,kk:integer; ft:textfile; f:file;
begin
  Result:=false;
  if not CheckFileExists(FileName) then exit;
  assignfile(f,FileName); reset(f,1);
  blockread(f,MapElem[1],MapElemQty*99);
  closefile(f);

  ActualMapElemQty:=0;
  for ii:=1 to MapElemQty do
  if (MapElem[ii].Count>0)and(MapElem[ii].Step[1]>0) then
  begin
    inc(ActualMapElemQty);
    ActualMapElem[ActualMapElemQty] := ii; //pointer
    OriginalMapElem[ii] := ActualMapElemQty; //Reverse lookup
  end;

  if WriteResourceInfoToTXT then begin
    assignfile(ft,ExeDir+'Trees.txt'); rewrite(ft);
    for ii:=1 to MapElemQty do begin
    writeln(ft);
    writeln(ft,inttostr(ii)+' :'+inttostr(MapElem[ii].Count));
      for kk:=1 to 30 do if MapElem[ii].Step[kk]>0 then
      write(ft,MapElem[ii].Step[kk],'.') else write(ft,'_.');

      writeln(ft);
//      for kk:=1 to 16 do
//      write(ft,MapElem[ii].CuttableTree,''); //Those are 1/0 so we can ommit space between them

      write(ft,' =',MapElem[ii].CanBeRemoved);
      writeln(ft);
    end;
    closefile(ft);
  end;

  Result:=true;
end;


//Reading pattern data (tile info)
function TResource.LoadPatternDAT(const FileName: string): Boolean;
var ii,kk:integer; ft:textfile; f:file; s:byte;
begin
  Result:=false;
  if not CheckFileExists(FileName) then exit;
  assignfile(f,FileName); reset(f,1);
  blockread(f,PatternDAT[1],6*256);
  for ii:=1 to 30 do begin
    blockread(f,TileTable[ii,1],30*10);
    blockread(f,s,1);
    if s<>0 then
      s:=s;
  end;

  closefile(f);

  if WriteResourceInfoToTXT then begin
    assignfile(ft,ExeDir+'Pattern.csv');
    rewrite(ft);
    writeln(ft,'PatternDAT');
    for ii:=0 to 15 do begin
      for kk:=1 to 16 do
        write(ft,inttostr(ii*16+kk),' ',PatternDAT[ii*16+kk].TileType,';');
      writeln(ft);
    end;
    writeln(ft,'TileTable');
    for ii:=1 to 30 do begin
      for kk:=1 to 30 do begin
      write(ft,inttostr(TileTable[ii,kk].Tile1)+'_'+inttostr(TileTable[ii,kk].Tile2)+'_'+inttostr(TileTable[ii,kk].Tile3)+' ');
      write(ft,inttostr(byte(TileTable[ii,kk].b1)));
      write(ft,inttostr(byte(TileTable[ii,kk].b2)));
      write(ft,inttostr(byte(TileTable[ii,kk].b3)));
      write(ft,inttostr(byte(TileTable[ii,kk].b4)));
      write(ft,inttostr(byte(TileTable[ii,kk].b5)));
      write(ft,inttostr(byte(TileTable[ii,kk].b6)));
      write(ft,inttostr(byte(TileTable[ii,kk].b7)));
      write(ft,';');
      end;

      writeln(ft);
    end;
    closefile(ft);
  end;

  Result:=true;
end;


{ This function should parse all valid files in Sprites folder and load them
  additionaly to or replacing original sprites }
procedure TResource.OverloadRX(aRT: TRXType);
var
  FileList:TStringList;
  SearchRec:TSearchRec;
  i:integer; x,y:integer;
  RX,ID: integer; p:cardinal;
  T:Byte;
  ft:TextFile;
  {$IFDEF WDC}
  po:TPNGObject;
  {$ENDIF}
  {$IFDEF FPC}
  po:TBGRABitmap;
  {$ENDIF}
begin
  if not DirectoryExists(ExeDir + 'Sprites\') then Exit;

  RX := Byte(aRT) + 1;

  FileList := TStringList.Create;
  FindFirst(ExeDir + 'Sprites\' + inttostr(RX)+'_????.png', faAnyFile AND NOT faDirectory, SearchRec);
  repeat
    FileList.Add(SearchRec.Name);
  until (FindNext(SearchRec)<>0);
  FindClose(SearchRec);

  //#_####.png - Default texture
  //#_####a.png - Flag colors areas
  //#_####.txt - Pivot info

  for i:=0 to FileList.Count-1 do begin

    ID := StrToIntDef(Copy(FileList.Strings[i], 3, 4),0); //wrong file will return 0
    if InRange(ID, 1, RXData[RX].Qty) then begin //Replace only certain sprites

      RXData[RX].HasMask[ID] := FileExists(ExeDir + 'Sprites\' + Copy(FileList.Strings[i], 1, 6)+'a.png');

      {$IFDEF WDC}
      po := TPNGObject.Create;
      po.LoadFromFile(ExeDir + 'Sprites\' + FileList.Strings[i]);
      {$ENDIF}
      {$IFDEF FPC}
      po := TBGRABitmap.Create(ExeDir + 'Sprites\' + FileList.Strings[i]);
      {$ENDIF}

      RXData[RX].Size[ID].X := po.Width;
      RXData[RX].Size[ID].Y := po.Height;

      SetLength(RXData[RX].RGBA[ID], po.Width*po.Height);
      SetLength(RXData[RX].Mask[ID], po.Width*po.Height); //Should allocate space for it's always comes along

      {$IFDEF WDC}
      case po.TransparencyMode of //There are ways to process PNG transparency
        ptmNone:
          for y:=0 to po.Height-1 do for x:=0 to po.Width-1 do
            RXData[RX].RGBA[ID, y*po.Width+x] := cardinal(po.Pixels[x,y]) OR $FF000000;
        ptmBit:
          for y:=0 to po.Height-1 do for x:=0 to po.Width-1 do
            if po.Pixels[x,y] = po.TransparentColor then
              RXData[RX].RGBA[ID, y*po.Width+x] := cardinal(po.Pixels[x,y]) AND $FFFFFF //avoid black edging
            else
              RXData[RX].RGBA[ID, y*po.Width+x] := cardinal(po.Pixels[x,y]) OR $FF000000;
        ptmPartial:
          for y:=0 to po.Height-1 do for x:=0 to po.Width-1 do begin
            p := (po.AlphaScanline[y]^[x]) shl 24; //semitransparency is killed by render later-on
            RXData[RX].RGBA[ID, y*po.Width+x] := cardinal(po.Pixels[x,y]) OR p;
          end;
        else Assert(false, 'Unknown PNG transparency mode')
      end;
      {$ENDIF}
      {$IFDEF FPC}
      for y:=0 to po.Height-1 do for x:=0 to po.Width-1 do
        RXData[RX].RGBA[ID, y*po.Width+x] := cardinal(po.GetPixel(x,y).red) OR (cardinal(po.GetPixel(x,y).green) shl 8) OR
                                            (cardinal(po.GetPixel(x,y).blue) shl 16) OR (cardinal(po.GetPixel(x,y).alpha) shl 24);
      {$ENDIF}
      po.Free;

      //Load and process the mask if it exists
      if RXData[RX].HasMask[ID] then
      begin
        {$IFDEF WDC}
        po := TPNGObject.Create;
        po.LoadFromFile(ExeDir + 'Sprites\' + StringReplace(FileList.Strings[i], '.png', 'a.png', [rfReplaceAll, rfIgnoreCase]));
        {$ENDIF}
        {$IFDEF FPC}
        po := TBGRABitmap.Create(ExeDir + 'Sprites\' + StringReplace(FileList.Strings[i], '.png', 'a.png', [rfReplaceAll, rfIgnoreCase]));
        {$ENDIF}

        if (RXData[RX].Size[ID].X = po.Width) and (RXData[RX].Size[ID].Y = po.Height) then
        begin
          //We don't handle transparency in Masks
          {$IFDEF WDC}
          for y:=0 to po.Height-1 do for x:=0 to po.Width-1 do
          if cardinal(po.Pixels[x,y] AND $FF) <> 0 then
          begin
            T := RXData[RX].RGBA[ID, y*po.Width+x] AND $FF; //Take red component
            RXData[RX].Mask[ID, y*po.Width+x] := Byte(255-Abs(255-T*2)) SHL 24 OR $FFFFFF;
            RXData[RX].RGBA[ID, y*po.Width+x] := T*65793 OR $FF000000;
          end;
          {$ENDIF}
          {$IFDEF FPC}
          for y:=0 to po.Height-1 do for x:=0 to po.Width-1 do
          if cardinal(po.GetPixel(x,y).red) <> 0 then
          begin
            T := RXData[RX].RGBA[ID, y*po.Width+x] AND $FF; //Take red component
            RXData[RX].Mask[ID, y*po.Width+x] := Byte(255-Abs(255-T*2)) SHL 24 OR $FFFFFF;
            RXData[RX].RGBA[ID, y*po.Width+x] := T*65793 OR $FF000000;
          end;
          {$ENDIF}
        end;
        po.Free;
      end;


      //Read pivots
      if FileExists(ExeDir + 'Sprites\' + Copy(FileList.Strings[i], 1, 6)+'.txt') then begin
        AssignFile(ft, ExeDir + 'Sprites\' + Copy(FileList.Strings[i], 1, 6)+'.txt');
        Reset(ft);
        ReadLn(ft, RXData[RX].Pivot[ID].X);
        ReadLn(ft, RXData[RX].Pivot[ID].Y);
        CloseFile(ft);
      end;

    end;
  end;

  FileList.Free;
end;


procedure TResource.AllocateRX(aRT: TRXType; Count: Integer = 0);
var ID: Byte;
begin
  ID := Byte(aRT) + 1;

  if Count>0 then
    RXData[ID].Qty := Count;

  Count := RXData[ID].Qty+1;
  SetLength(GFXData[ID],        Count);
  SetLength(RXData[ID].Flag,    Count);
  SetLength(RXData[ID].Size,    Count);
  SetLength(RXData[ID].Pivot,   Count);
  SetLength(RXData[ID].Data,    Count);
  SetLength(RXData[ID].RGBA,    Count);
  SetLength(RXData[ID].Mask,    Count);
  SetLength(RXData[ID].HasMask, Count);
end;


//Reading RX Data
function TResource.LoadRX(aRT: TRXType): Boolean;
var
  i, ID: Integer;
  FileName: String;
  f: file;
begin
  Result := False;

  FileName := ExeDir + 'data\gfx\res\' + RXInfo[aRT].Title + '.rx';

  if not CheckFileExists(FileName) then
    Exit;

  ID := Byte(aRT) + 1; //quick hack

  assignfile(f, FileName);
  reset(f, 1);
  blockread(f, RXData[ID].Qty, 4);

  AllocateRX(aRT);

  blockread(f, RXData[ID].Flag[1], RXData[ID].Qty);

  //Don't load the extra sprites, but keep them allocated
  //to avoid range check errors when game wants to use that sprite
  if (RXInfo[aRT].OverrideCount <> 0) then
    RXData[ID].Qty := RXInfo[aRT].OverrideCount;

  for i := 1 to RXData[ID].Qty do
    if RXData[ID].Flag[i] = 1 then
    begin
      blockread(f, RXData[ID].Size[i].X, 4);
      blockread(f, RXData[ID].Pivot[i].X, 8);
      SetLength(RXData[ID].Data[i], RXData[ID].Size[i].X * RXData[ID].Size[i].Y);
      blockread(f, RXData[ID].Data[i, 0], RXData[ID].Size[i].X * RXData[ID].Size[i].Y);
    end;
  closefile(f);
  fLog.AppendLog(RXInfo[aRT].Title + ' -', RXData[ID].Qty);

  RXData[ID].HasTeamColors := RXInfo[aRT].TeamColors;

  ExpandRX(aRT);
  Result := True;
end;


//Convert paletted data into RGBA and select Team color layer from it
procedure TResource.ExpandRX(aRT: TRXType);
var
  i: Integer;
  X, Y: Integer;
  Palette: TKMPal;
  L: byte;
  Pixel: Integer;
  ID: Byte;
begin
  ID := Byte(aRT) + 1;

  with RXData[ID] do
  for i:=1 to Qty do
  begin

    case ID of
      5: Palette := RX5Pal[i];
      6: Palette := RX6Pal[i];
      else Palette := DEF_PAL;
    end;

    if Flag[i] = 1 then begin
      SetLength(RGBA[i], Size[i].X*Size[i].Y);
      SetLength(Mask[i], Size[i].X*Size[i].Y);

      for y:=0 to Size[i].Y-1 do for x:=0 to Size[i].X-1 do
      begin
        Pixel := y*Size[i].X + x;
        L := Data[i, Pixel]; //0..255

        if HasTeamColors and (L in [24..30])
        and ((ID<>2) or (i>400))  //Skip the Inn Weapon Smithy and the rest
        and ((ID<>4) or InRange(i,141,154) or InRange(i,521,550)) then //Unit icons and scrolls
        begin
          RGBA[i,Pixel] := cardinal(((L-27)*42+128)*65793) OR $FF000000;
          case L of //Maybe it makes sense to convert to 8bit?
            24,30:  Mask[i,Pixel] := $60FFFFFF;   //7  //6
            25,29:  Mask[i,Pixel] := $90FFFFFF;   //11 //9
            26,28:  Mask[i,Pixel] := $C0FFFFFF;   //14 //12
            27:     Mask[i,Pixel] := $FFFFFFFF;   //16 //16
          end;
          HasMask[i] := true;
        end else
          RGBA[i,Pixel] := fPalettes[Palette].Color32(L);
      end;
    end;
  end;
end;


{Take RX data and make nice textures out of it.
Textures should be POT to improve performance and avoid drivers bugs
In result we have GFXData filled.}
procedure TResource.MakeGFX_AlphaTest(aRT: TRXType);
var
  HT:THouseType;
  ID1,ID2:integer; //RGB and A index
  i,k,h,StepCount:integer;
  t,tx,ty:integer;
  Alpha:byte;
  WidthPOT,HeightPOT:integer;
  TD:array of cardinal;
  ID: Byte;
begin
  ID := Byte(aRT) + 1;

  for HT:=Low(THouseType) to High(THouseType) do
    if HouseDat[HT].IsValid then

      for h:=1 to 2 do begin
        if h=1 then begin
          ID1 := HouseDat[HT].WoodPic+1;
          ID2 := HouseDat[HT].WoodPal+1;
          StepCount := HouseDat[HT].WoodPicSteps;
        end else begin
          ID1 := HouseDat[HT].StonePic+1;
          ID2 := HouseDat[HT].StonePal+1;
          StepCount := HouseDat[HT].StonePicSteps;
        end;

        Assert(
            (RXData[ID].Size[ID1].X >= RXData[ID].Size[ID2].X) and
            (RXData[ID].Size[ID1].Y >= RXData[ID].Size[ID2].Y),
            Format('Mismatched sprites %d:%d - %d:%d', [ID, ID1, ID, ID2]));

        WidthPOT  := MakePOT(RXData[ID].Size[ID1].X);
        HeightPOT := MakePOT(RXData[ID].Size[ID1].Y);
        SetLength(TD, WidthPOT*HeightPOT);

        //Fill in colors data
        for i := 0 to RXData[ID].Size[ID1].Y-1 do
        for k := 0 to RXData[ID].Size[ID1].X-1 do
          TD[i*WidthPOT+k] := RXData[ID].RGBA[ID1, i*RXData[ID].Size[ID1].X+k];

        //Apply mask to where colors are (yes, it needs to be done in 2 steps, since offsets can mismatch)
        tx := RXData[ID].Pivot[ID2].x - RXData[ID].Pivot[ID1].x;
        ty := (RXData[ID].Pivot[ID2].y - RXData[ID].Pivot[ID1].y)*WidthPOT;
        for i := 0 to RXData[ID].Size[ID2].Y-1 do
        for k := 0 to RXData[ID].Size[ID2].X-1 do
        begin
          t := i*WidthPOT+k + tx + ty; //Shift by pivot, always positive

          //Flag 1 means that we can use Data array
          //Otherwise, for addon sprites, we need to resort to RGBA data they provide
          if RXData[ID].Flag[ID2] = 1 then
            Alpha := RXData[ID].Data[ID2,i*RXData[ID].Size[ID2].X+k]
          else
            Alpha := RXData[ID].RGBA[ID2,i*RXData[ID].Size[ID2].X+k] AND $FF;

          //Steps are going in normal order 1..n, but that last step has Alpha=0
          if TD[t] <> 0 then
            if Alpha <> 0 then //Default case
              TD[t] := TD[t] AND ($FFFFFF OR (255-round(Alpha*(255/StepCount))) shl 24)
            else
              TD[t] := TD[t] AND $01FFFFFF; //Place it as last step
        end;

        GFXData[ID,ID1].TexID := fRender.GenTexture(WidthPOT,HeightPOT,@TD[0],tf_AlphaTest);
        SetLength(TD, 0);
        GFXData[ID,ID1].AltID := 0;
        GFXData[ID,ID1].u1    := 0;
        GFXData[ID,ID1].v1    := 0;
        GFXData[ID,ID1].u2    := RXData[ID].Size[ID1].X/WidthPOT;
        GFXData[ID,ID1].v2    := RXData[ID].Size[ID1].Y/HeightPOT;
        GFXData[ID,ID1].PxWidth := RXData[ID].Size[ID1].X;
        GFXData[ID,ID1].PxHeight:= RXData[ID].Size[ID1].Y;
      end;
end;


{Take RX data and make nice textures out of it.
Textures should be POT to improve performance and avoid driver bugs
In result we have GFXData filled.}
procedure TResource.MakeGFX(aRT: TRXType);
var
  ci,j,i,k,LeftIndex,RightIndex,TexCount,SpanCount:integer;
  AllocatedRAM,RequiredRAM,ColorsRAM:cardinal;
  WidthPOT,HeightPOT:word;
  TD:array of cardinal;
  TA:array of cardinal;
  HasMsk:boolean;
  ID: Byte;
begin
  ID := Byte(aRT) + 1;

  LeftIndex:=0; AllocatedRAM:=0; RequiredRAM:=0; ColorsRAM:=0; TexCount:=0;
  repeat
    inc(LeftIndex);

    WidthPOT  := RXData[ID].Size[LeftIndex].X;
    HeightPOT := MakePOT(RXData[ID].Size[LeftIndex].Y);
    SpanCount := 1;

    //Pack textures with same POT height into rows to save memory
    //This also means fewer textures for GPU RAM == better performance
    while((LeftIndex+SpanCount<RXData[ID].Qty)and //Keep packing until end of sprites
          (
            (HeightPOT=MakePOT(RXData[ID].Size[LeftIndex+SpanCount].Y)) //Pack if HeightPOT matches
        or((HeightPOT>=MakePOT(RXData[ID].Size[LeftIndex+SpanCount].Y))AND(WidthPOT+RXData[ID].Size[LeftIndex+SpanCount].X<MakePOT(WidthPOT)))
          )and
          (WidthPOT+RXData[ID].Size[LeftIndex+SpanCount].X<=MAX_TEX_RESOLUTION)) //Pack until max Tex_Resolution approached
    do begin
      inc(WidthPOT,RXData[ID].Size[LeftIndex+SpanCount].X);
      if (ID=5)and(RX5Pal[LeftIndex]<>RX5Pal[LeftIndex+SpanCount]) then break; //Don't align RX5 images for they use all different palettes
      if (ID=6)and(RX6Pal[LeftIndex]<>RX6Pal[LeftIndex+SpanCount]) then break; //Don't align RX6 images for they use all different palettes
      inc(SpanCount);
    end;

    RightIndex := LeftIndex+SpanCount-1;
    WidthPOT := MakePOT(WidthPOT);
    SetLength(TD,WidthPOT*HeightPOT+1);
    SetLength(TA,WidthPOT*HeightPOT+1);

    for i:=0 to HeightPOT-1 do begin
      ci:=0;
      for j:=LeftIndex to RightIndex do
        for k:=0 to RXData[ID].Size[j].X-1 do begin
          if i<RXData[ID].Size[j].Y then begin
            //CopyMemory(TD[(i-1)*WidthPOT+ci-1], RXData[ID].RGBA[j,(i-1)*RXData[ID].Size[j].X+k-1], )
            TD[i*WidthPOT+ci] := RXData[ID].RGBA[j,i*RXData[ID].Size[j].X+k];
            TA[i*WidthPOT+ci] := RXData[ID].Mask[j,i*RXData[ID].Size[j].X+k];
          end;
          inc(ci);
        end;
    end;

    HasMsk:=false;
    for j:=LeftIndex to RightIndex do
      HasMsk := HasMsk or RXData[ID].HasMask[j];

    //If we need to prepare textures for TeamColors          //special fix for iron mine logo
    if MAKE_TEAM_COLORS and RXData[ID].HasTeamColors and (not ((ID=4)and InRange(49,LeftIndex,RightIndex))) then
    begin
      GFXData[ID,LeftIndex].TexID := fRender.GenTexture(WidthPOT,HeightPOT,@TD[0],tf_Normal);
      //TeamColors are done through alternative plain colored texture
      if HasMsk then begin
        GFXData[ID,LeftIndex].AltID := fRender.GenTexture(WidthPOT,HeightPOT,@TA[0],tf_AltID);
        inc(ColorsRAM,WidthPOT*HeightPOT*4);
      end;
    end
    else
      GFXData[ID,LeftIndex].TexID := fRender.GenTexture(WidthPOT,HeightPOT,@TD[0],tf_Normal);

    SetLength(TD,0);
    SetLength(TA,0);

    k:=0;
    for j:=LeftIndex to RightIndex do begin //Hack to test AlphaTest
      GFXData[ID,j].TexID:=GFXData[ID,LeftIndex].TexID;
      GFXData[ID,j].AltID:=GFXData[ID,LeftIndex].AltID;
      GFXData[ID,j].u1:=k/WidthPOT;
      GFXData[ID,j].v1:=0;
      inc(k,RXData[ID].Size[j].X);
      GFXData[ID,j].u2:=k/WidthPOT;
      GFXData[ID,j].v2:=RXData[ID].Size[j].Y/HeightPOT;
      GFXData[ID,j].PxWidth:=RXData[ID].Size[j].X;
      GFXData[ID,j].PxHeight:=RXData[ID].Size[j].Y;

      inc(RequiredRAM,RXData[ID].Size[j].X*RXData[ID].Size[j].Y*4);
    end;

    inc(AllocatedRAM,WidthPOT*HeightPOT*4);
    inc(LeftIndex,SpanCount-1);
    inc(TexCount);

  until(LeftIndex>=RXData[ID].Qty); // >= in case data wasn't loaded and Qty=0

  fLog.AppendLog(inttostr(TexCount)+' Textures created');
  fLog.AddToLog(inttostr(AllocatedRAM div 1024)+'/'+inttostr((AllocatedRAM-RequiredRAM) div 1024)+' Kbytes allocated/wasted for units GFX when using Packing');
  fLog.AddToLog(inttostr(ColorsRAM div 1024)+' KBytes for team colors');
end;


// Now we can safely dispose of RXData[ID].Data to save us RAM
procedure TResource.ClearUnusedGFX(aRT: TRXType);
var
  i: Integer;
  ID: byte;
begin
  ID := byte(aRT) + 1;

  for i := 1 to RXData[ID].Qty do
  begin
    SetLength(RXData[ID].Data[i], 0);
    SetLength(RXData[ID].RGBA[i], 0);
    SetLength(RXData[ID].Mask[i], 0);
  end;
end;


//=============================================
//Export RX to Bitmaps
//=============================================
{That is when we want to export RX to Bitmaps without need to have GraphicsEditor, also this way we preserve image indexes}
procedure ExportRX2BMP(RXid:integer);
var MyBitMap:TBitmap;
    id,i,k:integer;
    sy,sx:integer;
    RT: TRXType;
begin
  RT := TRXType(RXid - 1);

  CreateDir(ExeDir + 'Export\');
  CreateDir(ExeDir + 'Export\' + RXInfo[RT].Title + '.rx\');
  MyBitMap := TBitmap.Create;
  MyBitMap.PixelFormat := pf24bit;

  if not fResource.LoadRX(RT) then Exit;

  for id:=1 to RXData[RXid].Qty do
  begin
    sx := RXData[RXid].Size[id].X;
    sy := RXData[RXid].Size[id].Y;
    MyBitMap.Width  := sx;
    MyBitMap.Height := sy;

    for i:=0 to sy-1 do for k:=0 to sx-1 do
      MyBitMap.Canvas.Pixels[k,i] := RXData[RXid].RGBA[id,i*sx+k] AND $FFFFFF; //Drop Alpha value

    //Mark pivot location with a dot
    k := sx + RXData[RXid].Pivot[id].x;
    i := sy + RXData[RXid].Pivot[id].y;
    if InRange(i, 0, sy-1) and InRange(k, 0, sx-1) then
      MyBitMap.Canvas.Pixels[k,i] := $FF00FF;

    if sy > 0 then
      MyBitMap.SaveToFile(ExeDir + 'Export\' + RXInfo[RT].Title + '.rx\' + RXInfo[RT].Title + '_' + int2fix(ID, 4) + '.bmp');

    SetLength(RXData[RXid].Data[id], 0);
  end;

  MyBitMap.Free;
end;

{Export Units graphics categorized by Unit and Action}
procedure ExportUnitAnim2BMP;
var MyBitMap:TBitmap;
    //U:TUnitType;
    //iAct,iDir,iFrame,ci:integer;
    //sy,sx,y,x:integer;
    //Used:array of integer;
begin
  CreateDir(ExeDir+'Export\');
  CreateDir(ExeDir+'Export\UnitAnim\');
  MyBitMap:=TBitmap.Create;
  MyBitMap.PixelFormat:=pf24bit;

  if not fResource.LoadRX(rxUnits) then Exit;

  {ci:=0;
  for U:=ut_Militia to ut_Militia do begin
    for iAct:=1 to 14 do begin
      for iDir:=1 to 8 do if UnitDat[U].UnitSprite[iUnit].Act[iAct].Dir[iDir].Step[1]<>-1 then begin
        for iFrame:=1 to UnitSprite[iUnit].Act[iAct].Dir[iDir].Count do begin
          CreateDir(ExeDir+'Export\UnitAnim\'+TypeToString(iUnit)+'\');
          CreateDir(ExeDir+'Export\UnitAnim\'+TypeToString(iUnit)+'\'+UnitAct[iAct]+'\');
          if UnitSprite[iUnit].Act[iAct].Dir[iDir].Step[iFrame]+1<>0 then
            ci:=UnitSprite[iUnit].Act[iAct].Dir[iDir].Step[iFrame]+1;

          sx:=RXData[3].Size[ci].X;
          sy:=RXData[3].Size[ci].Y;
          MyBitMap.Width:=sx;
          MyBitMap.Height:=sy;

          for y:=0 to sy-1 do for x:=0 to sx-1 do
            MyBitMap.Canvas.Pixels[x,y] := RXData[3].RGBA[ci,y*sx+x] AND $FFFFFF;

          if sy>0 then MyBitMap.SaveToFile(
            ExeDir+'Export\UnitAnim\'+TypeToString(iUnit)+'\'+UnitAct[iAct]+'\'+inttostr(iDir)+'_'+int2fix(iFrame,2)+'.bmp');
        end;
      end;
    end;
  end;

  CreateDir(ExeDir+'Export\UnitAnim\_TheRest');
  SetLength(Used,length(RXData[3].Size));
  for iUnit:=1 to 41 do
  for iAct:=1 to 14 do
  for iDir:=1 to 8 do if UnitSprite[iUnit].Act[iAct].Dir[iDir].Step[1]<>-1 then
  for iFrame:=1 to UnitSprite[iUnit].Act[iAct].Dir[iDir].Count do
  if UnitSprite[iUnit].Act[iAct].Dir[iDir].Step[iFrame]+1<>0 then
  Used[UnitSprite[iUnit].Act[iAct].Dir[iDir].Step[iFrame]+1]:=1;

  for ci:=1 to length(Used)-1 do
  if Used[ci]=0 then begin
    sx:=RXData[3].Size[ci].X;
    sy:=RXData[3].Size[ci].Y;
    MyBitMap.Width:=sx;
    MyBitMap.Height:=sy;

    for y:=0 to sy-1 do for x:=0 to sx-1 do
      MyBitMap.Canvas.Pixels[x,y] := RXData[3].RGBA[ci,y*sx+x] AND $FFFFFF;

    if sy>0 then MyBitMap.SaveToFile(
      ExeDir+'Export\UnitAnim\_TheRest\'+'_'+int2fix(ci,4)+'.bmp');
  end;}

  MyBitMap.Free;
end;


{Export Houses graphics categorized by House and Action}
procedure ExportHouseAnim2BMP;
var MyBitMap:TBitmap;
    ID:THouseType;
    Ac:THouseActionType;
    Q,Beast,i,k,ci:integer;
    sy,sx,y,x:integer;
begin
  CreateDir(ExeDir+'Export\');
  CreateDir(ExeDir+'Export\HouseAnim\');
  MyBitMap:=TBitmap.Create;
  MyBitMap.PixelFormat:=pf24bit;

  fResource.LoadGameResources;
  if not fResource.LoadRX(rxHouses) then Exit;

  ci:=0;
  for ID:=Low(THouseType) to High(THouseType) do
    for Ac:=ha_Work1 to ha_Flag3 do
      for k:=1 to fResource.HouseDat[ID].Anim[Ac].Count do
      begin
        CreateDir(ExeDir+'Export\HouseAnim\'+fResource.HouseDat[ID].HouseName+'\');
        CreateDir(ExeDir+'Export\HouseAnim\'+fResource.HouseDat[ID].HouseName+'\'+HouseAction[Ac]+'\');
        if fResource.HouseDat[ID].Anim[Ac].Step[k] <> -1 then
          ci := fResource.HouseDat[ID].Anim[Ac].Step[k]+1;

        sx := RXData[2].Size[ci].X;
        sy := RXData[2].Size[ci].Y;
        MyBitMap.Width:=sx;
        MyBitMap.Height:=sy;

        for y:=0 to sy-1 do for x:=0 to sx-1 do
          MyBitMap.Canvas.Pixels[x,y] := RXData[2].RGBA[ci,y*sx+x] AND $FFFFFF;

        if sy>0 then MyBitMap.SaveToFile(
        ExeDir+'Export\HouseAnim\'+fResource.HouseDat[ID].HouseName+'\'+HouseAction[Ac]+'\_'+int2fix(k,2)+'.bmp');
      end;

  ci:=0;
  for Q:=1 to 2 do
  begin
    if Q=1 then ID:=ht_Swine
           else ID:=ht_Stables;
    CreateDir(ExeDir+'Export\HouseAnim\_'+fResource.HouseDat[ID].HouseName+'\');
    for Beast:=1 to 5 do
      for i:=1 to 3 do
        for k:=1 to fResource.HouseDat.BeastAnim[ID,Beast,i].Count do
        begin
          CreateDir(ExeDir+'Export\HouseAnim\_'+fResource.HouseDat[ID].HouseName+'\'+int2fix(Beast,2)+'\');
          if fResource.HouseDat.BeastAnim[ID,Beast,i].Step[k]+1<>0 then
            ci := fResource.HouseDat.BeastAnim[ID,Beast,i].Step[k]+1;

          sx:=RXData[2].Size[ci].X;
          sy:=RXData[2].Size[ci].Y;
          MyBitMap.Width:=sx;
          MyBitMap.Height:=sy;

          for y:=0 to sy-1 do for x:=0 to sx-1 do
            MyBitMap.Canvas.Pixels[x,y] := RXData[2].RGBA[ci,y*sx+x] AND $FFFFFF;

          if sy>0 then MyBitMap.SaveToFile(ExeDir+'Export\HouseAnim\_'+fResource.HouseDat[ID].HouseName+'\'+int2fix(Beast,2)+'\_'+int2fix(i,1)+'_'+int2fix(k,2)+'.bmp');
        end;
  end;

  MyBitMap.Free;
end;


{Export Trees graphics categorized by ID}
procedure ExportTreeAnim2BMP;
var MyBitMap: TBitmap;
    i,k,ci:integer;
    sy,sx,y,x:integer;
begin
  CreateDir(ExeDir + 'Export\');
  CreateDir(ExeDir + 'Export\TreeAnim\');
  MyBitMap := TBitmap.Create;
  MyBitMap.PixelFormat := pf24bit;

  fResource.LoadMapElemDAT(ExeDir + 'data\defines\mapelem.dat');
  if not fResource.LoadRX(rxTrees) then Exit;

  ci:=0;
  for i:=1 to MapElemQty do
    for k:=1 to MapElem[i].Count do
    begin
      if MapElem[i].Step[k]+1 <> 0 then
        ci := MapElem[i].Step[k]+1;

      sx := RXData[1].Size[ci].X;
      sy := RXData[1].Size[ci].Y;
      MyBitMap.Width := sx;
      MyBitMap.Height := sy;

      for y:=0 to sy-1 do for x:=0 to sx-1 do
        MyBitMap.Canvas.Pixels[x,y] := RXData[1].RGBA[ci,y*sx+x] AND $FFFFFF;

      //We can insert field here and press Export>TreeAnim. Rename each folder after export to 'Cuttable',
      //'Quad' and etc.. there you'll have it. Note, we use 1..254 counting, JBSnorro uses 0..253 counting
      if sy>0 then MyBitMap.SaveToFile(
        ExeDir+'Export\TreeAnim\'+{inttostr(word(MapElem[i].DiagonalBlocked))+'_'+}int2fix(i,3)+'_'+int2fix(k,2)+'.bmp');
    end;

  MyBitMap.Free;
end;


end.
