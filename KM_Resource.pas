unit KM_Resource;
{$I KaM_Remake.inc}
interface
uses
  {$IFDEF Unix} LCLIntf, LCLType, {$ENDIF}
  Classes, Forms, Graphics, SysUtils,
  KM_CommonEvents, KM_Defaults, KM_Pics,
  KM_Render,
  KM_ResourceCursors,
  KM_ResourceFonts,
  KM_ResourceHouse,
  KM_ResourcePalettes,
  KM_ResourceResource,
  KM_ResourceSprites,
  KM_ResourceTileset,
  KM_ResourceUnit
  {$IFDEF WDC}, ZLibEx {$ENDIF}
  {$IFDEF FPC}, BGRABitmap {$ENDIF};


type
  TDataLoadingState = (dls_None, dls_Menu, dls_All); //Resources are loaded in 2 steps, for menu and the rest


  TResource = class
  private
    fRender: TRender;
    fDataState: TDataLoadingState;
    fCursors: TKMCursors;
    fResourceFont: TResourceFont;
    fHouseDat: TKMHouseDatCollection;
    fUnitDat: TKMUnitDatCollection;
    fPalettes: TKMPalettes;
    fResources: TKMResourceCollection;
    fSprites: TKMSprites;
    fTileset: TKMTileset;

    procedure StepRefresh;
    procedure StepCaption(const aCaption: string);

    function LoadMapElemDAT(const FileName: string): Boolean;
    function LoadPatternDAT(const FileName: string): Boolean;
  public
    OnLoadingStep: TEvent;
    OnLoadingText: TStringEvent;

    constructor Create(aRender: TRender; aLS: TEvent; aLT: TStringEvent);
    destructor Destroy; override;

    procedure LoadMenuResources(const aLocale: AnsiString);
    procedure LoadGameResources(aAlphaShadows: boolean);

    property DataState: TDataLoadingState read fDataState;
    property Cursors: TKMCursors read fCursors;
    property HouseDat: TKMHouseDatCollection read fHouseDat;
    property UnitDat: TKMUnitDatCollection read fUnitDat;
    property Palettes: TKMPalettes read fPalettes;
    property ResourceFont: TResourceFont read fResourceFont;
    property Resources: TKMResourceCollection read fResources;
    property Sprites: TKMSprites read fSprites;
    property Tileset: TKMTileset read fTileset;

    procedure ExportTreeAnim;
    procedure ExportHouseAnim;
    procedure ExportUnitAnim;
  end;

  var
    fResource: TResource;

implementation
uses KromUtils, KM_Log, KM_Points;


{ TResource }
constructor TResource.Create(aRender: TRender; aLS: TEvent; aLT: TStringEvent);
begin
  Inherited Create;
  fDataState := dls_None;
  fLog.AppendLog('Resource loading state - None');

  fRender := aRender;
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
  if fSprites <> nil then FreeAndNil(fSprites);
  if fTileset <> nil then FreeAndNil(fTileset);
  if fCursors <> nil then FreeAndNil(fCursors);
  Inherited;
end;


procedure TResource.StepRefresh;
begin
  if Assigned(OnLoadingStep) then OnLoadingStep;
end;


procedure TResource.StepCaption(const aCaption: string);
begin
  if Assigned(OnLoadingText) then OnLoadingText(aCaption);
end;


procedure TResource.LoadMenuResources(const aLocale: AnsiString);
begin
  Assert(fRender <> nil, 'fRenderSetup should be init before ReadGFX to be able access OpenGL');

  StepCaption('Reading palettes ...');
  fPalettes := TKMPalettes.Create;
  fPalettes.LoadPalettes;
  fLog.AppendLog('Reading palettes', True);

  fSprites := TKMSprites.Create(fRender, fPalettes, StepRefresh, StepCaption);

  fCursors := TKMCursors.Create;
  fSprites.LoadMenuResources;
  fCursors.MakeCursors(fSprites[rxGui]);
  fCursors.Cursor := kmc_Default;
  fSprites.ClearTemp;

  StepCaption('Reading fonts ...');
  fResourceFont := TResourceFont.Create(fRender);
  fResourceFont.LoadFonts(aLocale);
  fLog.AppendLog('Read fonts is done');

    fTileset := TKMTileset.Create(ExeDir + 'Resource\', fResource.Sprites[rxTiles]);
    LoadMapElemDAT(ExeDir + 'data\defines\mapelem.dat');
    LoadPatternDAT(ExeDir + 'data\defines\pattern.dat');

    fResources := TKMResourceCollection.Create;
    fHouseDat := TKMHouseDatCollection.Create;
    fUnitDat := TKMUnitDatCollection.Create;

  StepRefresh;
  fLog.AppendLog('ReadGFX is done');
  fDataState := dls_Menu;
  fLog.AppendLog('Resource loading state - Menu');
end;


procedure TResource.LoadGameResources(aAlphaShadows: boolean);
begin
  Assert(fRender <> nil, 'fRender inits OpenGL and we need OpenGL to make textures');

  {if fDataState <> dls_All then
  begin
    LoadMapElemDAT(ExeDir + 'data\defines\mapelem.dat');
    LoadPatternDAT(ExeDir + 'data\defines\pattern.dat');

    fResources := TKMResourceCollection.Create;
    fHouseDat := TKMHouseDatCollection.Create;
    fUnitDat := TKMUnitDatCollection.Create;
  end;}

  if (fDataState <> dls_All) or (aAlphaShadows <> fSprites.AlphaShadows) then
  begin
    fSprites.LoadGameResources(fHouseDat, fTileset.TextT, aAlphaShadows);

    fSprites.ClearTemp;
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


//Export Units graphics categorized by Unit and Action
procedure TResource.ExportUnitAnim;
var
  Folder: string;
  MyBitMap: TBitmap;
  U: TUnitType;
  A: TUnitActionType;
  D: TKMDirection;
  R: TResourceType;
  T: TUnitThought;
  i,ci:integer;
  sy,sx,y,x:integer;
  Used:array of Boolean;
  RXData: TRXData;
begin
  RXData := fSprites[rxUnits].RXData;

  Folder := ExeDir + 'Export\UnitAnim\';
  ForceDirectories(Folder);

  MyBitMap := TBitmap.Create;
  MyBitMap.PixelFormat := pf24bit;

  if fUnitDat = nil then
    fUnitDat := TKMUnitDatCollection.Create;

  fSprites.LoadSprites(rxUnits, False); //BMP can't show alpha shadows

  for U := ut_Serf to ut_Serf do
  for A := Low(TUnitActionType) to High(TUnitActionType) do
  for D := dir_N to dir_NW do
  if fUnitDat[U].UnitAnim[A,D].Step[1] <> -1 then
  for i := 1 to fUnitDat[U].UnitAnim[A, D].Count do
  begin
    ForceDirectories(Folder + fUnitDat[U].UnitName + '\' + UnitAct[A] + '\');

    if fUnitDat[U].UnitAnim[A,D].Step[i] + 1 <> 0 then
    begin
      ci := fUnitDat[U].UnitAnim[A,D].Step[i] + 1;

      sx := RXData.Size[ci].X;
      sy := RXData.Size[ci].Y;
      MyBitMap.Width := sx;
      MyBitMap.Height := sy;

      for y:=0 to sy-1 do
      for x:=0 to sx-1 do
        MyBitMap.Canvas.Pixels[x,y] := RXData.RGBA[ci, y*sx+x] AND $FFFFFF;

      if sy > 0 then
        MyBitMap.SaveToFile(Folder +
          fUnitDat[U].UnitName + '\' + UnitAct[A] + '\' +
          'Dir' + IntToStr(Byte(D)) + '_' + int2fix(i, 2) + '.bmp');
    end;
  end;

  CreateDir(Folder + '_Unused');
  SetLength(Used, Length(RXData.Size));

  //Exclude actions
  for U := Low(TUnitType) to High(TUnitType) do
  for A := Low(TUnitActionType) to High(TUnitActionType) do
  for D := dir_N to dir_NW do
  if fUnitDat[U].UnitAnim[A,D].Step[1] <> -1 then
  for i := 1 to fUnitDat[U].UnitAnim[A,D].Count do
    Used[fUnitDat[U].UnitAnim[A,D].Step[i]+1] := fUnitDat[U].UnitAnim[A,D].Step[i]+1 <> 0;

  //Exclude serfs carrying stuff
  for R := Low(TResourceType) to High(TResourceType) do
  if R in [WARE_MIN..WARE_MAX] then
  for D := dir_N to dir_NW do
  if fUnitDat.SerfCarry[R,D].Step[1] <> -1 then
  for i := 1 to fUnitDat.SerfCarry[R,D].Count do
    Used[fUnitDat.SerfCarry[R,D].Step[i]+1] := fUnitDat.SerfCarry[R,D].Step[i]+1 <> 0;

  for T := Low(TUnitThought) to High(TUnitThought) do
  for i := ThoughtBounds[T,1] to  ThoughtBounds[T,2] do
    Used[I+1] := True;

  for ci:=1 to length(Used)-1 do
  if not Used[ci] then
  begin
    sx := RXData.Size[ci].X;
    sy := RXData.Size[ci].Y;
    MyBitMap.Width := sx;
    MyBitMap.Height := sy;

    for y:=0 to sy-1 do for x:=0 to sx-1 do
      MyBitMap.Canvas.Pixels[x,y] := RXData.RGBA[ci, y*sx+x] AND $FFFFFF;

    if sy>0 then MyBitMap.SaveToFile(Folder + '_Unused\_'+int2fix(ci,4) + '.bmp');
  end;

  fSprites.ClearTemp;
  MyBitMap.Free;
end;


//Export Houses graphics categorized by House and Action
procedure TResource.ExportHouseAnim;
var
  Folder: string;
  MyBitMap:TBitmap;
  ID:THouseType;
  Ac:THouseActionType;
  Q,Beast,i,k,ci:integer;
  sy,sx,y,x:integer;
  RXData: TRXData;
begin
  RXData := fSprites[rxHouses].RXData;

  Folder := ExeDir + 'Export\HouseAnim\';
  ForceDirectories(Folder);

  MyBitMap := TBitmap.Create;
  MyBitMap.PixelFormat := pf24bit;

  fHouseDat := TKMHouseDatCollection.Create;
  fSprites.LoadSprites(rxHouses, False); //BMP can't show alpha shadows

  ci:=0;
  for ID:=Low(THouseType) to High(THouseType) do
    for Ac:=ha_Work1 to ha_Flag3 do
      for k:=1 to fHouseDat[ID].Anim[Ac].Count do
      begin
        ForceDirectories(Folder+fHouseDat[ID].HouseName+'\'+HouseAction[Ac]+'\');
        if fHouseDat[ID].Anim[Ac].Step[k] <> -1 then
          ci := fHouseDat[ID].Anim[Ac].Step[k]+1;

        sx := RXData.Size[ci].X;
        sy := RXData.Size[ci].Y;
        MyBitMap.Width:=sx;
        MyBitMap.Height:=sy;

        for y:=0 to sy-1 do for x:=0 to sx-1 do
          MyBitMap.Canvas.Pixels[x,y] := RXData.RGBA[ci,y*sx+x] AND $FFFFFF;

        if sy>0 then MyBitMap.SaveToFile(
        Folder+fHouseDat[ID].HouseName+'\'+HouseAction[Ac]+'\_'+int2fix(k,2)+'.bmp');
      end;

  ci:=0;
  for Q:=1 to 2 do
  begin
    if Q=1 then ID:=ht_Swine
           else ID:=ht_Stables;
    CreateDir(Folder+'_'+fHouseDat[ID].HouseName+'\');
    for Beast:=1 to 5 do
      for i:=1 to 3 do
        for k:=1 to fHouseDat.BeastAnim[ID,Beast,i].Count do
        begin
          CreateDir(Folder+'_'+fHouseDat[ID].HouseName+'\'+int2fix(Beast,2)+'\');
          if fHouseDat.BeastAnim[ID,Beast,i].Step[k]+1<>0 then
            ci := fHouseDat.BeastAnim[ID,Beast,i].Step[k]+1;

          sx:=RXData.Size[ci].X;
          sy:=RXData.Size[ci].Y;
          MyBitMap.Width:=sx;
          MyBitMap.Height:=sy;

          for y:=0 to sy-1 do for x:=0 to sx-1 do
            MyBitMap.Canvas.Pixels[x,y] := RXData.RGBA[ci,y*sx+x] AND $FFFFFF;

          if sy>0 then MyBitMap.SaveToFile(Folder+'_'+fHouseDat[ID].HouseName+'\'+int2fix(Beast,2)+'\_'+int2fix(i,1)+'_'+int2fix(k,2)+'.bmp');
        end;
  end;

  fSprites.ClearTemp;
  MyBitMap.Free;
end;


//Export Trees graphics categorized by ID
procedure TResource.ExportTreeAnim;
var
  Folder: string;
  MyBitMap: TBitmap;
  i,k,ci:integer;
  sy,sx,y,x:integer;
  RXData: TRXData;
begin
  RXData := fSprites[rxTrees].RXData;

  Folder := ExeDir + 'Export\TreeAnim\';
  ForceDirectories(Folder);

  MyBitMap := TBitmap.Create;
  MyBitMap.PixelFormat := pf24bit;

  LoadMapElemDAT(ExeDir + 'data\defines\mapelem.dat');
  fSprites.LoadSprites(rxTrees, False); //BMP can't show alpha shadows

  ci:=0;
  for i:=1 to MapElemQty do
    for k:=1 to MapElem[i].Count do
    begin
      if MapElem[i].Step[k]+1 <> 0 then
        ci := MapElem[i].Step[k]+1;

      sx := RXData.Size[ci].X;
      sy := RXData.Size[ci].Y;
      MyBitMap.Width := sx;
      MyBitMap.Height := sy;

      for y:=0 to sy-1 do for x:=0 to sx-1 do
        MyBitMap.Canvas.Pixels[x,y] := RXData.RGBA[ci,y*sx+x] AND $FFFFFF;

      //We can insert field here and press Export>TreeAnim. Rename each folder after export to 'Cuttable',
      //'Quad' and etc.. there you'll have it. Note, we use 1..254 counting, JBSnorro uses 0..253 counting
      if sy>0 then MyBitMap.SaveToFile(
        Folder+{inttostr(word(MapElem[i].DiagonalBlocked))+'_'+}int2fix(i,3)+'_'+int2fix(k,2)+'.bmp');
    end;

  fSprites.ClearTemp;
  MyBitMap.Free;
end;


end.
