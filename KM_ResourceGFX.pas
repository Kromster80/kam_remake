unit KM_ResourceGFX;
{$I KaM_Remake.inc}
interface
uses
  {$IFDEF Unix} LCLIntf, LCLType, {$ENDIF}
  Classes, Forms, Graphics, Math, SysUtils,
  KM_CommonEvents, KM_Defaults,
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
    procedure StepCaption(const aCaption: string); overload;
    procedure StepCaption(const aTextID: Word); overload;

    function LoadMapElemDAT(const FileName: string): Boolean;
    function LoadPatternDAT(const FileName: string): Boolean;
  public
    OnLoadingStep: TEvent;
    OnLoadingText: TStringEvent;

    constructor Create(aLS: TEvent; aLT: TStringEvent);
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
    property Sprites: TKMSprites read fSprites;
    property Tileset: TKMTileset read fTileset;

    //procedure ExportRX2BMP(RXid:integer);
    //procedure ExportTreeAnim2BMP;
    //procedure ExportHouseAnim2BMP;
    procedure ExportUnitAnim2BMP;
  end;

  var
    fResource: TResource;

    procedure ExportRX2BMP(RXid:integer);
    procedure ExportTreeAnim2BMP;
    procedure ExportHouseAnim2BMP;


implementation
uses KromUtils, KM_Render, KM_Log, KM_TextLibrary, KM_Points;


{ TResource }
constructor TResource.Create(aLS: TEvent; aLT: TStringEvent);
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


procedure TResource.StepCaption(const aTextID: Word);
begin
  if aTextID <> 0 then StepCaption(fTextLibrary[aTextID]);
end;


procedure TResource.LoadMenuResources(const aLocale: string);
begin
  Assert(fRender <> nil, 'fRender should be init before ReadGFX to be able access OpenGL');

  StepCaption('Reading palettes ...');
  fPalettes := TKMPalettes.Create;
  fPalettes.LoadPalettes;
  fLog.AppendLog('Reading palettes', True);

  fSprites := TKMSprites.Create(fPalettes, StepRefresh, StepCaption);

  fCursors := TKMCursors.Create;

  fSprites.LoadMenuResources(fCursors);

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
begin
  if fDataState = dls_All then Exit;

  Assert(fRender <> nil, 'fRender inits OpenGL and we need OpenGL to make textures');

  LoadMapElemDAT(ExeDir + 'data\defines\mapelem.dat');
  LoadPatternDAT(ExeDir + 'data\defines\pattern.dat');

  fResources := TKMResourceCollection.Create;
  fHouseDat := TKMHouseDatCollection.Create;
  fUnitDat := TKMUnitDatCollection.Create;
  fTileset := TKMTileset.Create(ExeDir + 'Resource\');

  fSprites.LoadGameResources(fHouseDat, fTileset.TextT);

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
  CreateDir(ExeDir + 'Export\' + fResource.Sprites.FileName[RT] + '.rx\');
  MyBitMap := TBitmap.Create;
  MyBitMap.PixelFormat := pf24bit;

  fResource.Sprites.LoadSprites(RT,False,nil,nil);

  for id:=1 to RXData[RT].Qty do
  begin
    sx := RXData[RT].Size[id].X;
    sy := RXData[RT].Size[id].Y;
    MyBitMap.Width  := sx;
    MyBitMap.Height := sy;

    for i:=0 to sy-1 do for k:=0 to sx-1 do
      MyBitMap.Canvas.Pixels[k,i] := RXData[RT].RGBA[id,i*sx+k] AND $FFFFFF; //Drop Alpha value

    //Mark pivot location with a dot
    {k := sx + RXData[RT].Pivot[id].x;
    i := sy + RXData[RT].Pivot[id].y;
    if InRange(i, 0, sy-1) and InRange(k, 0, sx-1) then
      MyBitMap.Canvas.Pixels[k,i] := $FF00FF;}

    if sy > 0 then
      MyBitMap.SaveToFile(ExeDir + 'Export\' + fResource.Sprites.FileName[RT] + '.rx\' + fResource.Sprites.FileName[RT] + '_' + int2fix(ID, 4) + '.bmp');

    SetLength(RXData[RT].Data[id], 0);
  end;

  MyBitMap.Free;
end;

{Export Units graphics categorized by Unit and Action}
procedure TResource.ExportUnitAnim2BMP;
var
  MyBitMap: TBitmap;
  U: TUnitType;
  A: TUnitActionType;
  D: TKMDirection;
  R: TResourceType;
  //T: TUnitThought;
  i,ci:integer;
  sy,sx,y,x:integer;
  Used:array of Boolean;
begin
  CreateDir(ExeDir + 'Export\');
  CreateDir(ExeDir + 'Export\UnitAnim\');
  MyBitMap := TBitmap.Create;
  MyBitMap.PixelFormat := pf24bit;

  if fUnitDat = nil then
    fUnitDat := TKMUnitDatCollection.Create;

  fResource.Sprites.LoadSprites(rxUnits,False,nil,nil);

  for U := ut_Serf to ut_Serf do
  for A := Low(TUnitActionType) to High(TUnitActionType) do
  for D := dir_N to dir_NW do
  if fUnitDat[U].UnitAnim[A,D].Step[1] <> -1 then
  for i := 1 to fUnitDat[U].UnitAnim[A, D].Count do
  begin
    CreateDir(ExeDir+'Export\UnitAnim\'+fUnitDat[U].UnitName+'\');
    CreateDir(ExeDir+'Export\UnitAnim\'+fUnitDat[U].UnitName+'\'+UnitAct[A]+'\');

    if fUnitDat[U].UnitAnim[A,D].Step[i] + 1 <> 0 then
    begin
      ci := fUnitDat[U].UnitAnim[A,D].Step[i] + 1;

      sx := RXData[rxUnits].Size[ci].X;
      sy := RXData[rxUnits].Size[ci].Y;
      MyBitMap.Width := sx;
      MyBitMap.Height := sy;

      for y:=0 to sy-1 do
      for x:=0 to sx-1 do
        MyBitMap.Canvas.Pixels[x,y] := RXData[rxUnits].RGBA[ci, y*sx+x] AND $FFFFFF;

      if sy > 0 then
        MyBitMap.SaveToFile(ExeDir + 'Export\UnitAnim\' +
          fUnitDat[U].UnitName + '\' + UnitAct[A] + '\' +
          'Dir' + IntToStr(Byte(D)) + '_' + int2fix(i, 2) + '.bmp');
    end;
  end;

  CreateDir(ExeDir+'Export\UnitAnim\_Unused');
  SetLength(Used, Length(RXData[rxUnits].Size));

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

  //todo: Exclude thoughts
  {for T := Low(TUnitThought) to High(TUnitThought) do
  if T in [th_Eat..th_Quest] then
  if fUnitDat.SerfCarry[R,D].Step[1] <> -1 then
  for i := 1 to fUnitDat.SerfCarry[R,D].Count do
    Used[fUnitDat.SerfCarry[R,D].Step[i]+1] := fUnitDat.SerfCarry[R,D].Step[i]+1 <> 0;}

  for ci:=1 to length(Used)-1 do
  if not Used[ci] then
  begin
    sx := RXData[rxUnits].Size[ci].X;
    sy := RXData[rxUnits].Size[ci].Y;
    MyBitMap.Width := sx;
    MyBitMap.Height := sy;

    for y:=0 to sy-1 do for x:=0 to sx-1 do
      MyBitMap.Canvas.Pixels[x,y] := RXData[rxUnits].RGBA[ci, y*sx+x] AND $FFFFFF;

    if sy>0 then MyBitMap.SaveToFile(
      ExeDir+'Export\UnitAnim\_Unused\'+'_'+int2fix(ci,4)+'.bmp');
  end;

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
  fResource.Sprites.LoadSprites(rxHouses,False,nil,nil);

  ci:=0;
  for ID:=Low(THouseType) to High(THouseType) do
    for Ac:=ha_Work1 to ha_Flag3 do
      for k:=1 to fResource.HouseDat[ID].Anim[Ac].Count do
      begin
        CreateDir(ExeDir+'Export\HouseAnim\'+fResource.HouseDat[ID].HouseName+'\');
        CreateDir(ExeDir+'Export\HouseAnim\'+fResource.HouseDat[ID].HouseName+'\'+HouseAction[Ac]+'\');
        if fResource.HouseDat[ID].Anim[Ac].Step[k] <> -1 then
          ci := fResource.HouseDat[ID].Anim[Ac].Step[k]+1;

        sx := RXData[rxHouses].Size[ci].X;
        sy := RXData[rxHouses].Size[ci].Y;
        MyBitMap.Width:=sx;
        MyBitMap.Height:=sy;

        for y:=0 to sy-1 do for x:=0 to sx-1 do
          MyBitMap.Canvas.Pixels[x,y] := RXData[rxHouses].RGBA[ci,y*sx+x] AND $FFFFFF;

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

          sx:=RXData[rxHouses].Size[ci].X;
          sy:=RXData[rxHouses].Size[ci].Y;
          MyBitMap.Width:=sx;
          MyBitMap.Height:=sy;

          for y:=0 to sy-1 do for x:=0 to sx-1 do
            MyBitMap.Canvas.Pixels[x,y] := RXData[rxHouses].RGBA[ci,y*sx+x] AND $FFFFFF;

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
  fResource.Sprites.LoadSprites(rxTrees,False,nil,nil);

  ci:=0;
  for i:=1 to MapElemQty do
    for k:=1 to MapElem[i].Count do
    begin
      if MapElem[i].Step[k]+1 <> 0 then
        ci := MapElem[i].Step[k]+1;

      sx := RXData[rxTrees].Size[ci].X;
      sy := RXData[rxTrees].Size[ci].Y;
      MyBitMap.Width := sx;
      MyBitMap.Height := sy;

      for y:=0 to sy-1 do for x:=0 to sx-1 do
        MyBitMap.Canvas.Pixels[x,y] := RXData[rxTrees].RGBA[ci,y*sx+x] AND $FFFFFF;

      //We can insert field here and press Export>TreeAnim. Rename each folder after export to 'Cuttable',
      //'Quad' and etc.. there you'll have it. Note, we use 1..254 counting, JBSnorro uses 0..253 counting
      if sy>0 then MyBitMap.SaveToFile(
        ExeDir+'Export\TreeAnim\'+{inttostr(word(MapElem[i].DiagonalBlocked))+'_'+}int2fix(i,3)+'_'+int2fix(k,2)+'.bmp');
    end;

  MyBitMap.Free;
end;


end.
