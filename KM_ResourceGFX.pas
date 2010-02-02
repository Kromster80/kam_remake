unit KM_ResourceGFX;
interface
uses
  {$IFDEF VER140} OpenGL, {$ENDIF}
  {$IFDEF FPC} GL, {$ENDIF}
  Windows, Forms, Graphics, SysUtils, Math, dglOpenGL, KM_Defaults, KM_LoadLib, Classes
  {$IFDEF VER140}, ZLibEx {$ENDIF};

type
  TByteArray2 = array of Byte;
  TexMode = (tm_NoCol, tm_TexID, tm_AltID, tm_AlphaTest); //Defines way to decode sprites using palette info
  TDataLoadingState = (dls_None, dls_Menu, dls_All); //Resources are loaded in 2 steps, for menu and the rest

type
  TResource = class
  private
    DataState:TDataLoadingState;

    procedure StepRefresh();
    procedure StepCaption(aCaption:string);

    function LoadPalette(filename:string; PalID:byte):boolean;
    function LoadMapElemDAT(filename:string):boolean;
    function LoadPatternDAT(filename:string):boolean;
    function LoadHouseDAT(filename:string):boolean;
    function LoadUnitDAT(filename:string):boolean;
    function LoadFont(filename:string; aFont:TKMFont; WriteFontToBMP:boolean):boolean;

    function LoadRX(filename:string; ID:integer):boolean;
    procedure MakeGFX_AlphaTest(Sender: TObject; RXid:integer);
    procedure MakeGFX(Sender: TObject; RXid:integer);

    procedure MakeMiniMapColors(FileName:string);
    procedure MakeCursors(RXid:integer);

    function GenTexture(mx, my:integer; Data:TByteArray2; Mode:TexMode; const UsePal:byte=DEF_PAL):gluint; //This should belong to TRender?
  public
    constructor Create;
    function LoadMenuResources(aLocale:string):boolean;
    function LoadGameResources():boolean;

    property GetDataState:TDataLoadingState read DataState;
    function GetUnitSequenceLength(aUnitType:TUnitType; aAction:TUnitActionType; aDir:TKMDirection):integer;

    procedure LoadFonts(DoExport:boolean; aLocale:string);
    //procedure ExportRX2BMP(RXid:integer);
    //procedure ExportTreeAnim2BMP();
    //procedure ExportHouseAnim2BMP();
    //procedure ExportUnitAnim2BMP();
  end;

  var
    fResource:TResource;

    procedure ExportRX2BMP(RXid:integer);
    procedure ExportTreeAnim2BMP();
    procedure ExportHouseAnim2BMP();
    procedure ExportUnitAnim2BMP();


implementation
uses KromUtils, KM_Unit1, KM_Render, KM_CommonTypes, KM_Utils, KM_TGATexture;


constructor TResource.Create;
begin
  Inherited;
  fLog.AssertToLog(fTextLibrary<>nil,'fTextLibrary should be init before ReadGFX');
  fLog.AssertToLog(fRender<>nil,'fRender should be init before ReadGFX to be able access OpenGL');
  DataState:=dls_None;
end;


procedure TResource.StepRefresh();
begin
  if not FormLoading.Visible then exit;
  FormLoading.Bar1.StepIt;
  FormLoading.Refresh;
end;


procedure TResource.StepCaption(aCaption:string);
begin
  if not FormLoading.Visible then exit;
  FormLoading.Label1.Caption:=aCaption;
  FormLoading.Refresh;
end;


function TResource.LoadMenuResources(aLocale:string):boolean;
var i:integer;
begin
//  Result:=false;
  fLog.AssertToLog(fTextLibrary <> nil, 'fTextLibrary should be init before ReadGFX');
  fLog.AssertToLog(fRender <> nil, 'fRender should be init before ReadGFX to be able access OpenGL');

  StepCaption('Reading palettes ...');
  for i:=1 to length(PalFiles) do
   LoadPalette(ExeDir+'data\gfx\'+PalFiles[i],i);
  fLog.AppendLog('Reading palettes',true);

  RXData[1].Title:='Trees';       RXData[1].NeedTeamColors:=false;
  RXData[2].Title:='Houses';      RXData[2].NeedTeamColors:=true;
  RXData[3].Title:='Units';       RXData[3].NeedTeamColors:=true;
  RXData[4].Title:='GUI';         RXData[4].NeedTeamColors:=true; //Required for unit scrolls, etc.
  RXData[5].Title:='GUIMain';     RXData[5].NeedTeamColors:=false;
  RXData[6].Title:='GUIMainH';    RXData[6].NeedTeamColors:=false;

  for i:=4 to 6 do
  begin
    StepCaption('Reading '+RXData[i].Title+' GFX ...');
    fLog.AppendLog('Reading '+RXData[i].Title+'.rx',LoadRX(ExeDir+'data\gfx\res\'+RXData[i].Title+'.rx',i));
    if i=4 then MakeCursors(4); //Make GUI items before they are flushed
    MakeGFX(nil,i);
    StepRefresh();
  end;

  StepCaption('Reading fonts ...');
  LoadFonts(false, aLocale);
  fLog.AppendLog('Read fonts is done');

  StepRefresh();
  fLog.AppendLog('ReadGFX is done');
  DataState:=dls_Menu;
  Result:=true;
end;


function TResource.LoadGameResources():boolean;
var i:integer;
begin
//  Result:=false;
  fLog.AssertToLog(fTextLibrary<>nil,'fTextLibrary should be init before ReadGFX');
  fLog.AssertToLog(fRender<>nil,'fRender should be init before ReadGFX to be able access OpenGL');

  StepCaption('Reading defines ...');
  fLog.AppendLog('Reading mapelem.dat',LoadMapElemDAT(ExeDir+'data\defines\mapelem.dat')); StepRefresh();
  fLog.AppendLog('Reading pattern.dat',LoadPatternDAT(ExeDir+'data\defines\pattern.dat')); StepRefresh();
  fLog.AppendLog('Reading houses.dat', LoadHouseDAT(ExeDir+'data\defines\houses.dat'));    StepRefresh();
  fLog.AppendLog('Reading unit.dat',   LoadUnitDAT(ExeDir+'data\defines\unit.dat'));       StepRefresh();

  for i:=1 to 3 do
  if (i=1) or ((i=2) and MakeHouseSprites) or ((i=3) and MakeUnitSprites) then
  begin
    StepCaption('Reading '+RXData[i].Title+' GFX ...');
    fLog.AppendLog('Reading '+RXData[i].Title+'.rx',LoadRX(ExeDir+'data\gfx\res\'+RXData[i].Title+'.rx',i));
    MakeGFX(nil,i);
    if i=2 then MakeGFX_AlphaTest(nil,i); //Make alphas for house building
    StepRefresh();
  end;

  StepCaption('Making minimap colors ...');
  MakeMiniMapColors(ExeDir+'Resource\Tiles1.tga');
  fLog.AppendLog('Prepared MiniMap colors...');
  StepRefresh();
  fLog.AppendLog('ReadGFX is done');
  DataState:=dls_All;
  Result:=true;
end;


procedure TResource.LoadFonts(DoExport:boolean; aLocale:string);
var i:integer;
begin
  for i:=1 to length(FontFiles) do
    LoadFont(ExeDir+'data\gfx\fonts\'+FontFiles[i]+'.'+aLocale+'.fnt',TKMFont(i),DoExport);
end;


function TResource.GetUnitSequenceLength(aUnitType:TUnitType; aAction:TUnitActionType; aDir:TKMDirection):integer;
begin
  Result := UnitSprite[Integer(aUnitType)].Act[Integer(aAction)].Dir[Integer(aDir)].Count;
end;



//=============================================
//Reading Palette for trees/objects
//=============================================
function TResource.LoadPalette(filename:string; PalID:byte):boolean;
var f:file; i:integer;
begin
  Result:=false;
  if not CheckFileExists(filename,true) then exit;

  assignfile(f,filename);
  reset(f,1);
  blockread(f,Pal[PalID],48); //Unknown and/or unimportant
  blockread(f,Pal[PalID],768); //256*3
  closefile(f);

  if PalID = pal_lin then //Make greyscale linear Pal
    for i:=0 to 255 do begin
      Pal[pal_lin,i+1,1] := i;
      Pal[pal_lin,i+1,2] := i;
      Pal[pal_lin,i+1,3] := i;
    end;

Result:=true;
end;


//=============================================
//Reading map elements (has animation data)
//=============================================
function TResource.LoadMapElemDAT(filename:string):boolean;
var ii,kk:integer; ft:textfile; f:file;
begin
  Result:=false;
  if not CheckFileExists(filename) then exit;
  assignfile(f,filename); reset(f,1);
  blockread(f,MapElem[1],MapElemQty*99); //256*3
  closefile(f);

  if WriteResourceInfoToTXT then begin
    assignfile(ft,ExeDir+'Trees.txt'); rewrite(ft);
    for ii:=1 to MapElemQty do begin
    writeln(ft);
    writeln(ft,ii);
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


//=============================================
//Reading pattern data (tile info)
//=============================================
function TResource.LoadPatternDAT(filename:string):boolean;
var ii,kk:integer; ft:textfile; f:file; s:byte;
begin
  Result:=false;
  if not CheckFileExists(filename) then exit;
  assignfile(f,filename); reset(f,1);
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
//Reading houses.dat data
//=============================================
function TResource.LoadHouseDAT(filename:string):boolean;
var ii,kk,h:integer; ft:textfile; f:file;
begin
Result:=false;
if not CheckFileExists(filename) then exit;
assignfile(f,filename); reset(f,1);
blockread(f,HouseDATs,30*70);
for h:=1 to 29 do begin
blockread(f,HouseDAT[h],88+19*70+270);
end;
//Append info for new houses
{for ii:=30 to HOUSE_COUNT do begin
fillChar(HouseDAT[ii],SizeOf(HouseDAT[ii]),#0);
HouseDAT[ii].StonePic:=129-1;
HouseDAT[ii].WoodPic:=130-1;
HouseDAT[ii].WoodPal:=132-1;
HouseDAT[ii].StonePal:=131-1;
HouseDAT[ii].WoodPicSteps:=7;
HouseDAT[ii].StonePicSteps:=8;
HouseDAT[ii].WoodCost:=1;
HouseDAT[ii].StoneCost:=1;
HouseDAT[ii].OwnerType:=0;
HouseDAT[ii].MaxHealth:=100;
end;}
closefile(f);

  if WriteResourceInfoToTXT then begin
    assignfile(ft,ExeDir+'Houses.csv'); rewrite(ft);
    writeln(ft,'House;a1;a3;a4;a5;a8;Foot---------->;');
    for ii:=1 to HOUSE_COUNT do begin
    //writeln(ft);
    write(ft,fTextLibrary.GetTextString(siHouseNames+ii)+';');
    {  write(ft,'Resource In: ');
      for kk:=1 to 4 do if HouseDAT[ii].SupplyIn[kk,1]>0 then
      write(ft,'#') else write(ft,' ');
      writeln(ft);
      write(ft,'Resource Out: ');
      for kk:=1 to 4 do if HouseDAT[ii].SupplyOut[kk,1]>0 then
      write(ft,'#') else write(ft,' ');
      writeln(ft);
      for kk:=1 to 19 do writeln(ft,HouseAction[kk]+#9+inttostr(HouseDAT[ii].Anim[kk].Count));}
      //write(ft,inttostr(HouseDAT[ii].WoodPicSteps)+'wooding ;');
      //write(ft,inttostr(HouseDAT[ii].StonePicSteps)+'stoning ;');
      write(ft,inttostr(HouseDAT[ii].a1)+';'); //0
      //write(ft,'X '+inttostr(HouseDAT[ii].EntranceOffsetX)+';');
      //write(ft,'Y '+inttostr(HouseDAT[ii].EntranceOffsetY)+';'); //0
      //write(ft,inttostr(HouseDAT[ii].EntranceOffsetXpx)+';');
      //write(ft,inttostr(HouseDAT[ii].EntranceOffsetXpx)+';');
      {writeln(ft);
      for kk:=1 to length(HouseDAT[ii].BuildArea) do begin
        for h:=1 to 10 do
          write(ft,inttostr(HouseDAT[ii].BuildArea[kk,h])+';');
        writeln(ft,';');
      end; }
      //write(ft,inttostr(HouseDAT[ii].WoodCost)+';');
      //write(ft,inttostr(HouseDAT[ii].StoneCost)+';');
      //for kk:=1 to 12 do write(ft,'dx '+inttostr(HouseDAT[ii].BuildSupply[kk].MoveX)+' dy '+inttostr(HouseDAT[ii].BuildSupply[kk].MoveY)+';');
      write(ft,inttostr(HouseDAT[ii].a5)+';');
      //write(ft,'Area '+inttostr(HouseDAT[ii].SizeArea)+';');
      //write(ft,'Size '+inttostr(HouseDAT[ii].SizeX)+'x'+inttostr(HouseDAT[ii].SizeY)+';');
      write(ft,'Size2 '+inttostr(HouseDAT[ii].sx2)+'x'+inttostr(HouseDAT[ii].sy2)+';');
      write(ft,inttostr(HouseDAT[ii].WorkerWork)+'W sec;');
      write(ft,inttostr(HouseDAT[ii].WorkerRest)+'R sec;');
      //for kk:=1 to 4 do write(ft,TypeToString(TResourceType(HouseDAT[ii].ResInput[kk]+1))+';');
      //for kk:=1 to 4 do write(ft,TypeToString(TResourceType(HouseDAT[ii].ResOutput[kk]+1))+';');
      //write(ft,'Product x'+inttostr(HouseDAT[ii].ResProductionX)+';');
      //write(ft,inttostr(HouseDAT[ii].MaxHealth)+'hp;');
      //write(ft,'Sight '+inttostr(HouseDAT[ii].Sight)+';');
      //write(ft,TypeToString(TUnitType(HouseDAT[ii].OwnerType+1))+';');
      for kk:=1 to 36 do write(ft,inttostr(HouseDAT[ii].Foot[kk])+';');
      writeln(ft);
    end;
    closefile(ft);
end;

//Form1.Close;

Result:=true;
end;

//=============================================
//Reading unit.dat data
//=============================================
function TResource.LoadUnitDAT(filename:string):boolean;
var ii,kk,jj,hh:integer; ft:textfile; f:file;
begin
Result:=false;
if not CheckFileExists(filename) then exit;
assignfile(f,filename); reset(f,1);
for ii:=1 to 28 do begin
blockread(f,SerfCarry[ii],8*70);
end;
for ii:=1 to 41 do begin
blockread(f,UnitStat[ii],22);
blockread(f,UnitSprite[ii],112*70);
blockread(f,UnitSprite2[ii],36);
end;
closefile(f);

  if WriteResourceInfoToTXT then begin
    assignfile(ft,ExeDir+'UnitDAT.csv'); rewrite(ft);
    writeln(ft,'Name;x1;Attack;AttackHorseBonus;x4;HitPoints;Speed;x7;Sight;x9;x10;CanWalkOut;0;');
    for ii:=1 to 40 do begin
      write(ft,fTextLibrary.GetTextString(siUnitNames+ii)+';');
      write(ft,inttostr(UnitStat[ii].x1)+';');
      write(ft,inttostr(UnitStat[ii].Attack)+';');
      write(ft,inttostr(UnitStat[ii].AttackHorseBonus)+';');
      write(ft,inttostr(UnitStat[ii].x4)+';');
      write(ft,inttostr(UnitStat[ii].HitPoints)+'hp;');
      write(ft,inttostr(UnitStat[ii].Speed)+';');
      write(ft,inttostr(UnitStat[ii].x7)+';');
      write(ft,inttostr(UnitStat[ii].Sight)+';');
      write(ft,inttostr(UnitStat[ii].x9)+';');
      write(ft,inttostr(UnitStat[ii].x10)+';');
      write(ft,inttostr(UnitStat[ii].CanWalkOut)+';');
      write(ft,inttostr(UnitStat[ii].x11)+';');
      writeln(ft);
    end;
    closefile(ft);

    //This is a bad idea to fix anything here, but at the time it's the simplest solution
    //Woodcutter(2) needs to shuffle actions
    //Split planting from chopping
    UnitSprite[2].Act[5].Dir[1]:=UnitSprite[2].Act[2].Dir[1];
    UnitSprite[2].Act[2].Dir[1]:=UnitSprite[2].Act[5].Dir[2];

    assignfile(ft,ExeDir+'Units.txt'); rewrite(ft);
    for ii:=1 to 40 do begin
    writeln(ft);
    writeln(ft);
    writeln(ft,'NewUnit'+inttostr(ii));
    for kk:=1 to 14 do
    for hh:=1 to 8 do
    //  if UnitSprite[ii].Act[kk].Dir[hh].Step[1]>0 then
        begin
          write(ft,inttostr(kk)+'.'+inttostr(hh)+#9);
          for jj:=1 to 30 do
          if UnitSprite[ii].Act[kk].Dir[hh].Step[jj]>0 then //write(ft,'#');
          write(ft,inttostr(UnitSprite[ii].Act[kk].Dir[hh].Step[jj])+'. ');
          write(ft,inttostr(UnitSprite[ii].Act[kk].Dir[hh].Count)+' ');
          write(ft,inttostr(UnitSprite[ii].Act[kk].Dir[hh].MoveX)+' ');
          write(ft,inttostr(UnitSprite[ii].Act[kk].Dir[hh].MoveY)+' ');
          writeln(ft);
        end;
    end;
    closefile(ft);
  end;
Result:=true;
end;


function TResource.LoadFont(filename:string; aFont:TKMFont; WriteFontToBMP:boolean):boolean;
const
  TexWidth=256; //Connected to TexData, don't change
var
  f:file;
  p,t:byte;
  a,b,c,d:word;
  i,k,ci,ck:integer;
  MaxHeight:integer;
  AdvX,AdvY:integer;
  TD:array of byte;
  MyBitMap:TBitMap;
begin
  Result:=false;
  MaxHeight:=0;
  if not CheckFileExists(filename, true) then exit;

  assignfile(f,filename); reset(f,1);
  blockread(f,a,2); blockread(f,b,2);
  blockread(f,c,2); blockread(f,d,2);
  blockread(f,FontData[byte(aFont)].Pal[0],256);

  //Read font data
  for i:=0 to 255 do
    if FontData[byte(aFont)].Pal[i]<>0 then
      with FontData[byte(aFont)].Letters[i] do begin
        blockread(f,Width,4);
        blockread(f,Add,8);
        MaxHeight:=Math.max(MaxHeight,Height);
        fLog.AssertToLog(Width*Height<>0,'Font data Width*Height <> 0'); //Fon01.fnt seems to be damaged..
        blockread(f,Data[1],Width*Height);
      end;
  closefile(f);

  //Special fixes:
  if aFont=fnt_game then
  for i:=0 to 255 do
    if FontData[byte(aFont)].Pal[i]<>0 then
      for k:=1 to 4096 do
        if FontData[byte(aFont)].Letters[i].Data[k]<>0 then
          FontData[byte(aFont)].Letters[i].Data[k]:=218; //Light grey color in Pal2


  //Compile texture
  AdvX:=0; AdvY:=0;
  setlength(TD,TexWidth*TexWidth+1);
  FillChar(TD[0],TexWidth*TexWidth+1,$80); //Make some background

  for i:=0 to 255 do
    if FontData[byte(aFont)].Pal[i]<>0 then
      with FontData[byte(aFont)].Letters[i] do begin

      fLog.AssertToLog(FontData[byte(aFont)].Pal[i]=1,'FontData palette <> 1');

        if AdvX+Width+2>TexWidth then begin
          AdvX:=0;
          inc(AdvY,MaxHeight);
        end;

        for ci:=1 to Height do for ck:=1 to Width do
          TD[(AdvY+ci-1)*TexWidth+AdvX+1+ck-1]:=Data[(ci-1)*Width+ck];

        u1:=(AdvX+1)/TexWidth;
        v1:=AdvY/TexWidth;
        u2:=(AdvX+1+Width)/TexWidth;
        v2:=(AdvY+Height)/TexWidth;

        inc(AdvX,1+Width+1);
      end;

    FontData[byte(aFont)].TexID := GenTexture(TexWidth,TexWidth,@TD[0],tm_NoCol,FontPal[byte(aFont)]);

    FontData[byte(aFont)].Letters[32].Width:=7; //"Space" width

  //for i:=1 to 10 do
  if WriteFontToBMP then begin
    MyBitMap:=TBitMap.Create;
    MyBitmap.PixelFormat:=pf24bit;
    MyBitmap.Width:=TexWidth;
    MyBitmap.Height:=TexWidth;

    for ci:=0 to TexWidth-1 do for ck:=0 to TexWidth-1 do begin
      p:=FontPal[byte(aFont)];
      //p:=i;
      t:=TD[ci*TexWidth+ck]+1;
      MyBitmap.Canvas.Pixels[ck,ci]:=Pal[p,t,1]+Pal[p,t,2]*256+Pal[p,t,3]*65536;
    end;

    CreateDir(ExeDir+'Export\');
    CreateDir(ExeDir+'Export\Fonts\');
    MyBitmap.SaveToFile(ExeDir+'Export\Fonts\'+ExtractFileName(filename)+inttostr(p)+'.bmp');
    MyBitmap.Free;
  end;

  setlength(TD,0);
  Result:=true;

end;


//=============================================
//Reading RX Data
//=============================================
function TResource.LoadRX(filename:string; ID:integer):boolean;
  var i:integer; f:file;
begin
  Result:=false;
  if not CheckFileExists(filename) then exit;

  assignfile(f,filename); reset(f,1);
  blockread(f, RXData[ID].Qty, 4);
  blockread(f, RXData[ID].Pal, RXData[ID].Qty);

  if (not FullyLoadUnitsRX)and(RXData[ID].Title = 'Units') then RXData[ID].Qty:=7885;

  for i:=1 to RXData[ID].Qty do
    if RXData[ID].Pal[i] = 1 then
    begin
      blockread(f, RXData[ID].Size[i,1], 4);
      blockread(f, RXData[ID].Pivot[i].x, 8);
      setlength(RXData[ID].Data[i], RXData[ID].Size[i,1] * RXData[ID].Size[i,2] );
      blockread(f, RXData[ID].Data[i,0], RXData[ID].Size[i,1] * RXData[ID].Size[i,2] );
    end;

  closefile(f);
  fLog.AppendLog(RXData[ID].Title+' -',RXData[ID].Qty);
  Result:=true;
end;


//=============================================
//Make texture
//=============================================
function TResource.GenTexture(mx, my:integer; Data:TByteArray2; Mode:TexMode; const UsePal:byte=DEF_PAL):gluint;
var
  MyBitMap:TBitMap;
  i,k:word;
  x:byte;
  by:^cardinal;
  DestX, DestY:word;
  col:cardinal;
  TD:Pointer;
begin

Result := 0;

DestX := MakePOT(mx);
DestY := MakePOT(my);

if DestX*DestY = 0 then exit; //Do not generate zeroed textures

if Mode=tm_AlphaTest then begin
  Result := GenerateTextureCommon; //Should be called prior to glTexImage2D or gluBuild2DMipmaps
  glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, DestX, DestY, 0, GL_RGBA, GL_UNSIGNED_BYTE, Data);
  exit;
end;

//Convert palette bitmap data to 32bit RGBA texture data
TD:=AllocMem(DestX*DestY*4); //same as GetMem+FillChar(0)
for i:=0 to (DestY-1) do for k:=0 to (DestX-1) do
  if (i<my)and(k<mx) then begin
    x:=Data[i*mx+k];
    col:=0;
    if (x<>0) then begin
      by:=pointer(integer(TD)+((i+DestY-my)*DestX+k)*4); //Get pointer

      if Mode=tm_NoCol then
          col:=Pal[UsePal,x+1,1]+Pal[UsePal,x+1,2] SHL 8 +Pal[UsePal,x+1,3] SHL 16 OR $FF000000
      else

      if Mode=tm_TexID then
        if InRange(x,24,30) then
          col:=cardinal((byte(x-27)*42+128)*65793) OR $FF000000 //convert to greyscale B>>>>>W
          //Alternative method
          //col:=$FF000000
        else
          col:=Pal[UsePal,x+1,1]+Pal[UsePal,x+1,2] SHL 8 +Pal[UsePal,x+1,3] SHL 16 OR $FF000000
      else

      if Mode=tm_AltID then
        case x of
          24,30: col:=$70FFFFFF;   //7
          25,29: col:=$B0FFFFFF;   //11
          26,28: col:=$E0FFFFFF;   //14
          27:    col:=$FFFFFFFF;   //16
          else   col:=0;
        end;
        //Alternative method
        //if InRange(x,24,30) then col:= ((x-24)*21+128) + ((x-24)*21+128) SHL 8 + ((x-24)*21+128) SHL 16 OR $FF000000;

      by^:=col;
    end;
  end;

  Result := GenerateTextureCommon; //Should be called prior to glTexImage2D or gluBuild2DMipmaps
  case Mode of
    //Has no team color info at all
    tm_NoCol: glTexImage2D(GL_TEXTURE_2D, 0, GL_RGB5_A1, DestX, DestY, 0, GL_RGBA, GL_UNSIGNED_BYTE, TD);
    //Base layer with greyscale color
    tm_TexID: glTexImage2D(GL_TEXTURE_2D, 0, GL_RGB5_A1, DestX, DestY, 0, GL_RGBA, GL_UNSIGNED_BYTE, TD);
    //Team color layer
    tm_AltID: glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA2, DestX, DestY, 0, GL_RGBA, GL_UNSIGNED_BYTE, TD);
  end;

if WriteAllTexturesToBMP then begin
  CreateDir(ExeDir+'GenTextures\');
  MyBitMap:=TBitMap.Create;
  MyBitmap.PixelFormat:=pf32bit;
  MyBitmap.Width:=DestX;
  MyBitmap.Height:=DestY;
    for i:=0 to DestY-1 do for k:=0 to DestX-1 do begin
      MyBitmap.Canvas.Pixels[k,i]:=((PCardinal(Cardinal(TD)+(i*DestX+k)*4))^) AND $FFFFFF; //Ignore alpha
    end;
    MyBitmap.SaveToFile(ExeDir+'GenTextures\'+int2fix(Result,4)+'.bmp');
  MyBitMap.Free;
end;

FreeMem(TD);
end;


//=============================================
//Making OpenGL textures
//=============================================
{Take RX data and make nice textures out of it.
Textures should be POT to improve performance and avoid drivers bugs
In result we have GFXData filled.}
procedure TResource.MakeGFX_AlphaTest(Sender: TObject; RXid:integer);
var
  ID,ID1,ID2:integer; //RGB and A index
  ci,i,k,h,StepCount:integer;
  t,t2:integer;
  col:byte;
  WidthPOT,HeightPOT:integer;
  TD:array of byte;
begin

for ID:=1 to HOUSE_COUNT do
if HouseDAT[ID].StonePic<>-1 then //Exlude House27 which is unused
  for h:=1 to 2 do begin

    if h=1 then begin
      ID1:=HouseDAT[ID].WoodPic+1; ID2:=HouseDAT[ID].WoodPal+1; StepCount:=HouseDAT[ID].WoodPicSteps;
    end else begin
      ID1:=HouseDAT[ID].StonePic+1; ID2:=HouseDAT[ID].StonePal+1; StepCount:=HouseDAT[ID].StonePicSteps;
    end;

    WidthPOT:=MakePOT(RXData[RXid].Size[ID1,1]);
    HeightPOT:=MakePOT(RXData[RXid].Size[ID1,2]);
    setlength(TD,WidthPOT*HeightPOT*4+1);

    for i:=1 to HeightPOT do begin
      ci:=-1;
      for k:=1 to RXData[RXid].Size[ID1,1] do begin
        inc(ci);
        if i<=RXData[RXid].Size[ID1,2] then begin
          t:=((i-1)*WidthPOT+ci)*4;
          col:=RXData[RXid].Data[ID1,(i-1)*RXData[RXid].Size[ID1,1]+k-1]+1;
          TD[t+0]:=Pal[DEF_PAL,col,1];
          TD[t+1]:=Pal[DEF_PAL,col,2];
          TD[t+2]:=Pal[DEF_PAL,col,3];
          if i<=RXData[RXid].Size[ID2,2] then
          if k<=RXData[RXid].Size[ID2,1] then begin//Cos someimes ID2 is smaller by few pixels
            t2:=t+(RXData[RXid].Pivot[ID2].x-RXData[RXid].Pivot[ID1].x)*4; //Shift by pivot, always positive
            t2:=t2+(RXData[RXid].Pivot[ID2].y-RXData[RXid].Pivot[ID1].y)*WidthPOT*4; //Shift by pivot, always positive
            TD[t2+3]:=255-round(RXData[RXid].Data[ID2,(i-1)*RXData[RXid].Size[ID2,1]+k-1]*(255/StepCount));
          end;
          if col=1 then TD[t+3]:=0;
        end;
      end;
    end;

    GFXData[RXid,ID1].TexID := GenTexture(WidthPOT,HeightPOT,@TD[0],tm_AlphaTest);
    setlength(TD,0);
    GFXData[RXid,ID1].AltID:=0;
    GFXData[RXid,ID1].u1:=0;
    GFXData[RXid,ID1].v1:=0;
    GFXData[RXid,ID1].u2:=RXData[RXid].Size[ID1,1]/WidthPOT;
    GFXData[RXid,ID1].v2:=RXData[RXid].Size[ID1,2]/HeightPOT;
    GFXData[RXid,ID1].PxWidth:=RXData[RXid].Size[ID1,1];
    GFXData[RXid,ID1].PxHeight:=RXData[RXid].Size[ID1,2];
  end;

//Now we can safely dispose of RXData[RXid].Data to save us some more RAM
for i:=1 to RXData[RXid].Qty do setlength(RXData[RXid].Data[i],0);
end;


//=============================================
//Making OpenGL textures
//=============================================
{Take RX data and make nice textures out of it.
Textures should be POT to improve performance and avoid driver bugs
In result we have GFXData filled.}
procedure TResource.MakeGFX(Sender: TObject; RXid:integer);
var
  ci,j,i,k,LeftIndex,RightIndex,TexCount,SpanCount:integer;
  AllocatedRAM,RequiredRAM,ColorsRAM:integer;
  WidthPOT,HeightPOT:integer;
  TD:array of byte;
begin
  LeftIndex:=0; AllocatedRAM:=0; RequiredRAM:=0; ColorsRAM:=0; TexCount:=0;
  repeat
    inc(LeftIndex);

    WidthPOT:=RXData[RXid].Size[LeftIndex,1];
    HeightPOT:=MakePOT(RXData[RXid].Size[LeftIndex,2]);
    SpanCount:=1;

    //Pack textures with same POT height into rows to save memory
    //This also means fewer textures for GPU RAM == better performance
    while((LeftIndex+SpanCount<RXData[RXid].Qty)and //Keep packing until end of sprites
          (
            (HeightPOT=MakePOT(RXData[RXid].Size[LeftIndex+SpanCount,2])) //Pack if HeightPOT matches
        or((HeightPOT>=MakePOT(RXData[RXid].Size[LeftIndex+SpanCount,2]))AND(WidthPOT+RXData[RXid].Size[LeftIndex+SpanCount,1]<MakePOT(WidthPOT)))
          )and
          (WidthPOT+RXData[RXid].Size[LeftIndex+SpanCount,1]<=MAX_TEX_RESOLUTION)) //Pack until max Tex_Resolution approached
    do begin
      inc(WidthPOT,RXData[RXid].Size[LeftIndex+SpanCount,1]);
      inc(SpanCount);
//      if (RXid=4)and(LeftIndex+SpanCount = 49) then break; //Don't align
      if (RXid=5)and(RX5pal[LeftIndex]<>RX5pal[LeftIndex+SpanCount]) then break; //Don't align RX5 images for they use all different palettes
      if (RXid=6)and(RX6pal[LeftIndex]<>RX6pal[LeftIndex+SpanCount]) then break; //Don't align RX6 images for they use all different palettes
    end;

    RightIndex:=LeftIndex+SpanCount-1;
    WidthPOT:=MakePOT(WidthPOT);
    setlength(TD,WidthPOT*HeightPOT+1);

    for i:=1 to HeightPOT do begin
      ci:=0;
      for j:=LeftIndex to RightIndex do
        for k:=1 to RXData[RXid].Size[j,1] do begin
          inc(ci);
          if i<=RXData[RXid].Size[j,2] then
            TD[(i-1)*WidthPOT+ci-1]:=RXData[RXid].Data[j,(i-1)*RXData[RXid].Size[j,1]+k-1]
          else
            TD[(i-1)*WidthPOT+ci-1]:=0;
        end;
    end;

    //If we need to prepare textures for TeamColors
    if MakeTeamColors and RXData[RXid].NeedTeamColors and (not ((RXid=4)and InRange(49,LeftIndex,RightIndex))) then
    begin
      GFXData[RXid,LeftIndex].TexID := GenTexture(WidthPOT,HeightPOT,@TD[0],tm_TexID);
      //TeamColors are done through alternative plain colored texture
      for i:=0 to length(TD)-1 do
        if TD[i] in [24..30] then begin //Determine if TD needs alt color
          GFXData[RXid,LeftIndex].AltID := GenTexture(WidthPOT,HeightPOT,@TD[0],tm_AltID);
          inc(ColorsRAM,WidthPOT*HeightPOT*4);
          break;
        end;
    end
    else
      if RXid=5 then
        if RX5Pal[LeftIndex]<>0 then
          GFXData[RXid,LeftIndex].TexID := GenTexture(WidthPOT,HeightPOT,@TD[0],tm_NoCol,RX5Pal[LeftIndex])
        else
          GFXData[RXid,LeftIndex].TexID := GenTexture(WidthPOT,HeightPOT,@TD[0],tm_NoCol,10)
      else
      if RXid=6 then
        if RX6Pal[LeftIndex]<>0 then
          GFXData[RXid,LeftIndex].TexID := GenTexture(WidthPOT,HeightPOT,@TD[0],tm_NoCol,RX6Pal[LeftIndex])
        else
          GFXData[RXid,LeftIndex].TexID := GenTexture(WidthPOT,HeightPOT,@TD[0],tm_NoCol,10)
      else
        GFXData[RXid,LeftIndex].TexID := GenTexture(WidthPOT,HeightPOT,@TD[0],tm_NoCol);

    setlength(TD,0);

    k:=0;
    for j:=LeftIndex to RightIndex do begin //Hack to test AlphaTest
      GFXData[RXid,j].TexID:=GFXData[RXid,LeftIndex].TexID;
      GFXData[RXid,j].AltID:=GFXData[RXid,LeftIndex].AltID;
      GFXData[RXid,j].u1:=k/WidthPOT;
      GFXData[RXid,j].v1:=0;
      inc(k,RXData[RXid].Size[j,1]);
      GFXData[RXid,j].u2:=k/WidthPOT;
      GFXData[RXid,j].v2:=RXData[RXid].Size[j,2]/HeightPOT;
      GFXData[RXid,j].PxWidth:=RXData[RXid].Size[j,1];
      GFXData[RXid,j].PxHeight:=RXData[RXid].Size[j,2];

      //setlength(RXData[RXid].Data[j],0); //Do not erase since we need it for AlphaTest
      inc(RequiredRAM,RXData[RXid].Size[j,1]*RXData[RXid].Size[j,2]*4);
    end;

    inc(AllocatedRAM,WidthPOT*HeightPOT*4);
    inc(LeftIndex,SpanCount-1);
    inc(TexCount);

  until(LeftIndex>=RXData[RXid].Qty); // >= in case data wasn't loaded and Qty=0

  fLog.AppendLog(inttostr(TexCount)+' Textures created');
  fLog.AddToLog(inttostr(AllocatedRAM div 1024)+'/'+inttostr((AllocatedRAM-RequiredRAM) div 1024)+' Kbytes allocated/wasted for units GFX when using Packing');
  fLog.AddToLog(inttostr(ColorsRAM div 1024)+' KBytes for team colors');
end;


//=============================================
//Export RX to Bitmaps
//=============================================
{That is when we want to export RX to Bitmaps without need to have GraphicsEditor, also this way we preserve image indexes}
procedure ExportRX2BMP(RXid:integer);
var MyBitMap:TBitMap;
    id,t:integer;
    sy,sx,y,x:integer;
    UsePal:integer;
begin
  CreateDir(ExeDir+'Export\');
  CreateDir(ExeDir+'Export\'+RXData[RXid].Title+'.rx\');
  MyBitMap:=TBitMap.Create;
  MyBitmap.PixelFormat:=pf24bit;

  fResource.LoadRX(ExeDir+'data\gfx\res\'+RXData[RXid].Title+'.rx',RXid);

  for id:=1 to RXData[RXid].Qty do begin

    sx:=RXData[RXid].Size[id,1];
    sy:=RXData[RXid].Size[id,2];
    MyBitmap.Width:=sx;
    MyBitmap.Height:=sy;

    UsePal:=DEF_PAL;
    if RXid=5 then UsePal:=RX5Pal[id];
    if RXid=6 then UsePal:=RX6Pal[id];
    if UsePal=0 then UsePal:=10;

    for y:=0 to sy-1 do for x:=0 to sx-1 do begin
      t:=RXData[RXid].Data[id,y*sx+x]+1;
      MyBitmap.Canvas.Pixels[x,y]:=Pal[UsePal,t,1]+Pal[UsePal,t,2]*256+Pal[UsePal,t,3]*65536;
    end;
    if sy>0 then MyBitmap.SaveToFile(ExeDir+'Export\'+RXData[RXid].Title+'.rx\'+RXData[RXid].Title+'_'+int2fix(id,4)+'.bmp');

    setlength(RXData[RXid].Data[id],0);
  end;

  MyBitMap.Free;
end;

{Export Units graphics categorized by Unit and Action}
procedure ExportUnitAnim2BMP();
var MyBitMap:TBitMap;
    iUnit,iAct,iDir,iFrame,ci,t:integer;
    sy,sx,y,x:integer;
    Used:array of integer;
begin
  CreateDir(ExeDir+'Export\');
  CreateDir(ExeDir+'Export\UnitAnim\');
  MyBitMap:=TBitMap.Create;
  MyBitmap.PixelFormat:=pf24bit;

  fResource.LoadUnitDAT(ExeDir+'data\defines\unit.dat');
  fResource.LoadRX(ExeDir+'data\gfx\res\'+RXData[3].Title+'.rx',3);

  ci:=0;
  for iUnit:=15 to 15 do begin
    for iAct:=1 to 14 do begin
      for iDir:=1 to 8 do if UnitSprite[iUnit].Act[iAct].Dir[iDir].Step[1]<>-1 then begin
        for iFrame:=1 to UnitSprite[iUnit].Act[iAct].Dir[iDir].Count do begin
          CreateDir(ExeDir+'Export\UnitAnim\'+TypeToString(TUnitType(iUnit))+'\');
          CreateDir(ExeDir+'Export\UnitAnim\'+TypeToString(TUnitType(iUnit))+'\'+UnitAct[iAct]+'\');
          if UnitSprite[iUnit].Act[iAct].Dir[iDir].Step[iFrame]+1<>0 then
          ci:=UnitSprite[iUnit].Act[iAct].Dir[iDir].Step[iFrame]+1;

          sx:=RXData[3].Size[ci,1];
          sy:=RXData[3].Size[ci,2];
          MyBitmap.Width:=sx;
          MyBitmap.Height:=sy;

          for y:=0 to sy-1 do for x:=0 to sx-1 do begin
            t:=RXData[3].Data[ci,y*sx+x]+1;
            MyBitmap.Canvas.Pixels[x,y]:=Pal[DEF_PAL,t,1]+Pal[DEF_PAL,t,2]*256+Pal[DEF_PAL,t,3]*65536;
          end;
          if sy>0 then MyBitmap.SaveToFile(
          ExeDir+'Export\UnitAnim\'+TypeToString(TUnitType(iUnit))+'\'+UnitAct[iAct]+'\'+inttostr(iDir)+'_'+int2fix(iFrame,2)+'.bmp');
        end;
      end;
    end;
  end;

  CreateDir(ExeDir+'Export\UnitAnim\_TheRest');
  setlength(Used,length(RXData[3].Size));
  for iUnit:=1 to 41 do
  for iAct:=1 to 14 do
  for iDir:=1 to 8 do if UnitSprite[iUnit].Act[iAct].Dir[iDir].Step[1]<>-1 then
  for iFrame:=1 to UnitSprite[iUnit].Act[iAct].Dir[iDir].Count do
  if UnitSprite[iUnit].Act[iAct].Dir[iDir].Step[iFrame]+1<>0 then
  Used[UnitSprite[iUnit].Act[iAct].Dir[iDir].Step[iFrame]+1]:=1;

  for iUnit:=1 to 28 do
  for iDir:=1 to 8 do if SerfCarry[iUnit].Dir[iDir].Step[1]<>-1 then
  for iFrame:=1 to SerfCarry[iUnit].Dir[iDir].Count do
  if SerfCarry[iUnit].Dir[iDir].Step[iFrame]+1<>0 then
  Used[SerfCarry[iUnit].Dir[iDir].Step[iFrame]+1]:=1;

  for ci:=1 to length(Used)-1 do
  if Used[ci]=0 then begin
    sx:=RXData[3].Size[ci,1];
    sy:=RXData[3].Size[ci,2];
    MyBitmap.Width:=sx;
    MyBitmap.Height:=sy;

    for y:=0 to sy-1 do for x:=0 to sx-1 do begin
      t:=RXData[3].Data[ci,y*sx+x]+1;
      MyBitmap.Canvas.Pixels[x,y]:=Pal[DEF_PAL,t,1]+Pal[DEF_PAL,t,2]*256+Pal[DEF_PAL,t,3]*65536;
    end;
    if sy>0 then MyBitmap.SaveToFile(
    ExeDir+'Export\UnitAnim\_TheRest\'+'_'+int2fix(ci,4)+'.bmp');
  end;

  MyBitmap.Free;
end;


{Export Houses graphics categorized by House and Action}
procedure ExportHouseAnim2BMP();
var MyBitMap:TBitMap;
    Q,ID,Ac,k,ci,t:integer;
    sy,sx,y,x:integer;
    s:string;
begin
  CreateDir(ExeDir+'Export\');
  CreateDir(ExeDir+'Export\HouseAnim\');
  MyBitMap:=TBitMap.Create;
  MyBitmap.PixelFormat:=pf24bit;

  fResource.LoadHouseDAT(ExeDir+'data\defines\houses.dat');
  fResource.LoadRX(ExeDir+'data\gfx\res\'+RXData[2].Title+'.rx',2);

  ci:=0;
  for ID:=1 to HOUSE_COUNT do begin
    for Ac:=1 to 5 do begin //Work1..Work5
      for k:=1 to HouseDAT[ID].Anim[Ac].Count do begin
        CreateDir(ExeDir+'Export\HouseAnim\'+TypeToString(THouseType(ID))+'\');
        CreateDir(ExeDir+'Export\HouseAnim\'+TypeToString(THouseType(ID))+'\Work'+IntToStr(Ac)+'\');
        if HouseDAT[ID].Anim[Ac].Step[k]+1<>0 then
        ci:=HouseDAT[ID].Anim[Ac].Step[k]+1;

        sx:=RXData[2].Size[ci,1];
        sy:=RXData[2].Size[ci,2];
        MyBitmap.Width:=sx;
        MyBitmap.Height:=sy;

        for y:=0 to sy-1 do for x:=0 to sx-1 do begin
          t:=RXData[2].Data[ci,y*sx+x]+1;
          MyBitmap.Canvas.Pixels[x,y]:=Pal[DEF_PAL,t,1]+Pal[DEF_PAL,t,2]*256+Pal[DEF_PAL,t,3]*65536;
        end;
        if sy>0 then MyBitmap.SaveToFile(
        ExeDir+'Export\HouseAnim\'+TypeToString(THouseType(ID))+'\Work'+IntToStr(Ac)+'\_'+int2fix(k,2)+'.bmp');
      end;
    end;
  end;

  ci:=0;
  for Q:=1 to 2 do begin
    if Q=1 then s:='_Swine';
    if Q=2 then s:='_Stables';
    CreateDir(ExeDir+'Export\HouseAnim\'+s+'\');
    for ID:=1 to 5 do begin
      for Ac:=1 to 3 do begin //Age 1..3
        for k:=1 to HouseDATs[Q,ID,Ac].Count do begin
          CreateDir(ExeDir+'Export\HouseAnim\'+s+'\'+int2fix(ID,2)+'\');
          if HouseDATs[Q,ID,Ac].Step[k]+1<>0 then
          ci:=HouseDATs[Q,ID,Ac].Step[k]+1;

          sx:=RXData[2].Size[ci,1];
          sy:=RXData[2].Size[ci,2];
          MyBitmap.Width:=sx;
          MyBitmap.Height:=sy;

          for y:=0 to sy-1 do for x:=0 to sx-1 do begin
            t:=RXData[2].Data[ci,y*sx+x]+1;
            MyBitmap.Canvas.Pixels[x,y]:=Pal[DEF_PAL,t,1]+Pal[DEF_PAL,t,2]*256+Pal[DEF_PAL,t,3]*65536;
          end;
          if sy>0 then MyBitmap.SaveToFile(ExeDir+'Export\HouseAnim\'+s+'\'+int2fix(ID,2)+'\_'+int2fix(Ac,1)+'_'+int2fix(k,2)+'.bmp');
        end;
      end;
    end;
  end;

  MyBitmap.Free;
end;


{Export Trees graphics categorized by ID}
procedure ExportTreeAnim2BMP();
var MyBitMap:TBitMap;
    ID,k,ci,t:integer;
    sy,sx,y,x:integer;
begin
  CreateDir(ExeDir+'Export\');
  CreateDir(ExeDir+'Export\TreeAnim\');
  MyBitMap:=TBitMap.Create;
  MyBitmap.PixelFormat:=pf24bit;

  fResource.LoadMapElemDAT(ExeDir+'data\defines\mapelem.dat');
  fResource.LoadRX(ExeDir+'data\gfx\res\'+RXData[1].Title+'.rx',1);

  ci:=0;
  for ID:=1 to MapElemQty do begin
    for k:=1 to MapElem[ID].Count do begin
      if MapElem[ID].Step[k]+1<>0 then
      ci:=MapElem[ID].Step[k]+1;

      sx:=RXData[1].Size[ci,1];
      sy:=RXData[1].Size[ci,2];
      MyBitmap.Width:=sx;
      MyBitmap.Height:=sy;

      for y:=0 to sy-1 do for x:=0 to sx-1 do begin
        t:=RXData[1].Data[ci,y*sx+x]+1;
        MyBitmap.Canvas.Pixels[x,y]:=Pal[DEF_PAL,t,1]+Pal[DEF_PAL,t,2]*256+Pal[DEF_PAL,t,3]*65536;
      end;
      if sy>0 then MyBitmap.SaveToFile(
      //@Lewin: insert field here and press Export>TreeAnim. Rename each folder after export to 'Cuttable',
      //'Quad' and etc.. there you'll have it. Note, we use 1..254 counting, JBSnorro uses 0..253 counting
      ExeDir+'Export\TreeAnim\'+{inttostr(word(MapElem[ID].DiagonalBlocked))+'_'+}int2fix(ID,3)+'_'+int2fix(k,2)+'.bmp');
    end;
  end;

  MyBitmap.Free;
end;

{Tile textures aren't always the same, e.g. if someone makes a mod they will be different,
thus it's better to spend few ms and generate minimap colors from actual data}
procedure TResource.MakeMiniMapColors(FileName:string);
var ii,kk,h,j,px:integer; c:array of byte; R,G,B,SizeX,SizeY:integer; f:file;
{$IFDEF VER140}
var
  InputStream: TFileStream;
  OutputStream: TMemoryStream;
  DeCompressionStream: TZDecompressionStream;
{$ENDIF}
begin
  assignfile(f,FileName);
  FileMode:=0; Reset(f,1); FileMode:=2; //Open ReadOnly

  setlength(c,18+1);
  blockread(f,c[1],18); //SizeOf(TGAHeader)
  SizeX := c[13]+c[14]*256;
  SizeY := c[15]+c[16]*256;

  if c[1]=120 then
  begin
    {$IFDEF VER140}
    closefile(f);
    InputStream := TFileStream.Create(FileName, fmOpenRead);
    OutputStream := TMemoryStream.Create;
    DecompressionStream := TZDecompressionStream.Create(InputStream);
    OutputStream.CopyFrom(DecompressionStream, 0);
    OutputStream.Position := 0; //SizeOf(TGAHeader)
    OutputStream.ReadBuffer(c[1], 18);
    SizeX := c[13]+c[14]*256;
    SizeY := c[15]+c[16]*256;
    setlength(c,SizeX*SizeY*4+1);
    OutputStream.ReadBuffer(c[1], SizeX*SizeY*4);
    InputStream.Free;
    OutputStream.Free;
    DeCompressionStream.Free;
    {$ENDIF};
  end
  else
  begin
    setlength(c,SizeX*SizeY*4+1);
    blockread(f,c[1],SizeX*SizeY*4);
    closefile(f);
  end;

  for ii:=0 to 15 do for kk:=0 to 15 do begin

    R:=0; G:=0; B:=0;

    for j:=0 to (SizeY div 16 - 1) do
    for h:=0 to (SizeX div 16 - 1) do
    begin
      px := (((SizeX-1)-(ii*(SizeY div 16)+j))*SizeX+kk*(SizeX div 16)+h)*4; //TGA comes flipped upside down
      inc(B, c[px+1]);
      inc(G, c[px+2]);
      inc(R, c[px+3]);
    end;

    px := ii*16+kk+1;

    TileMMColor[px].R := round(R / (SizeY*SizeY div 256)) ; //each tile is 32x32 px
    TileMMColor[px].G := round(G / (SizeY*SizeY div 256)) ;
    TileMMColor[px].B := round(B / (SizeY*SizeY div 256)) ;

  end;
end;


procedure TResource.MakeCursors(RXid:integer);
var
  i,sx,sy,x,y,t:integer;
  bm,bm2:TBitmap;
  IconInfo:TIconInfo;
begin
  bm:=TBitmap.Create;  bm.PixelFormat:=pf32bit;
  bm2:=TBitmap.Create; bm2.PixelFormat:=pf32bit;

  for i:=1 to length(Cursors) do begin

    sx:=RXData[RXid].Size[Cursors[i],1];
    sy:=RXData[RXid].Size[Cursors[i],2];
    bm.Width:=sx; bm.Height:=sy;
    bm2.Width:=sx; bm2.Height:=sy;

    for y:=0 to sy-1 do for x:=0 to sx-1 do begin
      t:=RXData[RXid].Data[Cursors[i],y*sx+x]+1;
      bm.Canvas.Pixels[x,y]:=Pal[DEF_PAL,t,1]+Pal[DEF_PAL,t,2] shl 8 + Pal[DEF_PAL,t,3] shl 16;
      if t=1 then
        bm2.Canvas.Pixels[x,y]:=clWhite
      else
        bm2.Canvas.Pixels[x,y]:=clBlack;
    end;

  IconInfo.fIcon:=false;
  //Load hotspot offsets from RX file, adding the manual offsets (normally 0)
  IconInfo.xHotspot:=-RXData[RXid].Pivot[Cursors[i]].x+CursorOffsetsX[i];
  IconInfo.yHotspot:=-RXData[RXid].Pivot[Cursors[i]].y+CursorOffsetsY[i];
  IconInfo.hbmColor:=bm.Handle;
  IconInfo.hbmMask:=bm2.Handle;

  Screen.Cursors[Cursors[i]]:=CreateIconIndirect(iconInfo);
  end;

  bm.Free;
  bm2.Free;
  Screen.Cursor:=c_Default;
end;





end.
