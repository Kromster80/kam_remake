unit KM_ResourceGFX;
{$I KaM_Remake.inc}
interface
uses
  {$IFDEF WDC} OpenGL, {$ENDIF}
  {$IFDEF FPC} GL, {$ENDIF}
  PNGImage,
  Windows, Forms, Graphics, SysUtils, Math, dglOpenGL, KM_Defaults, KM_LoadLib, Classes
  {$IFDEF WDC}, ZLibEx {$ENDIF}
  {$IFDEF FPC}, PasZLib {$ENDIF};

type
  TCardinalArray2 = array of Cardinal;
  TexMode = (tm_TexID, tm_AltID, tm_AlphaTest); //Defines way to decode sprites using palette info
  TDataLoadingState = (dls_None, dls_Menu, dls_All); //Resources are loaded in 2 steps, for menu and the rest



type
  TResource = class
  private
    fDataState:TDataLoadingState;

    procedure StepRefresh();
    procedure StepCaption(aCaption:string);

    function LoadPalettes():boolean;
    function LoadMapElemDAT(filename:string):boolean;
    function LoadPatternDAT(filename:string):boolean;
    function LoadHouseDAT(filename:string):boolean;
    function LoadUnitDAT(filename:string):boolean;
    function LoadFont(filename:string; aFont:TKMFont; WriteFontToBMP:boolean):boolean;

    function LoadRX(filename:string; ID:integer):boolean;
    procedure LoadRX7(aID:integer);
    procedure ExpandRX(ID:integer);
    procedure MakeGFX(RXid:integer);
    procedure MakeGFX_AlphaTest(RXid:integer);

    procedure ClearUnusedGFX(RXid:integer);

    procedure MakeMiniMapColors(FileName:string);
    procedure MakeCursors(RXid:integer);

    function GenTexture(DestX, DestY:word; const Data:TCardinalArray2; Mode:TexMode):gluint; //This should belong to TRender?
  public
    constructor Create;
    destructor Destroy; override;
    function LoadMenuResources(aLocale:string):boolean;
    function LoadGameResources():boolean;

    function GetColor32(aIndex:byte; aPal:TKMPal=DEF_PAL):cardinal;

    property GetDataState:TDataLoadingState read fDataState;
    function GetUnitSequenceLength(aUnitType:TUnitType; aAction:TUnitActionType; aDir:TKMDirection):smallint;

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
  fDataState := dls_None;
  fLog.AppendLog('Resource loading state - None');
end;


destructor TResource.Destroy;
begin
  //todo: Add destroy with "DestroyIcon" and move cursor creation to Create here
  Inherited;
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
  LoadPalettes();
  fLog.AppendLog('Reading palettes',true);

  RXData[1].Title:='Trees';       RXData[1].NeedTeamColors:=false;
  RXData[2].Title:='Houses';      RXData[2].NeedTeamColors:=true;
  RXData[3].Title:='Units';       RXData[3].NeedTeamColors:=true;
  RXData[4].Title:='GUI';         RXData[4].NeedTeamColors:=true; //Required for unit scrolls and stat icons
  RXData[5].Title:='GUIMain';     RXData[5].NeedTeamColors:=false;
  RXData[6].Title:='GUIMainH';    RXData[6].NeedTeamColors:=false;

  for i:=4 to 6 do
  begin
    StepCaption('Reading '+RXData[i].Title+' GFX ...');
    fLog.AppendLog('Reading '+RXData[i].Title+'.rx',LoadRX(ExeDir+'data\gfx\res\'+RXData[i].Title+'.rx',i));

    LoadRX7(i); //Something fancy to Load RX7 data (custom bitmaps and overrides)

    if i=4 then MakeCursors(4);
    MakeGFX(i);
    ClearUnusedGFX(i);
    
    StepRefresh();
  end;

  StepCaption('Reading fonts ...');
  LoadFonts(false, aLocale);
  fLog.AppendLog('Read fonts is done');

  StepRefresh();
  fLog.AppendLog('ReadGFX is done');
  fDataState:=dls_Menu;
  fLog.AppendLog('Resource loading state - Menu');
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
    if (i=1) or ((i=2) and MAKE_HOUSE_SPRITES) or ((i=3) and MAKE_UNIT_SPRITES) then
    begin
      StepCaption('Reading '+RXData[i].Title+' GFX ...');
      fLog.AppendLog('Reading '+RXData[i].Title+'.rx',LoadRX(ExeDir+'data\gfx\res\'+RXData[i].Title+'.rx',i));
      LoadRX7(i); //Updated sprites
      MakeGFX(i);
      //Alpha_tested sprites for houses. They come after MakeGFX cos they will
      //replace above data. 
      if i=2 then MakeGFX_AlphaTest(i);
      ClearUnusedGFX(i);
      StepRefresh();
    end;

  StepCaption('Making minimap colors ...');
  MakeMiniMapColors(ExeDir+'Resource\Tiles1.tga');
  fLog.AppendLog('Prepared MiniMap colors...');
  StepRefresh();
  fDataState:=dls_All;
  fLog.AppendLog('Resource loading state - Game');
  Result:=true;
end;


procedure TResource.LoadFonts(DoExport:boolean; aLocale:string);
var i:TKMFont;
begin
  for i:=low(TKMFont) to high(TKMFont) do
  if FileExists(ExeDir+'data\gfx\fonts\'+FontFiles[i]+'.'+aLocale+'.fnt') then
    LoadFont(ExeDir+'data\gfx\fonts\'+FontFiles[i]+'.'+aLocale+'.fnt', TKMFont(i), DoExport)
  else
    LoadFont(ExeDir+'data\gfx\fonts\'+FontFiles[i]+'.fnt', TKMFont(i), DoExport);
end;


function TResource.GetColor32(aIndex:byte; aPal:TKMPal=DEF_PAL):cardinal;
begin
  Result := Pal[aPal,aIndex,1] + Pal[aPal,aIndex,2] shl 8 + Pal[aPal,aIndex,3] shl 16 OR $FF000000;
end;


function TResource.GetUnitSequenceLength(aUnitType:TUnitType; aAction:TUnitActionType; aDir:TKMDirection):smallint;
begin
  Result := UnitSprite[Integer(aUnitType)].Act[Integer(aAction)].Dir[Integer(aDir)].Count;
end;



//=============================================
//Reading Palette for trees/objects
//=============================================
function TResource.LoadPalettes():boolean;
var f:file; i:TKMPal; k:integer; FileName:string;
begin
  Result := true;

  for i:=low(TKMPal) to high(TKMPal) do begin

    FileName := ExeDir+'data\gfx\'+PalFiles[i];
    if FileExists(FileName) then begin
      assignfile(f,FileName);
      reset(f,1);
      blockread(f,Pal[i],48); //Unknown and/or not important
      blockread(f,Pal[i],768); //256*3
      closefile(f);

      if i = pal_lin then //Make greyscale linear Pal
        for k:=0 to 255 do begin
          Pal[pal_lin,k,1] := k;
          Pal[pal_lin,k,2] := k;
          Pal[pal_lin,k,3] := k;
        end;
    end else
      Result := false;
  end;
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
      end;}
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
      for kk:=1 to 12 do write(ft,inttostr(HouseDAT[ii].Foot1[kk])+';');
      for kk:=1 to 12 do write(ft,inttostr(HouseDAT[ii].Foot2[kk])+';');
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

  for ii:=1 to 28 do
    blockread(f,SerfCarry[ii],8*70);

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
      write(ft,inttostr(UnitStat[ii].HitPoints)+';');
      write(ft,inttostr(UnitStat[ii].Attack)+';');
      write(ft,inttostr(UnitStat[ii].AttackHorseBonus)+';');
      write(ft,inttostr(UnitStat[ii].x4)+';');
      write(ft,inttostr(UnitStat[ii].Defence)+';');
      write(ft,inttostr(UnitStat[ii].Speed)+';');
      write(ft,inttostr(UnitStat[ii].x7)+';');
      write(ft,inttostr(UnitStat[ii].Sight)+';');
      write(ft,inttostr(UnitStat[ii].x9)+';');
      write(ft,inttostr(UnitStat[ii].x10)+';');
      write(ft,inttostr(UnitStat[ii].CanWalkOut)+';');
      write(ft,inttostr(UnitStat[ii].x11)+';');
      for kk:=1 to 18 do
        write(ft,inttostr(UnitSprite2[ii,kk])+';');
      writeln(ft);
    end;
    closefile(ft);

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
  L:byte;
  i,k,ci,ck:integer;
  MaxHeight:integer;
  AdvX,AdvY:integer;
  TD:array of cardinal;
  MyBitMap:TBitMap;
begin
  Result:=false;
  MaxHeight:=0;
  if not CheckFileExists(filename, true) then exit;

  assignfile(f,filename); reset(f,1);
  blockread(f,FontData[aFont].Unk1,8);
  blockread(f,FontData[aFont].Pal[0],256);

  //Read font data
  for i:=0 to 255 do
    if FontData[aFont].Pal[i]<>0 then
      with FontData[aFont].Letters[i] do begin
        blockread(f,Width,4);
        blockread(f,Add1,8);
        MaxHeight:=Math.max(MaxHeight,Height);
        fLog.AssertToLog(Width*Height<>0,'Font data Width*Height <> 0'); //Font01.fnt seems to be damaged..
        blockread(f,Data[1],Width*Height);
      end;
  closefile(f);

  //Special fixes: for monochrome fonts
  if FontPal[aFont]=pal_lin then
  for i:=0 to 255 do
    if FontData[aFont].Pal[i]<>0 then //see if letterspace is used
      for k:=1 to 4096 do
        if FontData[aFont].Letters[i].Data[k]<>0 then
          FontData[aFont].Letters[i].Data[k]:=255; //Full white


  //Compile texture
  AdvX:=0; AdvY:=0;
  setlength(TD,TexWidth*TexWidth);

  for i:=0 to 255 do
    if FontData[aFont].Pal[i]<>0 then
      with FontData[aFont].Letters[i] do begin

      fLog.AssertToLog(FontData[aFont].Pal[i]=1,'FontData palette <> 1');

        if AdvX+Width+2>TexWidth then begin
          AdvX:=0;
          inc(AdvY,MaxHeight);
        end;

        for ci:=1 to Height do for ck:=1 to Width do begin
          L := Data[(ci-1)*Width+ck]; //0..255
          if L<>0 then //Transparent
            TD[(AdvY+ci-1)*TexWidth+AdvX+1+ck-1]:=GetColor32(L,FontPal[aFont]);
        end;

        u1:=(AdvX+1)/TexWidth;
        v1:=AdvY/TexWidth;
        u2:=(AdvX+1+Width)/TexWidth;
        v2:=(AdvY+Height)/TexWidth;

        inc(AdvX,1+Width+1);
      end;

    FontData[aFont].TexID := GenTexture(TexWidth,TexWidth,@TD[0],tm_TexID);

  //for i:=1 to 10 do
  if WriteFontToBMP then begin
    MyBitMap:=TBitMap.Create;
    MyBitMap.PixelFormat:=pf24bit;
    MyBitMap.Width:=TexWidth;
    MyBitMap.Height:=TexWidth;

    for ci:=0 to TexWidth-1 do for ck:=0 to TexWidth-1 do begin
      MyBitMap.Canvas.Pixels[ck,ci]:= TD[ci*TexWidth+ck] AND $FFFFFF;
    end;

    CreateDir(ExeDir+'Export\');
    CreateDir(ExeDir+'Export\Fonts\');
    MyBitMap.SaveToFile(ExeDir+'Export\Fonts\'+ExtractFileName(filename)+PalFiles[FontPal[aFont]]+'.bmp');
    MyBitMap.Free;
  end;

  setlength(TD,0);
  Result:=true;

end;


{ This function should parse all valid files in Sprites folder and load them
  additionaly to or replacing original sprites }
procedure TResource.LoadRX7(aID:integer);
var
  FileList:TStringList;
  SearchRec:TSearchRec;
  i:integer; x,y:integer;
  RX,ID:integer; p:cardinal;
  po:TPNGObject;
begin

  if not DirectoryExists(ExeDir + 'Sprites\') then exit;

  FileList := TStringList.Create;
  ChDir(ExeDir + 'Sprites\');
  FindFirst('*', faAnyFile, SearchRec);
  repeat
    if (SearchRec.Name<>'.')and(SearchRec.Name<>'..') then //Exclude parent folders
    if SearchRec.Attr and faDirectory <> faDirectory then
    if GetFileExt(SearchRec.Name) = 'PNG' then
      FileList.Add(SearchRec.Name);
  until (FindNext(SearchRec)<>0);
  FindClose(SearchRec);

  //#-####.png - default texture
  //#-####a.png - alternative texture
  //todo:  Support alternative textures
  for i:=0 to FileList.Count-1 do begin

    RX := StrToIntDef(FileList.Strings[i][1],0); //wrong file will return 0
    ID := StrToIntDef(Copy(FileList.Strings[i], 3, 4),0); //wrong file will return 0
    if (RX=aID) and InRange(ID,1,RXData[RX].Qty) then begin //Replace only certain sprites

      po := TPNGObject.Create;
      po.LoadFromFile(ExeDir + 'Sprites\' + FileList.Strings[i]);

      RXData[RX].Size[ID].X := po.Width;
      RXData[RX].Size[ID].Y := po.Height;

      setlength(RXData[RX].RGBA[ID], po.Width*po.Height);
      setlength(RXData[RX].Mask[ID], po.Width*po.Height); //Should allocate space for it's always comes along

      for y:=0 to po.Height-1 do begin
        po.CreateAlpha; //Will create white Alpha if it does not exists

        for x:=0 to po.Width-1 do begin
          p := (po.AlphaScanline[y]^[x]) shl 24; //it's slow, I know, but we don't care yet
          RXData[RX].RGBA[ID, y*po.Width+x] := cardinal(po.Pixels[x,y] AND $FFFFFF) + p;
        end;
      end;
      po.Free;
    end;
  end;

  FileList.Free;
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

  setlength(GFXData[ID],      RXData[ID].Qty + 1);
  setlength(RXData[ID].Flag,  RXData[ID].Qty + 1);
  setlength(RXData[ID].Size,  RXData[ID].Qty + 1);
  setlength(RXData[ID].Pivot, RXData[ID].Qty + 1);
  setlength(RXData[ID].Data,  RXData[ID].Qty + 1);
  setlength(RXData[ID].RGBA,  RXData[ID].Qty + 1);
  setlength(RXData[ID].Mask,  RXData[ID].Qty + 1);

  blockread(f, RXData[ID].Flag[1], RXData[ID].Qty);

  if (not LOAD_UNIT_RX_FULL)and(RXData[ID].Title = 'Units') then RXData[ID].Qty:=7885;

  for i:=1 to RXData[ID].Qty do
    if RXData[ID].Flag[i] = 1 then
    begin
      blockread(f, RXData[ID].Size[i].X, 4);
      blockread(f, RXData[ID].Pivot[i].x, 8);
      setlength(RXData[ID].Data[i], RXData[ID].Size[i].X * RXData[ID].Size[i].Y);
      blockread(f, RXData[ID].Data[i,0], RXData[ID].Size[i].X * RXData[ID].Size[i].Y);
    end;

  closefile(f);
  fLog.AppendLog(RXData[ID].Title+' -',RXData[ID].Qty);
  Result:=true;

  ExpandRX(ID);
end;


//=============================================
// Expand RX Data
//Convert paletted data into RGBA and select Team color layer from it
//=============================================
procedure TResource.ExpandRX(ID:integer);
var i:integer; x,y:integer; Palette:TKMPal; L:byte; Pixel:integer;
begin
  with RXData[ID] do
  for i:=1 to Qty do begin

    case ID of
      5: Palette := RX5Pal[i];
      6: Palette := RX6Pal[i];
      else Palette := DEF_PAL;
    end;

    if Flag[i] = 1 then begin
      setlength(RGBA[i], Size[i].X*Size[i].Y);
      setlength(Mask[i], Size[i].X*Size[i].Y);

      for y:=0 to Size[i].Y-1 do for x:=0 to Size[i].X-1 do
      begin
        Pixel := y*Size[i].X+x;
        L := Data[i, Pixel]; //0..255

        if L<>0 then
          if NeedTeamColors and (L in[23..29]) then
            RGBA[i,Pixel] := cardinal((byte(L-26)*42+128)*65793) OR $FF000000
          else
            RGBA[i,Pixel] := GetColor32(L, Palette);

        case L of //Maybe it makes sense to convert to 8bit?
          23,29:  Mask[i,Pixel] := $60FFFFFF;   //7  //6
          24,28:  Mask[i,Pixel] := $90FFFFFF;   //11 //9
          25,27:  Mask[i,Pixel] := $C0FFFFFF;   //14 //12
          26:     Mask[i,Pixel] := $FFFFFFFF;   //16 //16
        end;
      end;
   end;
  end;
end;


//=============================================
//Make texture
//=============================================
function TResource.GenTexture(DestX, DestY:word; const Data:TCardinalArray2; Mode:TexMode):gluint;
var
  MyBitMap:TBitMap;
  i,k:word;
begin

  Result := 0;

  DestX := MakePOT(DestX);
  DestY := MakePOT(DestY);
  if DestX*DestY = 0 then exit; //Do not generate zeroed textures

  Result := GenerateTextureCommon; //Should be called prior to glTexImage2D or gluBuild2DMipmaps

  case Mode of
    tm_AlphaTest: glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA,    DestX, DestY, 0, GL_RGBA, GL_UNSIGNED_BYTE, Data);
    //Base layer
    tm_TexID:     glTexImage2D(GL_TEXTURE_2D, 0, GL_RGB5_A1, DestX, DestY, 0, GL_RGBA, GL_UNSIGNED_BYTE, Data);
    //Team color layer
    tm_AltID:     glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA2,   DestX, DestY, 0, GL_RGBA, GL_UNSIGNED_BYTE, Data);
  end;

  if WriteAllTexturesToBMP then begin
    CreateDir(ExeDir+'Export\GenTextures\');
    MyBitMap:=TBitMap.Create;
    MyBitMap.PixelFormat:=pf24bit;
    MyBitMap.Width:=DestX;
    MyBitMap.Height:=DestY;

    for i:=0 to DestY-1 do for k:=0 to DestX-1 do
      MyBitMap.Canvas.Pixels[k,i] := ((PCardinal(Cardinal(Data)+(i*DestX+k)*4))^) AND $FFFFFF; //Ignore alpha
    MyBitMap.SaveToFile(ExeDir+'Export\GenTextures\'+int2fix(Result,4)+'.bmp');

    if Mode=tm_AlphaTest then begin //these Alphas are worth looking at
      for i:=0 to DestY-1 do for k:=0 to DestX-1 do
        MyBitMap.Canvas.Pixels[k,i] := ((PCardinal(Cardinal(Data)+(i*DestX+k)*4))^) SHR 24 *65793;
      MyBitMap.SaveToFile(ExeDir+'Export\GenTextures\'+int2fix(Result,4)+'a.bmp');
    end;

    MyBitMap.Free;
  end;

end;


//=============================================
//Making OpenGL textures
//=============================================
{Take RX data and make nice textures out of it.
Textures should be POT to improve performance and avoid drivers bugs
In result we have GFXData filled.}
procedure TResource.MakeGFX_AlphaTest(RXid:integer);
var
  ID,ID1,ID2:integer; //RGB and A index
  ci,i,k,h,StepCount:integer;
  t,t2:integer;
  ColorID:byte;
  WidthPOT,HeightPOT:integer;
  TD:array of byte;
begin

  for ID:=1 to HOUSE_COUNT do
    if HouseDAT[ID].StonePic<>-1 then //Exlude House27 which is unused
      for h:=1 to 2 do begin

        if h=1 then begin
          ID1 := HouseDAT[ID].WoodPic+1;
          ID2 := HouseDAT[ID].WoodPal+1;
          StepCount:=HouseDAT[ID].WoodPicSteps;
        end else begin
          ID1 := HouseDAT[ID].StonePic+1;
          ID2 := HouseDAT[ID].StonePal+1;
          StepCount:=HouseDAT[ID].StonePicSteps;
        end;

        WidthPOT  := MakePOT(RXData[RXid].Size[ID1].X);
        HeightPOT := MakePOT(RXData[RXid].Size[ID1].Y);
        setlength(TD, WidthPOT*HeightPOT*4+1);

        for i := 1 to HeightPOT do begin
          ci := -1;

          for k := 1 to RXData[RXid].Size[ID1].X do begin
            inc(ci);
            if i <= RXData[RXid].Size[ID1].Y then begin

              t   := ((i-1)*WidthPOT+ci)*4;
              ColorID := RXData[RXid].Data[ID1,(i-1)*RXData[RXid].Size[ID1].X+k-1]; //0..255
              TD[t+0] := Pal[DEF_PAL, ColorID, 1];
              TD[t+1] := Pal[DEF_PAL, ColorID, 2];
              TD[t+2] := Pal[DEF_PAL, ColorID, 3];
              if ColorID = 0 then
                TD[t+3] := 0
              else
                if i<=RXData[RXid].Size[ID2].Y then
                if k<=RXData[RXid].Size[ID2].X then begin//Cos someimes ID2 is smaller by few pixels
                  t2 := t  + (RXData[RXid].Pivot[ID2].x-RXData[RXid].Pivot[ID1].x)*4; //Shift by pivot, always positive
                  t2 := t2 + (RXData[RXid].Pivot[ID2].y-RXData[RXid].Pivot[ID1].y)*WidthPOT*4; //Shift by pivot, always positive
                  TD[t2+3] := 255 - round(RXData[RXid].Data[ID2,(i-1)*RXData[RXid].Size[ID2].X+k-1]*(255/StepCount));
                end;
            end;
          end;
        end;

        GFXData[RXid,ID1].TexID := GenTexture(WidthPOT,HeightPOT,@TD[0],tm_AlphaTest);
        setlength(TD,0);
        GFXData[RXid,ID1].AltID := 0;
        GFXData[RXid,ID1].u1    := 0;
        GFXData[RXid,ID1].v1    := 0;
        GFXData[RXid,ID1].u2    := RXData[RXid].Size[ID1].X/WidthPOT;
        GFXData[RXid,ID1].v2    := RXData[RXid].Size[ID1].Y/HeightPOT;
        GFXData[RXid,ID1].PxWidth:=RXData[RXid].Size[ID1].X;
        GFXData[RXid,ID1].PxHeight:=RXData[RXid].Size[ID1].Y;
      end;
end;


//=============================================
//Making OpenGL textures
//=============================================
{Take RX data and make nice textures out of it.
Textures should be POT to improve performance and avoid driver bugs
In result we have GFXData filled.}
procedure TResource.MakeGFX(RXid:integer);
var
  ci,j,i,k,LeftIndex,RightIndex,TexCount,SpanCount:integer;
  AllocatedRAM,RequiredRAM,ColorsRAM:integer;
  WidthPOT,HeightPOT:integer;
  TD:array of cardinal;
  TA:array of cardinal;
begin

  LeftIndex:=0; AllocatedRAM:=0; RequiredRAM:=0; ColorsRAM:=0; TexCount:=0;
  repeat
    inc(LeftIndex);

    WidthPOT  := RXData[RXid].Size[LeftIndex].X;
    HeightPOT := MakePOT(RXData[RXid].Size[LeftIndex].Y);
    SpanCount := 1;

    //Pack textures with same POT height into rows to save memory
    //This also means fewer textures for GPU RAM == better performance
    while((LeftIndex+SpanCount<RXData[RXid].Qty)and //Keep packing until end of sprites
          (
            (HeightPOT=MakePOT(RXData[RXid].Size[LeftIndex+SpanCount].Y)) //Pack if HeightPOT matches
        or((HeightPOT>=MakePOT(RXData[RXid].Size[LeftIndex+SpanCount].Y))AND(WidthPOT+RXData[RXid].Size[LeftIndex+SpanCount].X<MakePOT(WidthPOT)))
          )and
          (WidthPOT+RXData[RXid].Size[LeftIndex+SpanCount].X<=MAX_TEX_RESOLUTION)) //Pack until max Tex_Resolution approached
    do begin
      inc(WidthPOT,RXData[RXid].Size[LeftIndex+SpanCount].X);
      if (RXid=5)and(RX5pal[LeftIndex]<>RX5pal[LeftIndex+SpanCount]) then break; //Don't align RX5 images for they use all different palettes
      if (RXid=6)and(RX6pal[LeftIndex]<>RX6pal[LeftIndex+SpanCount]) then break; //Don't align RX6 images for they use all different palettes
      inc(SpanCount);
    end;

    RightIndex := LeftIndex+SpanCount-1;
    WidthPOT := MakePOT(WidthPOT);
    setlength(TD,WidthPOT*HeightPOT+1);
    setlength(TA,WidthPOT*HeightPOT+1);

    for i:=1 to HeightPOT do begin
      ci:=0;
      for j:=LeftIndex to RightIndex do
        for k:=1 to RXData[RXid].Size[j].X do begin
          inc(ci);
          if i<=RXData[RXid].Size[j].Y then begin
            TD[(i-1)*WidthPOT+ci-1] := RXData[RXid].RGBA[j,(i-1)*RXData[RXid].Size[j].X+k-1];
            TA[(i-1)*WidthPOT+ci-1] := RXData[RXid].Mask[j,(i-1)*RXData[RXid].Size[j].X+k-1];
          end else begin
            //TD[(i-1)*WidthPOT+ci-1]:=0;
          end;
        end;
    end;

    //If we need to prepare textures for TeamColors          //special fix for iron mine logo
    if MAKE_TEAM_COLORS and RXData[RXid].NeedTeamColors and (not ((RXid=4)and InRange(49,LeftIndex,RightIndex))) then
    begin
      GFXData[RXid,LeftIndex].TexID := GenTexture(WidthPOT,HeightPOT,@TD[0],tm_TexID);
      //TeamColors are done through alternative plain colored texture
      GFXData[RXid,LeftIndex].AltID := GenTexture(WidthPOT,HeightPOT,@TA[0],tm_AltID);
      inc(ColorsRAM,WidthPOT*HeightPOT*4);
    end
    else
      GFXData[RXid,LeftIndex].TexID := GenTexture(WidthPOT,HeightPOT,@TD[0],tm_TexID);

    setlength(TD,0);
    setlength(TA,0);

    k:=0;
    for j:=LeftIndex to RightIndex do begin //Hack to test AlphaTest
      GFXData[RXid,j].TexID:=GFXData[RXid,LeftIndex].TexID;
      GFXData[RXid,j].AltID:=GFXData[RXid,LeftIndex].AltID;
      GFXData[RXid,j].u1:=k/WidthPOT;
      GFXData[RXid,j].v1:=0;
      inc(k,RXData[RXid].Size[j].X);
      GFXData[RXid,j].u2:=k/WidthPOT;
      GFXData[RXid,j].v2:=RXData[RXid].Size[j].Y/HeightPOT;
      GFXData[RXid,j].PxWidth:=RXData[RXid].Size[j].X;
      GFXData[RXid,j].PxHeight:=RXData[RXid].Size[j].Y;

      inc(RequiredRAM,RXData[RXid].Size[j].X*RXData[RXid].Size[j].Y*4);
    end;

    inc(AllocatedRAM,WidthPOT*HeightPOT*4);
    inc(LeftIndex,SpanCount-1);
    inc(TexCount);

  until(LeftIndex>=RXData[RXid].Qty); // >= in case data wasn't loaded and Qty=0

  fLog.AppendLog(inttostr(TexCount)+' Textures created');
  fLog.AddToLog(inttostr(AllocatedRAM div 1024)+'/'+inttostr((AllocatedRAM-RequiredRAM) div 1024)+' Kbytes allocated/wasted for units GFX when using Packing');
  fLog.AddToLog(inttostr(ColorsRAM div 1024)+' KBytes for team colors');
end;


//Now we can safely dispose of RXData[RXid].Data to save us some more RAM
procedure TResource.ClearUnusedGFX(RXid:integer);
var i:integer;
begin
  for i:=1 to RXData[RXid].Qty do begin
    setlength(RXData[RXid].Data[i],0);
    setlength(RXData[RXid].RGBA[i],0);
    setlength(RXData[RXid].Mask[i],0);
  end;
end;


//=============================================
//Export RX to Bitmaps
//=============================================
{That is when we want to export RX to Bitmaps without need to have GraphicsEditor, also this way we preserve image indexes}
procedure ExportRX2BMP(RXid:integer);
var MyBitMap:TBitMap;
    id:integer; t:byte;
    sy,sx,y,x:integer;
    UsePal:TKMPal;
begin
  CreateDir(ExeDir+'Export\');
  CreateDir(ExeDir+'Export\'+RXData[RXid].Title+'.rx\');
  MyBitMap := TBitMap.Create;
  MyBitMap.PixelFormat := pf24bit;

  fResource.LoadRX(ExeDir+'data\gfx\res\'+RXData[RXid].Title+'.rx',RXid);

  for id:=1 to RXData[RXid].Qty do begin

    sx := RXData[RXid].Size[id].X;
    sy := RXData[RXid].Size[id].Y;
    MyBitMap.Width  := sx;
    MyBitMap.Height := sy;

    case RXid of
      5:   UsePal := RX5Pal[id];
      6:   UsePal := RX6Pal[id];
      else UsePal := DEF_PAL;
    end;

    for y:=0 to sy-1 do for x:=0 to sx-1 do begin
      t := RXData[RXid].Data[id,y*sx+x];
      MyBitMap.Canvas.Pixels[x,y] := fResource.GetColor32(t,UsePal) AND $FFFFFF;
    end;
    if sy>0 then MyBitMap.SaveToFile(ExeDir+'Export\'+RXData[RXid].Title+'.rx\'+RXData[RXid].Title+'_'+int2fix(id,4)+'.bmp');

    setlength(RXData[RXid].Data[id],0);
  end;

  MyBitMap.Free;
end;

{Export Units graphics categorized by Unit and Action}
procedure ExportUnitAnim2BMP();
var MyBitMap:TBitMap;
    iUnit,iAct,iDir,iFrame,ci:integer; t:byte;
    sy,sx,y,x:integer;
    Used:array of integer;
begin
  CreateDir(ExeDir+'Export\');
  CreateDir(ExeDir+'Export\UnitAnim\');
  MyBitMap:=TBitMap.Create;
  MyBitMap.PixelFormat:=pf24bit;

  fResource.LoadUnitDAT(ExeDir+'data\defines\unit.dat');
  fResource.LoadRX(ExeDir+'data\gfx\res\'+RXData[3].Title+'.rx',3);

  ci:=0;
  for iUnit:=byte(ut_Arbaletman) to byte(ut_Arbaletman) do begin
    for iAct:=1 to 14 do begin
      for iDir:=1 to 8 do if UnitSprite[iUnit].Act[iAct].Dir[iDir].Step[1]<>-1 then begin
        for iFrame:=1 to UnitSprite[iUnit].Act[iAct].Dir[iDir].Count do begin
          CreateDir(ExeDir+'Export\UnitAnim\'+TypeToString(TUnitType(iUnit))+'\');
          CreateDir(ExeDir+'Export\UnitAnim\'+TypeToString(TUnitType(iUnit))+'\'+UnitAct[iAct]+'\');
          if UnitSprite[iUnit].Act[iAct].Dir[iDir].Step[iFrame]+1<>0 then
          ci:=UnitSprite[iUnit].Act[iAct].Dir[iDir].Step[iFrame]+1;

          sx:=RXData[3].Size[ci].X;
          sy:=RXData[3].Size[ci].Y;
          MyBitMap.Width:=sx;
          MyBitMap.Height:=sy;

          for y:=0 to sy-1 do for x:=0 to sx-1 do begin
            t:=RXData[3].Data[ci,y*sx+x];
            MyBitMap.Canvas.Pixels[x,y]:=fResource.GetColor32(t,DEF_PAL);
          end;
          if sy>0 then MyBitMap.SaveToFile(
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
    sx:=RXData[3].Size[ci].X;
    sy:=RXData[3].Size[ci].Y;
    MyBitMap.Width:=sx;
    MyBitMap.Height:=sy;

    for y:=0 to sy-1 do for x:=0 to sx-1 do begin
      t:=RXData[3].Data[ci,y*sx+x];
      MyBitMap.Canvas.Pixels[x,y]:=fResource.GetColor32(t,DEF_PAL);
    end;
    if sy>0 then MyBitMap.SaveToFile(
    ExeDir+'Export\UnitAnim\_TheRest\'+'_'+int2fix(ci,4)+'.bmp');
  end;

  MyBitMap.Free;
end;


{Export Houses graphics categorized by House and Action}
procedure ExportHouseAnim2BMP();
var MyBitMap:TBitMap;
    Q,ID,Ac,k,ci:integer; t:byte;
    sy,sx,y,x:integer;
    s:string;
begin
  CreateDir(ExeDir+'Export\');
  CreateDir(ExeDir+'Export\HouseAnim\');
  MyBitMap:=TBitMap.Create;
  MyBitMap.PixelFormat:=pf24bit;

  fResource.LoadHouseDAT(ExeDir+'data\defines\houses.dat');
  fResource.LoadRX(ExeDir+'data\gfx\res\'+RXData[2].Title+'.rx',2);

  ci:=0;
  for ID:=byte(ht_WatchTower) to byte(ht_WatchTower) do begin
    for Ac:=1 to 5 do begin //Work1..Work5
      for k:=1 to HouseDAT[ID].Anim[Ac].Count do begin
        CreateDir(ExeDir+'Export\HouseAnim\'+TypeToString(THouseType(ID))+'\');
        CreateDir(ExeDir+'Export\HouseAnim\'+TypeToString(THouseType(ID))+'\Work'+IntToStr(Ac)+'\');
        if HouseDAT[ID].Anim[Ac].Step[k]+1<>0 then
        ci:=HouseDAT[ID].Anim[Ac].Step[k]+1;

        sx:=RXData[2].Size[ci].X;
        sy:=RXData[2].Size[ci].Y;
        MyBitMap.Width:=sx;
        MyBitMap.Height:=sy;

        for y:=0 to sy-1 do for x:=0 to sx-1 do begin
          t:=RXData[2].Data[ci,y*sx+x];
          MyBitMap.Canvas.Pixels[x,y]:=fResource.GetColor32(t,DEF_PAL);
        end;
        if sy>0 then MyBitMap.SaveToFile(
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

          sx:=RXData[2].Size[ci].X;
          sy:=RXData[2].Size[ci].Y;
          MyBitMap.Width:=sx;
          MyBitMap.Height:=sy;

          for y:=0 to sy-1 do for x:=0 to sx-1 do begin
            t:=RXData[2].Data[ci,y*sx+x];
            MyBitMap.Canvas.Pixels[x,y]:=fResource.GetColor32(t,DEF_PAL);
          end;
          if sy>0 then MyBitMap.SaveToFile(ExeDir+'Export\HouseAnim\'+s+'\'+int2fix(ID,2)+'\_'+int2fix(Ac,1)+'_'+int2fix(k,2)+'.bmp');
        end;
      end;
    end;
  end;

  MyBitMap.Free;
end;


{Export Trees graphics categorized by ID}
procedure ExportTreeAnim2BMP();
var MyBitMap:TBitMap;
    ID,k,ci:integer; t:byte;
    sy,sx,y,x:integer;
begin
  CreateDir(ExeDir+'Export\');
  CreateDir(ExeDir+'Export\TreeAnim\');
  MyBitMap:=TBitMap.Create;
  MyBitMap.PixelFormat:=pf24bit;

  fResource.LoadMapElemDAT(ExeDir+'data\defines\mapelem.dat');
  fResource.LoadRX(ExeDir+'data\gfx\res\'+RXData[1].Title+'.rx',1);

  ci:=0;
  for ID:=1 to MapElemQty do begin
    for k:=1 to MapElem[ID].Count do begin
      if MapElem[ID].Step[k]+1<>0 then
      ci:=MapElem[ID].Step[k]+1;

      sx:=RXData[1].Size[ci].X;
      sy:=RXData[1].Size[ci].Y;
      MyBitMap.Width:=sx;
      MyBitMap.Height:=sy;

      for y:=0 to sy-1 do for x:=0 to sx-1 do begin
        t:=RXData[1].Data[ci,y*sx+x];
        MyBitMap.Canvas.Pixels[x,y]:=fResource.GetColor32(t,DEF_PAL);
      end;
      if sy>0 then MyBitMap.SaveToFile(
      //@Lewin: insert field here and press Export>TreeAnim. Rename each folder after export to 'Cuttable',
      //'Quad' and etc.. there you'll have it. Note, we use 1..254 counting, JBSnorro uses 0..253 counting
      ExeDir+'Export\TreeAnim\'+{inttostr(word(MapElem[ID].DiagonalBlocked))+'_'+}int2fix(ID,3)+'_'+int2fix(k,2)+'.bmp');
    end;
  end;

  MyBitMap.Free;
end;

{Tile textures aren't always the same, e.g. if someone makes a mod they will be different,
thus it's better to spend few ms and generate minimap colors from actual data}
procedure TResource.MakeMiniMapColors(FileName:string);
var ii,kk,h,j,px:integer; c:array of byte; R,G,B,SizeX,SizeY:integer; f:file; {ft:textfile;}
  {$IFDEF WDC}
  InputStream: TFileStream;
  OutputStream: TMemoryStream;
  DeCompressionStream: TZDecompressionStream;
  {$ENDIF}
  {$IFDEF FPC}
  {InStream: TMemoryStream;
  Comp:Pointer;
  DestSize:cardinal;}
  {$ENDIF}
begin
  if not FileExists(FileName) then exit;
  assignfile(f,FileName);
  FileMode:=0; Reset(f,1); FileMode:=2; //Open ReadOnly

  setlength(c,18+1);
  blockread(f,c[1],18); //SizeOf(TGAHeader)
  SizeX := c[13]+c[14]*256;
  SizeY := c[15]+c[16]*256;

  if c[1]=120 then
  begin
    {$IFDEF WDC}
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
    {$IFDEF FPC}
    //todo: Read zlib packed texture to minimap color
    {InStream := TMemoryStream.Create;
    InStream.LoadFromFile(FileName);
    GetMem(Comp, InStream.Size);
    InStream.Read(Comp^, InStream.Size);
    setlength(c,SizeX*SizeY*4+1);
    DestSize := SizeX*SizeY*4 + 18; //SizeOf(TGAHeader)
    uncompress(@c[1], DestSize, Comp, InStream.Size);
    InStream.Free;}
    exit;
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

  {assignfile(ft,ExeDir+'mm.txt'); rewrite(ft);
  for ii:=1 to 256 do begin
    write(ft, '$'+inttohex(TileMMColor[ii].B,2)+inttohex(TileMMColor[ii].G,2)+inttohex(TileMMColor[ii].R,2)+',');
    if ii mod 8 = 0 then writeln(ft);
  end;
  closefile(ft);}
end;


procedure TResource.MakeCursors(RXid:integer);
var
  i,sx,sy,x,y:integer;
  bm,bm2:TBitMap;
  IconInfo:TIconInfo;
begin
  bm:=TBitMap.Create;  bm.PixelFormat:=pf24bit;
  bm2:=TBitMap.Create; bm2.PixelFormat:=pf24bit;

  for i:=1 to length(Cursors) do begin

    //Special case for invisible cursor
    if Cursors[i] = 999 then
    begin
      bm.Width  := 1; bm.Height  := 1;
      bm2.Width := 1; bm2.Height := 1;
      bm2.Canvas.Pixels[0,0] := clWhite; //Invisible mask, we don't care for Image color
      IconInfo.xHotspot := 0;
      IconInfo.yHotspot := 0;
    end
    else
    begin
      sx := RXData[RXid].Size[Cursors[i]].X;
      sy := RXData[RXid].Size[Cursors[i]].Y;
      bm.Width  := sx; bm.Height  := sy;
      bm2.Width := sx; bm2.Height := sy;

      for y:=0 to sy-1 do for x:=0 to sx-1 do
      begin
        //todo: Find a PC which doesn't shows transparency and try to change 4th byte in bm.Canvas.Pixels
        if RXData[RXid].RGBA[Cursors[i],y*sx+x] and $FF000000 = 0 then begin
          bm.Canvas.Pixels[x,y] := 0; //If not reset will invert background color
          bm2.Canvas.Pixels[x,y] := clWhite;
        end else begin
          bm.Canvas.Pixels[x,y] := (RXData[RXid].RGBA[Cursors[i],y*sx+x] AND $FFFFFF);
          bm2.Canvas.Pixels[x,y] := clBlack;
        end;
        //bm2.Canvas.Pixels[x,y] := byte((RXData[RXid].RGBA[Cursors[i],y*sx+x] shr 24) and $FF)*65793;
      end;
      //Load hotspot offsets from RX file, adding the manual offsets (normally 0)
      IconInfo.xHotspot := Math.max(-RXData[RXid].Pivot[Cursors[i]].x+CursorOffsetsX[i],0);
      IconInfo.yHotspot := Math.max(-RXData[RXid].Pivot[Cursors[i]].y+CursorOffsetsY[i],0);
    end;
    IconInfo.fIcon := false; //true=Icon, false=Cursor
    IconInfo.hbmColor:=bm.Handle;
    IconInfo.hbmMask:=bm2.Handle;

    Screen.Cursors[Cursors[i]]:=CreateIconIndirect(iconInfo);
  end;

  bm.Free;
  bm2.Free;
  Screen.Cursor := c_Default;
end;





end.
