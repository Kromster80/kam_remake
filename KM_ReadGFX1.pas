unit KM_ReadGFX1;
interface
uses OpenGL, Windows, Forms, Graphics, SysUtils, Math, dglOpenGL, KM_Defaults, KM_LoadLib;

type
  TByteArray2 = array of Byte;
  TexMode = (tm_NoCol, tm_TexID, tm_AltID, tm_AlphaTest);

    function ReadGFX(text: string; LoadEssence:boolean):boolean;

      function ReadPallete(filename:string; PalID:byte):boolean;
      function ReadMapElem(filename:string):boolean;
      function ReadHouseDAT(filename:string):boolean;
      function ReadUnitDAT(filename:string):boolean;

      function ReadFont(filename:string; aFont:TKMFont; WriteFontToBMP:boolean):boolean;

      function ReadRX(filename:string; ID:integer):boolean;
      procedure MakeGFX_AlphaTest(Sender: TObject; RXid:integer);
      procedure MakeGFX(Sender: TObject; RXid:integer);

    procedure ExportRX2BMP(RXid:integer);
    procedure ExportUnitAnim2BMP();
    procedure ExportHouseAnim2BMP();

    procedure MakeMiniMapColors();
    procedure MakeCursors(RXid:integer);

implementation

uses KromUtils, KM_Unit1, KM_Render, KM_Game;

function ReadGFX(text: string; LoadEssence:boolean):boolean;
var i,RX1,RX2:integer; procedure StepRefresh(); begin FormLoading.Bar1.StepIt; FormLoading.Refresh; end;
begin
  Assert(fTextLibrary<>nil,'fTextLibrary should be init before ReadGFX');
  Assert(fRender<>nil,'fRender should be init before ReadGFX to be able access OpenGL');

  if LoadEssence then begin
    FormLoading.Label1.Caption:='Reading palettes ...';
    ReadPallete(text+'data\gfx\map.bbm',pal_map);
    ReadPallete(text+'data\gfx\pal0.bbm',pal_0);
    ReadPallete(text+'data\gfx\pal1.bbm',pal_1);
    ReadPallete(text+'data\gfx\pal2.bbm',pal_2);
    ReadPallete(text+'data\gfx\pal3.bbm',pal_3);
    ReadPallete(text+'data\gfx\pal4.bbm',pal_4);
    ReadPallete(text+'data\gfx\pal5.bbm',pal_5);
    ReadPallete(text+'data\gfx\setup.bbm',pal_set);
    ReadPallete(text+'data\gfx\setup2.bbm',pal_set2);
    ReadPallete(text+'data\gfx\map.bbm',pal_lin);
    ReadPallete(text+'data\gfx\mapgold.lbm',pal2_mapgold);
    ReadPallete(text+'data\gfx\setup.lbm',pal2_setup);
    ReadPallete(text+'data\gfx\pal1.lbm',pal2_1);
    fLog.AppendLog('Reading palettes',true);
    StepRefresh();
  end;

  if not LoadEssence then begin
    FormLoading.Label1.Caption:='Reading defines ...';
    fLog.AppendLog('Reading mapelem.dat',ReadMapElem(text+'data\defines\mapelem.dat')); StepRefresh();
    fLog.AppendLog('Reading houses.dat',ReadHouseDAT(text+'data\defines\houses.dat'));  StepRefresh();
    fLog.AppendLog('Reading unit.dat',ReadUnitDAT(text+'data\defines\unit.dat'));       StepRefresh();
  end;

  if LoadEssence then begin
    RXData[1].Title:='Trees';       RXData[1].NeedTeamColors:=false;
    RXData[2].Title:='Houses';      RXData[2].NeedTeamColors:=true;
    RXData[3].Title:='Units';       RXData[3].NeedTeamColors:=true;
    RXData[4].Title:='GUI';         RXData[4].NeedTeamColors:=true; //Required for unit scrolls, etc.
    RXData[5].Title:='GUIMain';     RXData[5].NeedTeamColors:=false;
  end;

  if LoadEssence then begin
    RX1:=4; RX2:=5;
  end else begin
    RX1:=1; RX2:=3;
  end;

  for i:=RX1 to RX2 do
  if (i in [4,5])or(MakeGameSprites) then begin //Always make GUI
    FormLoading.Label1.Caption:='Reading '+RXData[i].Title+' GFX ...';
    fLog.AppendLog('Reading '+RXData[i].Title+'.rx',ReadRX(text+'data\gfx\res\'+RXData[i].Title+'.rx',i));
    if i=4 then MakeCursors(4); //Make GUI items
    MakeGFX(nil,i);
    if i=2 then MakeGFX_AlphaTest(nil,i);
    StepRefresh();
  end;

  if not LoadEssence then begin
    FormLoading.Label1.Caption:='Making minimap colors ...';
    MakeMiniMapColors();
    fLog.AppendLog('Prepared MiniMap colors...');
    StepRefresh();
  end;

  if LoadEssence then begin
    FormLoading.Label1.Caption:='Reading fonts ...';
    for i:=1 to length(FontFiles) do
      ReadFont(text+'data\gfx\fonts\'+FontFiles[i]+'.fnt',TKMFont(i),false);
    fLog.AppendLog('Read fonts is done');
    StepRefresh();
  end;

  fLog.AppendLog('ReadGFX is done');
  Result:=true;
  //Form1.Close;
end;


//=============================================
//Reading pallete for trees/objects
//=============================================
function ReadPallete(filename:string; PalID:byte):boolean;
var f:file; i:integer;
begin
Result:=false;
if not CheckFileExists(filename) then exit;
  assignfile(f,filename);
  reset(f,1);
  blockread(f,Pal[PalID],48); //Unknown and/or unimportant
  blockread(f,Pal[PalID],768); //256*3
  closefile(f);

  if PalID=pal_lin then //Make greyscale linear Pal
    for i:=0 to 255 do begin
      Pal[pal_lin,i+1,1]:=i;
      Pal[pal_lin,i+1,2]:=i;
      Pal[pal_lin,i+1,3]:=i;
    end;

Result:=true;
end;


//=============================================
//Reading map elements (has animation data)
//=============================================
function ReadMapElem(filename:string):boolean;
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
    writeln(ft);
    writeln(ft,ii);
      for kk:=1 to 30 do if MapElem[ii].Step[kk]>0 then
      write(ft,MapElem[ii].Step[kk],' ') else write(ft,'- ');

      writeln(ft);
      for kk:=1 to 16 do
      write(ft,MapElem[ii].u1[kk],' ');
      write(ft,' =',MapElem[ii].u2);
      write(ft,' =',MapElem[ii].u3);
      write(ft,' =',MapElem[ii].u4);
      writeln(ft);
    end;
    closefile(ft);
  end;

  Result:=true;
end;


//=============================================
//Reading houses.dat data
//=============================================
function ReadHouseDAT(filename:string):boolean;
var ii,kk,h:integer; ft:textfile; f:file;
begin
Result:=false;
if not CheckFileExists(filename) then exit;
assignfile(f,filename); reset(f,1);
blockread(f,HouseDAT1,30*70);
for h:=1 to 29 do begin
blockread(f,HouseDAT[h],88+19*70+270);
end;
//Append info for new houses
for ii:=30 to HOUSE_COUNT do begin
fillChar(HouseDAT[ii],SizeOf(HouseDAT[ii]),#0);
HouseDAT[ii].StonePic:=129-1;
HouseDAT[ii].WoodPic:=130-1;
HouseDAT[ii].WoodPal:=132-1;
HouseDAT[ii].StonePal:=131-1;
HouseDAT[ii].WoodPicSteps:=7;
HouseDAT[ii].StonePicSteps:=8;
HouseDAT[ii].MaxHealth:=50;
end;
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
function ReadUnitDAT(filename:string):boolean;
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


//=============================================
//Reading RX Data
//=============================================
function ReadRX(filename:string; ID:integer):boolean;
  var i:integer; f:file;
begin
  Result:=false;
  if not CheckFileExists(filename) then exit;

  assignfile(f,filename); reset(f,1);
  blockread(f, RXData[ID].Qty, 4);
  blockread(f, RXData[ID].Pal, RXData[ID].Qty);

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
procedure GenTexture(ID:PGLUint; mx, my:integer; Data:TByteArray2; Mode:TexMode; const UsePal:byte=DEF_PAL);
var
  MyBitMap:TBitMap;
  i,k:word;
  x:byte;
  by:^cardinal;
  DestX, DestY:word;
  col:cardinal;
  TD:Pointer;
begin

DestX:=MakePOT(mx);
DestY:=MakePOT(my);

if DestX*DestY=0 then exit; //Do not generate zeroed textures

if Mode=tm_AlphaTest then begin
  glGenTextures(1, id);
  glBindTexture(GL_TEXTURE_2D, id^);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
  glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MAX_LEVEL, 0);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP);
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
          col:=(byte(x-27)*42+128)*65793 OR $FF000000 //convert to greyscale B>>>>>W
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

      //col:=max(MAXDWORD-col,0) OR $FF000000;//Fast invert
      by^:=col;
    end;
  end;

glGenTextures(1, id);
begin
glBindTexture(GL_TEXTURE_2D, id^);
//gluBuild2DMipmaps(GL_TEXTURE_2D, GL_RGBA, DestX, DestY, GL_RGBA, GL_UNSIGNED_BYTE, TD);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
  glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MAX_LEVEL, 0);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP);
  case Mode of
  tm_NoCol: glTexImage2D(GL_TEXTURE_2D, 0, GL_RGB5_A1, DestX, DestY, 0, GL_RGBA, GL_UNSIGNED_BYTE, TD);
  tm_TexID: glTexImage2D(GL_TEXTURE_2D, 0, GL_RGB5_A1, DestX, DestY, 0, GL_RGBA, GL_UNSIGNED_BYTE, TD);
  tm_AltID: glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA2, DestX, DestY, 0, GL_RGBA, GL_UNSIGNED_BYTE, TD);
  end;
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
    MyBitmap.SaveToFile(ExeDir+'GenTextures\'+int2fix(ID^,4)+'.bmp');
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
procedure MakeGFX_AlphaTest(Sender: TObject; RXid:integer);
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

    GenTexture(@GFXData[RXid,ID1].TexID,WidthPOT,HeightPOT,@TD[0],tm_AlphaTest);
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
Textures should be POT to improve performance and avoid drivers bugs
In result we have GFXData filled.}
procedure MakeGFX(Sender: TObject; RXid:integer);
var
  ci,ad,j,i,k,id,TexCount:integer;
  Am,Cm,Rm:integer;
  WidthPOT,HeightPOT:integer;
  TD:array of byte;
begin
id:=0; Am:=0; Rm:=0; Cm:=0; TexCount:=0;
repeat
  inc(id);

  WidthPOT:=RXData[RXid].Size[id,1];
  HeightPOT:=MakePOT(RXData[RXid].Size[id,2]);
  ad:=1;

  //Pack textures with same POT height into rows to save memory
  //This also means fewer textures for GPU RAM == better performance
  while((id+ad<RXData[RXid].Qty)and //Keep packing until end of sprites
        (
          (HeightPOT=MakePOT(RXData[RXid].Size[id+ad,2])) //Pack if HeightPOT matches
          or((HeightPOT>=MakePOT(RXData[RXid].Size[id+ad,2]))AND(WidthPOT+RXData[RXid].Size[id+ad,1]<MakePOT(WidthPOT)))
        )and
        (WidthPOT+RXData[RXid].Size[id+ad,1]<=MAX_TEX_RESOLUTION)) //Pack until Texresolution approached
  do begin
    inc(WidthPOT,RXData[RXid].Size[id+ad,1]);
    inc(ad);
    if (RXid=5)and(RX5pal[id]<>RX5pal[id+ad]) then break; //Don't align RX5 images for they use all different palettes
  end;

  WidthPOT:=MakePOT(WidthPOT);
  setlength(TD,WidthPOT*HeightPOT+1);

  for i:=1 to HeightPOT do begin
    ci:=0;
    for j:=id to id+ad-1 do
      for k:=1 to RXData[RXid].Size[j,1] do begin
        inc(ci);
        if i<=RXData[RXid].Size[j,2] then
          TD[(i-1)*WidthPOT+ci-1]:=RXData[RXid].Data[j,(i-1)*RXData[RXid].Size[j,1]+k-1]
        else
          TD[(i-1)*WidthPOT+ci-1]:=0;
      end;
  end;

  //If we need to prepare textures for TeamColors
  if MakeTeamColors and RXData[RXid].NeedTeamColors then
    GenTexture(@GFXData[RXid,id].TexID,WidthPOT,HeightPOT,@TD[0],tm_TexID)
  else
    if RXid=5 then begin
      if RX5Pal[id]<>0 then
        GenTexture(@GFXData[RXid,id].TexID,WidthPOT,HeightPOT,@TD[0],tm_NoCol,RX5Pal[id])
      else
        GenTexture(@GFXData[RXid,id].TexID,WidthPOT,HeightPOT,@TD[0],tm_NoCol,10);
    end else
      GenTexture(@GFXData[RXid,id].TexID,WidthPOT,HeightPOT,@TD[0],tm_NoCol);

  //TeamColors are done through alternative plain colored texture
  if MakeTeamColors and RXData[RXid].NeedTeamColors then
  for i:=0 to length(TD)-1 do
    if TD[i] in [24..30] then begin
      GenTexture(@GFXData[RXid,id].AltID,WidthPOT,HeightPOT,@TD[0],tm_AltID);
      inc(Cm,WidthPOT*HeightPOT*4);
      break;
    end;

  setlength(TD,0);

  k:=0;
  for j:=id to id+ad-1 do begin //Hack to test AlphaTest
    GFXData[RXid,j].TexID:=GFXData[RXid,id].TexID;
    GFXData[RXid,j].AltID:=GFXData[RXid,id].AltID;
    GFXData[RXid,j].u1:=k/WidthPOT;
    GFXData[RXid,j].v1:=0;
    inc(k,RXData[RXid].Size[j,1]);
    GFXData[RXid,j].u2:=k/WidthPOT;
    GFXData[RXid,j].v2:=RXData[RXid].Size[j,2]/HeightPOT;
    GFXData[RXid,j].PxWidth:=RXData[RXid].Size[j,1];
    GFXData[RXid,j].PxHeight:=RXData[RXid].Size[j,2];

    //setlength(RXData[RXid].Data[j],0); //Do not erase since we need it for AlphaTest
    inc(Rm,RXData[RXid].Size[j,1]*RXData[RXid].Size[j,2]*4);
  end;

  inc(Am,WidthPOT*HeightPOT*4);
  inc(id,ad-1);
  inc(TexCount);

until(id=RXData[RXid].Qty);

fLog.AppendLog(inttostr(TexCount)+' Textures created');
fLog.AddToLog(inttostr(Am div 1024)+'/'+inttostr((Am-Rm) div 1024)+' Kbytes allocated/wasted for units GFX when using Packing');
fLog.AddToLog(inttostr(Cm div 1024)+' KBytes for team colors');
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

  ReadRX(ExeDir+'data\gfx\res\'+RXData[RXid].Title+'.rx',RXid);

  for id:=1 to RXData[RXid].Qty do begin

    sx:=RXData[RXid].Size[id,1];
    sy:=RXData[RXid].Size[id,2];
    MyBitmap.Width:=sx;
    MyBitmap.Height:=sy;

    UsePal:=DEF_PAL;
    if RXid=5 then UsePal:=RX5Pal[id];
    if UsePal=0 then UsePal:=10;

    for y:=0 to sy-1 do for x:=0 to sx-1 do begin
      t:=RXData[RXid].Data[id,y*sx+x]+1;
      MyBitmap.Canvas.Pixels[x,y]:=Pal[UsePal,t,1]+Pal[UsePal,t,2]*256+Pal[UsePal,t,3]*65536;
    end;
    if sy>0 then MyBitmap.SaveToFile(ExeDir+'Export\'+RXData[RXid].Title+'.rx\'+RXData[RXid].Title+'_'+int2fix(id,4)+'.bmp');

    setlength(RXData[RXid].Data[id],0);
  end;
end;

{Export Units graphics categorized by Unit and Action}
procedure ExportUnitAnim2BMP();
var MyBitMap:TBitMap;
    ID,Ac,Di,k,ci,t:integer;
    sy,sx,y,x:integer;
begin
  CreateDir(ExeDir+'Export\');
  CreateDir(ExeDir+'Export\UnitAnim\');
  MyBitMap:=TBitMap.Create;
  MyBitmap.PixelFormat:=pf24bit;

  ReadRX(ExeDir+'data\gfx\res\'+RXData[3].Title+'.rx',3);

  ci:=0;
  for ID:=2 to 2 do begin
    for Ac:=1 to 14 do begin
      for Di:=1 to 8 do if UnitSprite[ID].Act[Ac].Dir[Di].Step[1]<>-1 then begin
        for k:=1 to UnitSprite[ID].Act[Ac].Dir[Di].Count do begin
          CreateDir(ExeDir+'UnitAnim\'+TypeToString(TUnitType(ID))+'\');
          CreateDir(ExeDir+'UnitAnim\'+TypeToString(TUnitType(ID))+'\'+UnitAct[Ac]+'\');
          if UnitSprite[ID].Act[Ac].Dir[Di].Step[k]+1<>0 then
          ci:=UnitSprite[ID].Act[Ac].Dir[Di].Step[k]+1;

          sx:=RXData[3].Size[ci,1];
          sy:=RXData[3].Size[ci,2];
          MyBitmap.Width:=sx;
          MyBitmap.Height:=sy;

          for y:=0 to sy-1 do for x:=0 to sx-1 do begin
            t:=RXData[3].Data[ci,y*sx+x]+1;
            MyBitmap.Canvas.Pixels[x,y]:=Pal[DEF_PAL,t,1]+Pal[DEF_PAL,t,2]*256+Pal[DEF_PAL,t,3]*65536;
          end;
          if sy>0 then MyBitmap.SaveToFile(
          ExeDir+'Export\UnitAnim\'+TypeToString(TUnitType(ID))+'\'+UnitAct[Ac]+'\'+inttostr(Di)+'_'+int2fix(k,2)+'.bmp');
        end;
      end;
    end;
  end;

  MyBitmap.Free;
end;


{Export Houses graphics categorized by House and Action}
procedure ExportHouseAnim2BMP();
var MyBitMap:TBitMap;
    ID,Ac,k,ci,t:integer;
    sy,sx,y,x:integer;
begin
  CreateDir(ExeDir+'HouseAnim\');
  MyBitMap:=TBitMap.Create;
  MyBitmap.PixelFormat:=pf24bit;

  ReadRX(ExeDir+'data\gfx\res\'+RXData[2].Title+'.rx',2);

  ci:=0;
  for ID:=1 to 30 do begin
    for Ac:=1 to 5 do begin //Work1..Work5
      for k:=1 to HouseDAT[ID].Anim[Ac].Count do begin
        CreateDir(ExeDir+'HouseAnim\'+TypeToString(THouseType(ID))+'\');
        CreateDir(ExeDir+'HouseAnim\'+TypeToString(THouseType(ID))+'\Work'+IntToStr(Ac)+'\');
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
  
  MyBitmap.Free;
end;

{Tile textures aren't always the same, e.g. if someone makes a mod they will be different,
thus it's better to spend few ms and generate minimap colors from actual data}
procedure MakeMiniMapColors();
var ii,kk,h,j,px:integer; c:array of byte; R,G,B:integer; f:file;
begin
  assignfile(f,ExeDir+'Resource\Tiles512.tga');
  FileMode:=0; Reset(f,1); FileMode:=2; //Open ReadOnly

  setlength(c,512*512*4+1);
  blockread(f,c[1],18);
  blockread(f,c[1],512*512*4);
  closefile(f);

  for ii:=0 to 15 do for kk:=0 to 15 do begin

    R:=0; G:=0; B:=0;

    for j:=0 to 31 do for h:=0 to 31 do begin
      px:=((511-(ii*32+j))*512+kk*32+h)*4; //TGA comes flipped upside down
      inc(B, c[px+1]);
      inc(G, c[px+2]);
      inc(R, c[px+3]);
    end;

    if (kk<8)and(ii<8) then px:=ii*8+kk+1;
    if (kk>7)and(ii<8) then px:=ii*8+(kk-8)+64+1;
    if (kk<8)and(ii>7) then px:=(ii-8)*8+kk+128+1;
    if (kk>7)and(ii>7) then px:=(ii-8)*8+(kk-8)+192+1;

    TileMMColor[px].R:= (R / 1024) / 255; //each tile is 32x32 px
    TileMMColor[px].G:= (G / 1024) / 255;
    TileMMColor[px].B:= (B / 1024) / 255;

  end;
end;


procedure MakeCursors(RXid:integer);
var
  i,sx,sy,x,y,t:integer;
  bm,bm2:TBitmap;
  IconInfo:TIconInfo;
begin
  bm:=TBitmap.Create;  bm.PixelFormat:=pf24bit;
  bm2:=TBitmap.Create; bm2.PixelFormat:=pf24bit;

  for i:=1 to length(Cursors) do begin

    sx:=RXData[RXid].Size[Cursors[i],1];
    sy:=RXData[RXid].Size[Cursors[i],2];
    bm.Width:=sx; bm.Height:=sy;
    bm2.Width:=sx; bm2.Height:=sy;

    for y:=0 to sy-1 do for x:=0 to sx-1 do begin
      t:=RXData[RXid].Data[Cursors[i],y*sx+x]+1;
      bm.Canvas.Pixels[x,y]:=Pal[DEF_PAL,t,1]+Pal[DEF_PAL,t,2]*256+Pal[DEF_PAL,t,3]*65536;
      if t=1 then
        bm2.Canvas.Pixels[x,y]:=$FFFFFF
      else
        bm2.Canvas.Pixels[x,y]:=$000000;
    end;

  IconInfo.fIcon:=false;
  //Load hotspot offsets from RX file, adding the manual offsets (normally 0)
  IconInfo.xHotspot:=-RXData[RXid].Pivot[Cursors[i]].x+CursorOffsetsX[i];
  IconInfo.yHotspot:=-RXData[RXid].Pivot[Cursors[i]].y+CursorOffsetsY[i];
  IconInfo.hbmMask:=bm2.Handle;
  IconInfo.hbmColor:=bm.Handle;

  Screen.Cursors[Cursors[i]]:=CreateIconIndirect(iconInfo);
  end;

  bm.Free;
  bm2.Free;
  Screen.Cursor:=c_Default;
end;


function ReadFont(filename:string; aFont:TKMFont; WriteFontToBMP:boolean):boolean;
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
      MaxHeight:=max(MaxHeight,Height);
      Assert(Width*Height<>0); //Fon01.fnt seems to be damaged..
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

    Assert(FontData[byte(aFont)].Pal[i]=1);

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

  GenTexture(@FontData[byte(aFont)].TexID,TexWidth,TexWidth,@TD[0],tm_NoCol,FontPal[byte(aFont)]);

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


end.
