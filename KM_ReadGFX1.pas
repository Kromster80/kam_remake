unit KM_ReadGFX1;
interface
uses OpenGL, Windows, Forms, Controls, KM_Defaults;

type
  TByteArray2 = array of Byte;
  TWordArray2 = array of Word;
  TexMode = (tm_NoCol, tm_TexID, tm_AltID);

    function ReadGFX(text: string):boolean;

      function ReadPallete(filename:string):boolean;
      function ReadMapElem(filename:string):boolean;
      function ReadHouseDAT(filename:string):boolean;
      function ReadUnitDAT(filename:string):boolean;

      function ReadFont(filename:string; aFont:TKMFont):boolean;

      function ReadRX(filename:string; ID:integer):boolean;
      procedure MakeGFX(Sender: TObject; RXid:integer);

    procedure ExportRX2BMP(RXid:integer);

    function MakeMiniMapColors(filename:string):boolean;
    function MakeCursors(RXid:integer):boolean;
    function MakeResourceIcons(RXid:integer):boolean;

implementation

uses KromUtils, KM_Unit1, KM_Form_Loading,  Graphics, Sysutils, Dialogs, math, dglOpenGL,
     KM_Global_Data, KM_Log;

function ReadGFX(text: string):boolean;
var i:integer; procedure StepRefresh(); begin FormLoading.Bar1.StepIt; FormLoading.Refresh; end;
begin
  fLog.AppendLog('Reading pal0.bbm',ReadPallete(text+'data\gfx\pal0.bbm'));           StepRefresh();
  fLog.AppendLog('Reading mapelem.dat',ReadMapElem(text+'data\defines\mapelem.dat')); StepRefresh();
  fLog.AppendLog('Reading houses.dat',ReadHouseDAT(text+'data\defines\houses.dat'));  StepRefresh();
  fLog.AppendLog('Reading unit.dat',ReadUnitDAT(text+'data\defines\unit.dat'));       StepRefresh();

  fLog.AppendLog('Reading game.fnt',ReadFont(text+'data\gfx\fonts\game.fnt',fnt_Game));       StepRefresh();

  RXData[1].Title:='Trees';       RXData[1].NeedTeamColors:=false;
  RXData[2].Title:='Houses';      RXData[2].NeedTeamColors:=true;
  RXData[3].Title:='Units';       RXData[3].NeedTeamColors:=true;
  RXData[4].Title:='GUI';         RXData[4].NeedTeamColors:=false;
  RXData[5].Title:='GUIMain';     RXData[5].NeedTeamColors:=false;

  for i:=1 to 5 do begin
    fLog.AppendLog('Reading '+RXData[i].Title+'.rx',ReadRX(text+'data\gfx\res\'+RXData[i].Title+'.rx',i));

    if RXData[i].Title='GUI' then begin
      MakeCursors(i);
      MakeResourceIcons(i);
    end;

    MakeGFX(nil,i);
    StepRefresh();
  end;

  fLog.AppendLog('Preparing MiniMap colors',MakeMiniMapColors('')); StepRefresh();
  fLog.AppendLog('ReadGFX is done');

  Result:=true;
end;


//=============================================
//Reading pallete for trees/objects
//=============================================
function ReadPallete(filename:string):boolean;
var f:file;
begin
Result:=false;
if not CheckFileExists(filename) then exit;
  assignfile(f,filename);
  reset(f,1);
  blockread(f,Pal0,48); //Unknown and/or unimportant 
  blockread(f,Pal0,768); //256*3
  closefile(f);
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

  Result:=true;
end;


//=============================================
//Reading houses.dat data
//=============================================
function ReadHouseDAT(filename:string):boolean;
var ii,kk:integer; ft:textfile; f:file;
begin
Result:=false;
if not CheckFileExists(filename) then exit;
assignfile(f,filename); reset(f,1);
blockread(f,HouseDAT1,30*70);
for ii:=1 to 29 do begin
blockread(f,HouseDAT[ii],88+19*70+270);
end;
closefile(f);

assignfile(ft,ExeDir+'Houses.txt'); rewrite(ft);
for ii:=1 to 29 do begin
writeln(ft);
writeln(ft);
writeln(ft,HouseName[ii]);
  for kk:=1 to 4 do if HouseDAT[ii].SupplyIn[kk,1]>0 then
  write(ft,'#') else write(ft,' ');
  writeln(ft);
  for kk:=1 to 4 do if HouseDAT[ii].SupplyOut[kk,1]>0 then
  write(ft,'#') else write(ft,' ');
  writeln(ft);
  for kk:=1 to 19 do
    writeln(ft,inttostr(kk)+'. '+inttostr(HouseDAT[ii].Anim[kk].Count));

  for kk:=1 to 135 do
  write(ft,inttostr(HouseDAT[ii].Foot[kk]+1)+' ');
  writeln(ft);
end;
closefile(ft);

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
blockread(f,UnitCarry[ii],8*70);
end;
for ii:=1 to 41 do begin
blockread(f,UnitStat[ii],22);
blockread(f,UnitSprite[ii],112*70);
blockread(f,UnitSprite2[ii],36);
end;
closefile(f);

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
      if UnitSprite[ii].Act[kk].Dir[hh].Step[jj]>0 then write(ft,'#');
      write(ft,inttostr(UnitSprite[ii].Act[kk].Dir[hh].Count)+' ');
      write(ft,inttostr(UnitSprite[ii].Act[kk].Dir[hh].MoveX)+' ');
      write(ft,inttostr(UnitSprite[ii].Act[kk].Dir[hh].MoveY)+' ');
      writeln(ft);
    end;
end;
closefile(ft);
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
procedure GenTexture(ID:PGLUint; mx, my:integer; Data:TByteArray2; Mode:TexMode);
var
  i,k:integer;
  x:byte;
  by:^cardinal;
  DestX, DestY:integer;
  col:cardinal;
  TD:Pointer;
begin

DestX:=MakePOT(mx);
DestY:=MakePOT(my);

//Convert palette bitmap data to 32bit RGBA texture data
TD:=AllocMem(DestX*DestY*4); //same as GetMem+FillChar(0)
for i:=0 to (DestY-1) do for k:=0 to (DestX-1) do
  if (i<my)and(k<mx) then begin
    x:=Data[i*mx+k];
    if (x<>0) then begin
      by:=pointer(integer(TD)+((i+DestY-my)*DestX+k)*4); //Get pointer

      if Mode=tm_NoCol then
          col:=Pal0[x+1,1]+Pal0[x+1,2] SHL 8 +Pal0[x+1,3] SHL 16 OR $FF000000
      else

      if Mode=tm_TexID then
        if x in [24..30] then
          col:=((x-27)*42+128)*65793 OR $FF000000 //convert to greyscale B>>>>>W
        else
          col:=Pal0[x+1,1]+Pal0[x+1,2] SHL 8 +Pal0[x+1,3] SHL 16 OR $FF000000
      else

      if Mode=tm_AltID then
        case x of
          24,30: col:=$70FFFFFF;   //7
          25,29: col:=$B0FFFFFF;   //11
          26,28: col:=$E0FFFFFF;   //14
          27:    col:=$FFFFFFFF;   //16
          else   col:=0;
        end;

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
FreeMem(TD);
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
  while((id+ad<RXData[RXid].Qty)and
        (HeightPOT=MakePOT(RXData[RXid].Size[id+ad,2]))and
        (WidthPOT+RXData[RXid].Size[id+ad,1]<=MaxTexRes)) do begin
    inc(WidthPOT,RXData[RXid].Size[id+ad,1]);    
    inc(ad);
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
  if RXData[RXid].NeedTeamColors then
    GenTexture(@GFXData[RXid,id].TexID,WidthPOT,HeightPOT,@TD[0],tm_TexID)
  else
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
  for j:=id to id+ad-1 do begin
    GFXData[RXid,j].TexID:=GFXData[RXid,id].TexID;
    GFXData[RXid,j].AltID:=GFXData[RXid,id].AltID;
    GFXData[RXid,j].u1:=k/WidthPOT;
    GFXData[RXid,j].v1:=0;
    inc(k,RXData[RXid].Size[j,1]);
    GFXData[RXid,j].u2:=k/WidthPOT;
    GFXData[RXid,j].v2:=RXData[RXid].Size[j,2]/HeightPOT;
    GFXData[RXid,j].PxWidth:=RXData[RXid].Size[j,1];
    GFXData[RXid,j].PxHeight:=RXData[RXid].Size[j,2];

    setlength(RXData[RXid].Data[j],0);
    inc(Rm,RXData[RXid].Size[j,1]*RXData[RXid].Size[j,2]*4);
  end;

  inc(Am,WidthPOT*HeightPOT*4);
  inc(id,ad-1);
  inc(TexCount);

until(id=RXData[RXid].Qty);

fLog.AppendLog(inttostr(TexCount)+' Textures created');
fLog.AppendLog(inttostr(Am div 1024)+'/'+inttostr((Am-Rm) div 1024)+' Kbytes allocated/wasted for units GFX when using Packing');
fLog.AppendLog(inttostr(Cm div 1024)+' KBytes for team colors'+eol);
end;

//=============================================
//Export RX to Bitmaps
//=============================================
{That is when we want to export RX to Bitmaps without need to have GraphicsEditor, also this way we preserve image indexes}
procedure ExportRX2BMP(RXid:integer);
var MyBitMap:TBitMap;
    id,t:integer;
    sy,sx,y,x:integer;
begin
  CreateDir(ExeDir+RXData[RXid].Title+'rx\');
  MyBitMap:=TBitMap.Create;
  MyBitmap.PixelFormat:=pf24bit;

  ReadRX(ExeDir+'data\gfx\res\'+RXData[RXid].Title+'.rx',RXid);

  for id:=1 to RXData[RXid].Qty do begin

    sx:=RXData[RXid].Size[id,1];
    sy:=RXData[RXid].Size[id,2];
    MyBitmap.Width:=sx;
    MyBitmap.Height:=sy;

    for y:=0 to sy-1 do for x:=0 to sx-1 do begin
      t:=RXData[RXid].Data[id,y*sx+x]+1;
      MyBitmap.Canvas.Pixels[x,y]:=Pal0[t,1]+Pal0[t,2]*256+Pal0[t,3]*65536;
    end;
    if sy>0 then MyBitmap.SaveToFile(ExeDir+RXData[RXid].Title+'rx\'+RXData[RXid].Title+'_'+int2fix(id,4)+'.bmp');

    setlength(RXData[RXid].Data[id],0);
  end;
end;

{Tile textures aren't always the same, e.g. if someone makes a mod they will be different,
thus it's better to spend few ms and generate minimap colors from actual data}
function MakeMiniMapColors(filename:string):boolean;
var ii,kk,h,j:integer; c:array of byte; R,G,B:integer; f:file;
begin
Result:=false;
assignfile(f,ExeDir+'Resource\Tiles512.tga');
FileMode:=0; Reset(f,1); FileMode:=2; //Open ReadOnly

setlength(c,512*512*4+1);
blockread(f,c[1],18);
blockread(f,c[1],512*512*4);
closefile(f);

for ii:=0 to 15 do for kk:=0 to 15 do begin

  R:=0; G:=0; B:=0;

  for j:=0 to 31 do
  for h:=0 to 31 do begin
    inc(B, c[((ii+j)*512+kk*32+h)*4+1]);
    inc(G, c[((ii+j)*512+kk*32+h)*4+2]);
    inc(R, c[((ii+j)*512+kk*32+h)*4+3]);
  end;

  TileMMColor[ii*16+kk+1].R:=round (R / 1024); //each tile is 32x32 px
  TileMMColor[ii*16+kk+1].G:=round (G / 1024);
  TileMMColor[ii*16+kk+1].B:=round (B / 1024);

end;

setlength(c,0);
Result:=true;
end;


function MakeCursors(RXid:integer):boolean;
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
      bm.Canvas.Pixels[x,y]:=Pal0[t,1]+Pal0[t,2]*256+Pal0[t,3]*65536;
      if t=1 then
        bm2.Canvas.Pixels[x,y]:=$FFFFFF
      else
        bm2.Canvas.Pixels[x,y]:=$000000;
    end;

  IconInfo.fIcon:=false;
  IconInfo.xHotspot:=1;
  IconInfo.yHotspot:=1;
  IconInfo.hbmMask:=bm2.Handle;
  IconInfo.hbmColor:=bm.Handle;

  Screen.Cursors[Cursors[i]]:=CreateIconIndirect(iconInfo);
  end;

  Screen.Cursor:=c_Default;
end;


function MakeResourceIcons(RXid:integer):boolean;
var
  i,sx,sy,x,y,t:integer;
  bm,bm2:TBitmap;
begin
  bm:=TBitmap.Create;  bm.PixelFormat:=pf24bit;
  bm2:=TBitmap.Create; bm2.PixelFormat:=pf24bit;

  for i:=351 to 378 do begin

    sx:=RXData[RXid].Size[i,1];
    sy:=RXData[RXid].Size[i,2];

    bm.Width:=20; bm.Height:=20;
    bm2.Width:=20; bm2.Height:=20;

    for y:=0 to 20-1 do for x:=0 to 20-1 do begin

      if (y>sy-1)or(x>sx-1) then t:=1 else t:=RXData[RXid].Data[i,y*sx+x]+1;
      bm.Canvas.Pixels[x,y]:=Pal0[t,1]+Pal0[t,2]*256+Pal0[t,3]*65536;
      if t=1 then
        bm2.Canvas.Pixels[x,y]:=$FFFFFF
      else
        bm2.Canvas.Pixels[x,y]:=$000000;
    end;

  Form1.IL_ResourceIcons.Add(bm,bm2);
  end;

end;


function ReadFont(filename:string; aFont:TKMFont):boolean;
const
  TexWidth=256; //Connected to TexData, don't change
var
  f:file;
  t:byte;
  a,b,c,d:word;
  i,ci,ck:integer;
  MaxHeight:integer;
  AdvX,AdvY:integer;
  TD:array of byte;
  MyBitMap:TBitMap;
begin
Result:=false;
MaxHeight:=0;
if not CheckFileExists(filename) then exit;
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
      blockread(f,Data[1],Width*Height);
    end;

closefile(f);

//Compile texture
AdvX:=0; AdvY:=0;
setlength(TD,256*256+1);

for i:=0 to 255 do
  if FontData[byte(aFont)].Pal[i]<>0 then
    with FontData[byte(aFont)].Letters[i] do begin
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

  GenTexture(@FontData[byte(aFont)].TexID,TexWidth,TexWidth,@TD[0],tm_TexID);

MyBitMap:=TBitMap.Create;
MyBitmap.PixelFormat:=pf24bit;
MyBitmap.Width:=256;
MyBitmap.Height:=256;

for ci:=0 to 255 do for ck:=0 to 255 do begin
  t:=TD[ci*256+ck]+1;
  MyBitmap.Canvas.Pixels[ck,ci]:=Pal0[t,1]+Pal0[t,2]*256+Pal0[t,3]*65536;
end;

MyBitmap.SaveToFile(ExeDir+'font5.bmp');

setlength(TD,0);
Result:=true;
end;


end.
