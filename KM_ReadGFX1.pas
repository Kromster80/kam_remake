unit KM_ReadGFX1;
interface

    function ReadGFX(text: string):boolean;
    function ReadPallete(filename:string):boolean;
    function ReadTreesRX(filename:string):boolean;
    function ReadHousesRX(filename:string):boolean;
    function ReadUnitsRX(filename:string):boolean;
    function ReadUnitDAT(filename:string):boolean;
    function ReadHouseDAT(filename:string):boolean;
    function ReadMapElem(filename:string):boolean;
    function AddObjectsToImageList():boolean;
    function AddHousesToImageList():boolean;
    function AddUnitsToImageList():boolean;
    procedure MakeObjectsGFX(Sender: TObject);
    procedure MakeHousesGFX(Sender: TObject);
    procedure MakeUnitsGFX(Sender: TObject);

implementation

uses KromUtils, KM_Unit1, KM_Defaults, KM_Form_Loading,  Graphics, Sysutils, Dialogs, math, OpenGL, dglOpenGL,
     KM_Global_Data, KM_Log;

function ReadGFX(text: string):boolean;
        procedure StepRefresh();
          begin
            FormLoading.Bar1.StepIt; FormLoading.Refresh;
          end;
begin
fLog.AppendLog('Reading pallete',ReadPallete(text+'data\gfx\pal0.bbm'));            StepRefresh();
fLog.AppendLog('Reading objects',ReadTreesRX(text+'data\gfx\res\trees.rx'));        StepRefresh();
fLog.AppendLog('Reading houses',ReadHousesRX(text+'data\gfx\res\houses.rx'));       StepRefresh();
fLog.AppendLog('Reading units',ReadUnitsRX(text+'data\gfx\res\units.rx'));          StepRefresh();
fLog.AppendLog('Reading mapelem.dat',ReadMapElem(text+'data\defines\mapelem.dat')); StepRefresh();
fLog.AppendLog('Reading houses.dat',ReadHouseDAT(text+'data\defines\houses.dat'));  StepRefresh();
fLog.AppendLog('Reading unit.dat',ReadUnitDAT(text+'data\defines\unit.dat'));       StepRefresh();

//AddObjectsToImageList();
//AddHousesToImageList();
//AddUnitsToImageList();

Result:=true;
end;

//=============================================
//Reading pallete for trees/objects
//=============================================
function ReadPallete(filename:string):boolean;
begin
Result:=false;
if fileexists(filename) then
  begin
    assignfile(f,filename);
    reset(f,1);
    blockread(f,c,48);
    blockread(f,Pal0,768); //256*3
    closefile(f);
  end
else
  begin
    ShowMessage('Unable to locate ".\data\gfx\pal0.bbm" file');
    exit;
  end;
Result:=true;
end;

//=============================================
//Reading trees/objects data
//=============================================
function ReadTreesRX(filename:string):boolean;
var ii:integer;
begin
Result:=false;
if fileexists(filename) then
  begin
    assignfile(f,filename); reset(f,1);
    blockread(f,TreeQty,4);
    blockread(f,TreePal,TreeQty);
    for ii:=1 to TreeQty do
      if TreePal[ii]=1 then
      begin
        blockread(f,TreeSize[ii],4);
        blockread(f,TreePivot[ii],8);
        setlength(TreeData[ii],TreeSize[ii,1]*TreeSize[ii,2]);
        blockread(f,TreeData[ii,0],TreeSize[ii,1]*TreeSize[ii,2]);
      end;
    closefile(f);
  end
else
  begin
    ShowMessage('Unable to locate ".\data\gfx\res\trees.rx" file');
    exit;
  end;
Result:=true;
end;

//=============================================
//Reading map elements (has animation data)
//=============================================
function ReadMapElem(filename:string):boolean;
begin
Result:=false;
if fileexists(filename) then
  begin
  assignfile(f,filename); reset(f,1);
  //for ii:=1 to 254 do
  blockread(f,MapElem[1],MapElemQty*99); //256*3
  closefile(f);
  end
else
  begin
    ShowMessage('Unable to locate ".\data\defines\mapelem.dat" file');
    exit;
  end;
Result:=true;
end;

//=============================================
//Adding all objects to ImageList in right order
//=============================================
function AddObjectsToImageList():boolean;
var MyBitMap,MyBitMapA:TBitMap;
    ii,id:integer;
    sy,sx:integer;
    y,x:integer;
begin
for ii:=1 to length(ObjIndex) do
  begin
  //for ii:=1 to MapElemQty do begin //1..254
  MyBitMap:=TBitMap.Create;
  MyBitmap.Height:=128;
  MyBitmap.Width:=128;
  MyBitMapA:=TBitMap.Create;
  MyBitmapA.Height:=128;
  MyBitmapA.Width:=128;
  MyBitmap.PixelFormat:=pf24bit;

  //id:=MapElem[ii].Tree[1]+1;
  id:=ObjIndexGFX[ii];

  sy:=TreeSize[id,2]; sy:=min(sy,128);
  sx:=TreeSize[id,1]; sx:=min(sx,128);
  for y:=0 to sy-1 do for x:=0 to sx-1 do begin
  MyBitmap.Canvas.Pixels[x,y]:=Pal0[TreeData[id,y*sx+x]+1,1]
                              +Pal0[TreeData[id,y*sx+x]+1,2]*256
                              +Pal0[TreeData[id,y*sx+x]+1,3]*65536;
  if TreeData[id,y*sx+x]<>0 then
  MyBitMapA.Canvas.Pixels[x,y]:=255 else MyBitMapA.Canvas.Pixels[x,y]:=0;
  end;
  if sy>0 then Form1.ImageList1.Add(MyBitmap,MyBitmapA);

  end;
Result:=true;
end;

//=============================================
//Reading houses data
//=============================================
function ReadHousesRX(filename:string):boolean;
var ii:integer;
begin
Result:=false;
if fileexists(filename) then begin
assignfile(f,filename); reset(f,1);
blockread(f,HouseQty,4);
blockread(f,HousePal,HouseQty);
for ii:=1 to HouseQty do
if HousePal[ii]=1 then begin //entry used
inc(HouseBMP[0]);
HouseBMP[ii]:=HouseBMP[0];
blockread(f,HouseSize[ii],4);
blockread(f,HousePivot[ii],8); 
setlength(HouseData[ii],HouseSize[ii,1]*HouseSize[ii,2]);
blockread(f,HouseData[ii,0],HouseSize[ii,1]*HouseSize[ii,2],NumRead);
end;
closefile(f);
end else begin
ShowMessage('Unable to locate ".\data\gfx\res\houses.rx" file');
exit;
end;
Result:=true;
end;

//=============================================
//Adding all houses to ImageList in right order
//=============================================
function AddHousesToImageList():boolean;
var MyBitMap,MyBitMapA:TBitMap;
    ii,id:integer;
    sy,sx:integer;
    y,x:integer;
    row:integer;
begin
row:=1; //row in ObjPallete
for ii:=1 to 29 do
if ii<>27 then
begin
MyBitMap:=TBitMap.Create;
MyBitmap.Height:=256;
MyBitmap.Width:=256;
MyBitMapA:=TBitMap.Create;
MyBitmapA.Height:=256;
MyBitmapA.Width:=256;
MyBitmap.PixelFormat:=pf24bit;

id:=ii;
HouseID[row]:=ii;

sy:=HouseSize[id,2]; if sy>256 then sy:=256;
sx:=HouseSize[id,1]; if sx>256 then sx:=256;
for y:=0 to sy-1 do for x:=0 to sx-1 do begin
MyBitmap.Canvas.Pixels[x,y]:=Pal0[HouseData[id,y*HouseSize[id,1]+x]+1,1]
                            +Pal0[HouseData[id,y*HouseSize[id,1]+x]+1,2]*256
                            +Pal0[HouseData[id,y*HouseSize[id,1]+x]+1,3]*65536;
if HouseData[id,y*HouseSize[id,1]+x]<>0 then
MyBitMapA.Canvas.Pixels[x,y]:=255 else MyBitMapA.Canvas.Pixels[x,y]:=0;
end;
if sy>0 then Form1.ImageList2.Add(MyBitmap,MyBitmapA);

Form1.HousePallete.RowCount:=row;
Form1.HousePallete.RowHeights[row-1]:=sy;

inc(row);
end;
Form1.HousePallete.RowCount:=row-1;
Form1.HousePalleteScroll.Max:=row-2;
Result:=true;
end;

//=============================================
//Reading units data
//=============================================
function ReadUnitsRX(filename:string):boolean;
var ii:integer;
begin
Result:=false;
FormLoading.Label1.Caption:='Reading units';
FormLoading.Bar1.StepIt; FormLoading.Refresh;
if fileexists(filename) then begin
assignfile(f,filename); reset(f,1);
blockread(f,UnitQty,4);
blockread(f,UnitPal,UnitQty);
for ii:=1 to UnitQty do
if UnitPal[ii]=1 then begin //entry used
blockread(f,UnitSize[ii],4);
blockread(f,UnitPivot[ii],8);
setlength(UnitData[ii],UnitSize[ii,1]*UnitSize[ii,2]);
blockread(f,UnitData[ii,0],UnitSize[ii,1]*UnitSize[ii,2],NumRead);
if UnitSize[ii,1]*UnitSize[ii,2]<>NumRead then
s:='0';
end;
closefile(f);
end else begin
ShowMessage('Units loading error.'+zz+'Place KaM Editor into KaM folder');
exit;
end;
Result:=true;
end;

//=============================================
//Reading unit.dat data
//=============================================
function ReadUnitDAT(filename:string):boolean;
var ii,kk,jj,hh:integer;
begin
Result:=false;
if fileexists(filename) then begin
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

assignfile(ft,ExeDir+'Units.txt'); rewrite(ft);
for ii:=1 to 40 do begin
writeln(ft);
writeln(ft);
writeln(ft,'NewUnit'+inttostr(ii));
for kk:=1 to 14 do
for hh:=1 to 8 do
  if UnitSprite[ii].Act[kk].Dir[hh].Step[1]>0 then
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

end else begin
ShowMessage('UnitDAT loading error.'+zz+'Place KaM Editor into KaM folder');
exit;
end;
Result:=true;
end;

//=============================================
//Reading houses.dat data
//=============================================
function ReadHouseDAT(filename:string):boolean;
var ii,kk:integer;
begin
Result:=false;
if fileexists(filename) then begin
assignfile(f,filename); reset(f,1);
blockread(f,HouseDAT1,30*70);
for ii:=1 to 29 do begin
blockread(f,HouseDAT[ii],88+19*70+270);
end;
closefile(f);

assignfile(ft,ExeDir+'Houses.txt'); rewrite(ft);
  for ii:=1 to 30 do begin
  for kk:=1 to 35 do
  if HouseDAT1[ii,kk]+1>=1 then
  if HouseDAT1[ii,kk]+1<=2000 then
  write(ft,inttostr(HouseBMP[HouseDAT1[ii,kk]+1])+' ');
//  write(ft,inttostr(HouseDAT1[ii,kk]+1)+' ');
  writeln(ft);
  end;

for ii:=1 to 29 do begin
writeln(ft);
writeln(ft);
writeln(ft,'NewHouse'+HouseName[ii]);
  for kk:=1 to 4 do if HouseDAT[ii].SupplyIn[kk,1]>0 then
  write(ft,'#') else write(ft,' ');
  writeln(ft);
  for kk:=1 to 4 do if HouseDAT[ii].SupplyOut[kk,1]>0 then
  write(ft,'#') else write(ft,' ');
  writeln(ft);
  for kk:=1 to 135 do
  write(ft,inttostr(HouseDAT[ii].Foot[kk]+1)+' ');
  writeln(ft);
{  for kk:=1 to 135 do
  if HouseDAT[ii].Foot[kk]+1>=1 then
  if HouseDAT[ii].Foot[kk]+1<=2000 then
  write(ft,inttostr(HouseBMP[HouseDAT[ii].Foot[kk]+1])+' ');
  writeln(ft);}
end;
closefile(ft);

end else begin
ShowMessage('HouseDAT loading error.'+zz+'Place KaM Editor into KaM folder');
exit;
end;
Result:=true;
end;

//=============================================
//Adding all units to ImageList in right order
//=============================================
function AddUnitsToImageList():boolean;
var MyBitMap,MyBitMapA:TBitMap;
    ii,id:integer;
    sy,sx:integer;
    y,x:integer;
    row:integer;
begin
row:=1; //row in ObjPallete
Form1.UnitPallete.ColCount:=1;
Form1.UnitPallete.DefaultColWidth:=128;
for ii:=1669 to 1710 do
begin

MyBitMap:=TBitMap.Create;
MyBitmap.Height:=128;
MyBitmap.Width:=128;
MyBitMapA:=TBitMap.Create;
MyBitmapA.Height:=128;
MyBitmapA.Width:=128;
MyBitmap.PixelFormat:=pf24bit;

id:=ii;
UnitID[row]:=ii;

sy:=UnitSize[id,2]; if sy>128 then sy:=128;
sx:=UnitSize[id,1]; if sx>128 then sx:=128;
for y:=0 to sy-1 do for x:=0 to sx-1 do begin
MyBitmap.Canvas.Pixels[x,y]:=Pal0[UnitData[id,y*UnitSize[id,1]+x]+1,1]
                            +Pal0[UnitData[id,y*UnitSize[id,1]+x]+1,2]*256
                            +Pal0[UnitData[id,y*UnitSize[id,1]+x]+1,3]*65536;
if UnitData[id,y*UnitSize[id,1]+x]<>0 then
MyBitMapA.Canvas.Pixels[x,y]:=255 else MyBitMapA.Canvas.Pixels[x,y]:=0;
end;
if sy>0 then Form1.ImageList3.Add(MyBitmap,MyBitmapA);

Form1.UnitPallete.RowCount:=row;
Form1.UnitPallete.RowHeights[row-1]:=sy;

inc(row);
end;
Form1.UnitPallete.RowCount:=row-1;
Form1.UnitPalleteScroll.Max:=row-2;

Result:=true;
end;

//=============================================
//Making OpenGL textures out of objects
//=============================================
procedure MakeObjectsGFX(Sender: TObject);
var h,i,k,mx,my,id:integer; TD:Pointer;  by:^cardinal;
    DestX,DestY,Am,Rm:integer;
begin
Am:=0; Rm:=0;
for h:=1 to length(ObjIndex) do begin
id:=ObjIndexGFX[h];
if id=0 then id:=81;
mx:=TreeSize[id,1];
my:=TreeSize[id,2];
DestX:=MakePOT(mx);
DestY:=MakePOT(my);

TD:=AllocMem(DestX*DestY*4); //GetMem+FillChar(0)
for i:=0 to (DestY-1) do for k:=0 to (DestX-1) do
if (i<my)and(k<mx)and(TreeData[id,i*TreeSize[id,1]+k]<>0) then begin
by:=pointer(integer(TD)+((i+DestY-my)*DestX+k)*4); //Get pointer
by^:=Pal0[TreeData[id,i*TreeSize[id,1]+k]+1,1] //Assign color
    +Pal0[TreeData[id,i*TreeSize[id,1]+k]+1,2] SHL 8
    +Pal0[TreeData[id,i*TreeSize[id,1]+k]+1,3] SHL 16
    OR $FF000000;
end;

TreeTex[id,2]:=DestX;
TreeTex[id,3]:=DestY;

glGenTextures(1, @TreeTex[id,1]);
glBindTexture(GL_TEXTURE_2D, TreeTex[id,1]);
glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
gluBuild2DMipmaps(GL_TEXTURE_2D, GL_RGBA, DestX, DestY, GL_RGBA, GL_UNSIGNED_BYTE, TD);
FreeMem(TD);
//setlength(TreeData[id],0);
inc(Am,DestX*DestY*4);
inc(Rm,TreeSize[id,1]*TreeSize[id,2]*4);
end;
fLog.AppendLog(Am div 1024, 'Kbytes allocated for trees GFX');
fLog.AppendLog((Am-Rm) div 1024, 'Kbytes wasted for trees GFX');
end;

//=============================================
//Making OpenGL textures out of houses
//=============================================
procedure MakeHousesGFX(Sender: TObject);
var h,i,k,mx,my,id:integer; TD:Pointer;  by:^cardinal;
    DestX,DestY,Am,Rm:integer;
begin
Am:=0; Rm:=0;
for h:=1 to HouseQty do begin

id:=h;//HouseIndexGFX[h];
mx:=HouseSize[id,1];
my:=HouseSize[id,2];
DestX:=MakePOT(mx);
DestY:=MakePOT(my);

TD:=AllocMem(DestX*DestY*4); //GetMem+FillChar(0)
for i:=0 to (DestY-1) do for k:=0 to (DestX-1) do
if (i<my)and(k<mx)and(HouseData[id,i*HouseSize[id,1]+k]<>0) then begin
by:=pointer(integer(TD)+((i+DestY-my)*DestX+k)*4); //Get pointer
by^:=Pal0[HouseData[id,i*HouseSize[id,1]+k]+1,1] //Assign color
    +Pal0[HouseData[id,i*HouseSize[id,1]+k]+1,2] SHL 8
    +Pal0[HouseData[id,i*HouseSize[id,1]+k]+1,3] SHL 16
    OR $FF000000;
end;

HouseTex[id,2]:=DestX;
HouseTex[id,3]:=DestY;

glGenTextures(1, @HouseTex[h,1]);
glBindTexture(GL_TEXTURE_2D, HouseTex[h,1]);
glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
gluBuild2DMipmaps(GL_TEXTURE_2D, GL_RGBA2, DestX, DestY, GL_RGBA, GL_UNSIGNED_BYTE, TD);
FreeMem(TD);
setlength(HouseData[id],0);
inc(Am,DestX*DestY*4);
inc(Rm,HouseSize[id,1]*HouseSize[id,2]*4);
end;
fLog.AppendLog(Am div 1024, 'Kbytes allocated for houses GFX');
fLog.AppendLog((Am-Rm) div 1024, 'Kbytes wasted for houses GFX');
end;

//=============================================
//Making OpenGL textures out of units
//=============================================
procedure MakeUnitsGFX(Sender: TObject);
var i,k,mx,my,id:integer; TD:Pointer;  by:^cardinal;
    DestX,DestY,Am,Rm:integer;
begin
Am:=0; Rm:=0;
for id:=1 to UnitQty do begin

mx:=UnitSize[id,1];
my:=UnitSize[id,2];
DestX:=MakePOT(mx);
DestY:=MakePOT(my);

TD:=AllocMem(DestX*DestY*4); //GetMem+FillChar(0)
for i:=0 to (DestY-1) do for k:=0 to (DestX-1) do
if (i<my)and(k<mx)and(UnitData[id,i*UnitSize[id,1]+k]<>0) then begin
by:=pointer(integer(TD)+((i+DestY-my)*DestX+k)*4); //Get pointer
by^:=Pal0[UnitData[id,i*UnitSize[id,1]+k]+1,1] //Assign color
    +Pal0[UnitData[id,i*UnitSize[id,1]+k]+1,2] SHL 8
    +Pal0[UnitData[id,i*UnitSize[id,1]+k]+1,3] SHL 16
    OR $FF000000;
end;

UnitTex[id,2]:=DestX;
UnitTex[id,3]:=DestY;

glGenTextures(1, @UnitTex[id,1]);
glBindTexture(GL_TEXTURE_2D, UnitTex[id,1]);
glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
//gluBuild2DMipmaps(GL_TEXTURE_2D, GL_RGBA, DestX, DestY, GL_RGBA, GL_UNSIGNED_BYTE, TD);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
  glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MAX_LEVEL, 0);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP);
  glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA2, DestX, DestY, 0, GL_RGBA, GL_UNSIGNED_BYTE, TD);
//glTexImage2D(GL_TEXTURE_2D, 0, GL_COLOR_INDEX8_EXT, DestX, DestY, 0, GL_COLOR_INDEX, GL_BITMAP, TD);
FreeMem(TD);
setlength(UnitData[id],0);
inc(Am,DestX*DestY*4);
inc(Rm,UnitSize[id,1]*UnitSize[id,2]*4);
end;
fLog.AppendLog(Am div 1024, 'Kbytes allocated for units GFX');
fLog.AppendLog((Am-Rm) div 1024, 'Kbytes wasted for units GFX');
end;




end.
