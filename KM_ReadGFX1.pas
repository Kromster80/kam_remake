unit KM_ReadGFX1;
interface
uses OpenGL;

type
  TByteArray2 = array of Byte;
  TWordArray2 = array of Word;

    function ReadGFX(text: string):boolean;
    function ReadPallete(filename:string):boolean;
    function ReadTreesRX(filename:string):boolean;
    function ReadHousesRX(filename:string):boolean;
    function ReadUnitsRX(filename:string):boolean;
    function ReadGUIRX(filename:string):boolean;
    function ReadGUIMainRX(filename:string):boolean;
    function ReadUnitDAT(filename:string):boolean;
    function ReadHouseDAT(filename:string):boolean;
    function ReadMapElem(filename:string):boolean;
    procedure ExportRX2BMP(Param:string);
    procedure MakeObjectsGFX(Sender: TObject);
    procedure MakeHousesGFX(Sender: TObject);
    procedure MakeUnitsGFX(Sender: TObject);
    procedure MakeGUIGFX(Sender: TObject);

implementation

uses KromUtils, KM_Unit1, KM_Defaults, KM_Form_Loading,  Graphics, Sysutils, Dialogs, math, dglOpenGL,
     KM_Global_Data, KM_Log;

function ReadGFX(text: string):boolean;
        procedure StepRefresh();
          begin
            FormLoading.Bar1.StepIt; FormLoading.Refresh;
          end;
begin
fLog.AppendLog('Reading pal0.bbm',ReadPallete(text+'data\gfx\pal0.bbm'));           StepRefresh();
fLog.AppendLog('Reading trees.rx',ReadTreesRX(text+'data\gfx\res\trees.rx'));       StepRefresh();
fLog.AppendLog('Reading houses.rx',ReadHousesRX(text+'data\gfx\res\houses.rx'));    StepRefresh();
fLog.AppendLog('Reading units.rx',ReadUnitsRX(text+'data\gfx\res\units.rx'));       StepRefresh();
fLog.AppendLog('Reading gui.rx',ReadGUIRX(text+'data\gfx\res\gui.rx'));             StepRefresh();
fLog.AppendLog('Reading guimain.rx',ReadGUIMainRX(text+'data\gfx\res\guimain.rx')); StepRefresh();
fLog.AppendLog('Reading mapelem.dat',ReadMapElem(text+'data\defines\mapelem.dat')); StepRefresh();
fLog.AppendLog('Reading houses.dat',ReadHouseDAT(text+'data\defines\houses.dat'));  StepRefresh();
fLog.AppendLog('Reading unit.dat',ReadUnitDAT(text+'data\defines\unit.dat'));       StepRefresh();

    MakeObjectsGFX(nil);
    MakeHousesGFX(nil);
    MakeUnitsGFX(nil);
    MakeGUIGFX(nil);

fLog.AppendLog('ReadGFX is done');

Result:=true;
end;

//=============================================
//Reading pallete for trees/objects
//=============================================
function ReadPallete(filename:string):boolean;
begin
Result:=false;
if not CheckFileExists(filename) then exit;
  assignfile(f,filename);
  reset(f,1);
  blockread(f,c,48);
  blockread(f,Pal0,768); //256*3
  closefile(f);
Result:=true;
end;

//=============================================
//Reading trees/objects data
//=============================================
function ReadTreesRX(filename:string):boolean;
var ii:integer;
begin
  Result:=false;
  if not CheckFileExists(filename) then exit;
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
  Result:=true;
end;

//=============================================
//Reading map elements (has animation data)
//=============================================
function ReadMapElem(filename:string):boolean;
begin
  Result:=false;
  if not CheckFileExists(filename) then exit;
  assignfile(f,filename); reset(f,1);
  blockread(f,MapElem[1],MapElemQty*99); //256*3
  closefile(f);
  Result:=true;
end;

//=============================================
//Reading houses data
//=============================================
function ReadHousesRX(filename:string):boolean;
var ii,Count:integer;
begin
Result:=false;
Count:=0;
if not CheckFileExists(filename) then exit;
assignfile(f,filename); reset(f,1);
blockread(f,HouseQty,4);
blockread(f,HousePal,HouseQty);
for ii:=1 to HouseQty do
if HousePal[ii]=1 then begin //entry used
inc(Count);
inc(HouseBMP[0]);
HouseBMP[ii]:=HouseBMP[0];
blockread(f,HouseSize[ii],4);
blockread(f,HousePivot[ii],8);
setlength(HouseData[ii],HouseSize[ii,1]*HouseSize[ii,2]);
blockread(f,HouseData[ii,0],HouseSize[ii,1]*HouseSize[ii,2]);
end;
closefile(f);
fLog.AppendLog('House sprites 2000 -',Count);
Result:=true;
end;

//=============================================
//Reading units data
//=============================================
function ReadUnitsRX(filename:string):boolean;
var ii,Count:integer;
begin
Result:=false;
Count:=0;
FormLoading.Label1.Caption:='Reading units';
FormLoading.Bar1.StepIt; FormLoading.Refresh;
if not CheckFileExists(filename) then exit;
assignfile(f,filename); reset(f,1);
blockread(f,UnitQty,4);
blockread(f,UnitPal,UnitQty);
for ii:=1 to UnitQty do
if UnitPal[ii]=1 then begin //entry used
inc(Count);
blockread(f,UnitSize[ii],4);
blockread(f,UnitPivot[ii],8);
setlength(UnitData[ii],UnitSize[ii,1]*UnitSize[ii,2]);
blockread(f,UnitData[ii,0],UnitSize[ii,1]*UnitSize[ii,2]);
end;
closefile(f);
fLog.AppendLog('Unit sprites 9500 -',Count);
Result:=true;
end;

//=============================================
//Reading GUI data
//=============================================
function ReadGUIRX(filename:string):boolean;
var ii,Count:integer;
begin
Result:=false;
Count:=0;
FormLoading.Label1.Caption:='Reading GUI';
FormLoading.Bar1.StepIt; FormLoading.Refresh;
if not CheckFileExists(filename) then exit;
assignfile(f,filename); reset(f,1);
blockread(f,GUIQty,4);
blockread(f,GUIPal,GUIQty);
for ii:=1 to GUIQty do
if GUIPal[ii]=1 then begin //entry used
inc(Count);
blockread(f,GUISize[ii],4);
blockread(f,GUIPivot[ii],8);
setlength(GUIData[ii],GUISize[ii,1]*GUISize[ii,2]);
blockread(f,GUIData[ii,0],GUISize[ii,1]*GUISize[ii,2]);
end;
closefile(f);
fLog.AppendLog('GUI sprites 600 -',Count);
Result:=true;
end;

//=============================================
//Reading GUIMain data
//=============================================
function ReadGUIMainRX(filename:string):boolean;
var ii,Count:integer;
begin
Result:=false;
Count:=0;
FormLoading.Label1.Caption:='Reading GUIMain';
FormLoading.Bar1.StepIt; FormLoading.Refresh;
if not CheckFileExists(filename) then exit;
assignfile(f,filename); reset(f,1);
blockread(f,GUIMQty,4);
blockread(f,GUIMPal,GUIMQty);
for ii:=1 to GUIMQty do
if GUIMPal[ii]=1 then begin //entry used
inc(Count);
blockread(f,GUIMSize[ii],4);
blockread(f,GUIMPivot[ii],8);
setlength(GUIMData[ii],GUIMSize[ii,1]*GUIMSize[ii,2]);
blockread(f,GUIMData[ii,0],GUIMSize[ii,1]*GUIMSize[ii,2]);
end;
closefile(f);
fLog.AppendLog('GUIMain sprites -',Count);
Result:=true;
end;

//=============================================
//Reading unit.dat data
//=============================================
function ReadUnitDAT(filename:string):boolean;
var ii,kk,jj,hh:integer;
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
//Reading houses.dat data
//=============================================
function ReadHouseDAT(filename:string):boolean;
var ii,kk:integer;
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
//Export
//=============================================
procedure ExportRX2BMP(Param:string);
var MyBitMap:TBitMap;
    id,t,Qty:integer;
    sy,sx,y,x:integer;
begin
CreateDir(ExeDir+Param+'rx\');
  MyBitMap:=TBitMap.Create;
  MyBitmap.PixelFormat:=pf24bit;
    if Param='Trees'  then Qty:=TreeQty;
    if Param='Houses' then Qty:=HouseQty;
    if Param='Units'  then Qty:=UnitQty;
    if Param='GUI'    then Qty:=GUIQty;
    if Param='GUIMain'then Qty:=GUIMQty;

    if Param='Trees'  then ReadTreesRX(ExeDir+'data\gfx\res\trees.rx');
    if Param='Houses' then ReadHousesRX(ExeDir+'data\gfx\res\houses.rx');
    if Param='Units'  then ReadUnitsRX(ExeDir+'data\gfx\res\units.rx');
    if Param='GUI'    then ReadGUIRX(ExeDir+'data\gfx\res\gui.rx');
    if Param='GUIMain'then ReadGUIMainRX(ExeDir+'data\gfx\res\guimain.rx');

for id:=1 to Qty do
begin
  if Param='Trees'  then begin sx:=TreeSize[id,1];  sy:=TreeSize[id,2];  end;
  if Param='Houses' then begin sx:=HouseSize[id,1]; sy:=HouseSize[id,2]; end;
  if Param='Units'  then begin sx:=UnitSize[id,1];  sy:=UnitSize[id,2];  end;
  if Param='GUI'    then begin sx:=GUISize[id,1];   sy:=GUISize[id,2];   end;
  if Param='GUIMain'then begin sx:=GUIMSize[id,1];  sy:=GUIMSize[id,2];  end;

  MyBitmap.Width:=sx;
  MyBitmap.Height:=sy;

  for y:=0 to sy-1 do for x:=0 to sx-1 do begin
    if Param='Trees'  then t:=TreeData[id,y*sx+x]+1;
    if Param='Houses' then t:=HouseData[id,y*sx+x]+1;
    if Param='Units'  then t:=UnitData[id,y*sx+x]+1;
    if Param='GUI'    then t:=GUIData[id,y*sx+x]+1;
    if Param='GUIMain'then t:=GUIMData[id,y*sx+x]+1;
    MyBitmap.Canvas.Pixels[x,y]:=Pal0[t,1]+Pal0[t,2]*256+Pal0[t,3]*65536;
  end;
  if sy>0 then MyBitmap.SaveToFile(ExeDir+Param+'rx\'+Param+'_'+int2fix(id,4)+'.bmp');

  if Param='Trees'  then setlength(TreeData[id],0);
  if Param='Houses' then setlength(HouseData[id],0);
  if Param='Units'  then setlength(UnitData[id],0);
  if Param='GUI'    then setlength(GUIData[id],0);
  if Param='GUIMain'then setlength(GUIMData[id],0);
end;
end;

//=============================================
//Make texture
//=============================================
procedure GenTexture(ID:PGLUint; mx, my:integer; Data:TByteArray2; Mode:string);
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

TD:=AllocMem(DestX*DestY*4); //GetMem+FillChar(0)
for i:=0 to (DestY-1) do for k:=0 to (DestX-1) do
  if (i<my)and(k<mx) then begin
    x:=Data[i*mx+k];
    if (x<>0) then begin
      by:=pointer(integer(TD)+((i+DestY-my)*DestX+k)*4); //Get pointer
      if Mode='Norm' then
        if x in [24..30] then
          col:=((x-27)*42+128)*65793 OR $FF000000 //convert to greyscale B>>>>>W
        else
          col:=Pal0[x+1,1]+Pal0[x+1,2] SHL 8 +Pal0[x+1,3] SHL 16 OR $FF000000
      else
        case x of
          24,30: col:=$70FFFFFF;   //7
          25,29: col:=$B0FFFFFF;   //11
          26,28: col:=$E0FFFFFF;   //14
          27: col:=$FFFFFFFF;      //16
        else
          col:=0;
        end;
      by^:=col;
    end;
  end;

glGenTextures(1, id);
//if id^<=16000 then
begin
glBindTexture(GL_TEXTURE_2D, id^);
//gluBuild2DMipmaps(GL_TEXTURE_2D, GL_RGBA, DestX, DestY, GL_RGBA, GL_UNSIGNED_BYTE, TD);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
  glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MAX_LEVEL, 0);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP);
  if Mode='Norm' then
    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGB5_A1, DestX, DestY, 0, GL_RGBA, GL_UNSIGNED_BYTE, TD)
  else
    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA2, DestX, DestY, 0, GL_RGBA, GL_UNSIGNED_BYTE, TD);
end;
FreeMem(TD);
end;

//=============================================
//Making OpenGL textures out of objects
//=============================================
procedure MakeObjectsGFX(Sender: TObject);
var id:integer; Am,Rm:integer;
begin
Am:=0; Rm:=0;
for id:=1 to TreeQty do begin
TreeTex[id,2]:=MakePOT(TreeSize[id,1]);
TreeTex[id,3]:=MakePOT(TreeSize[id,2]);

if TreeSize[id,1]*TreeSize[id,2]<>0 then
GenTexture(@TreeTex[id,1],TreeSize[id,1],TreeSize[id,2],@TreeData[id,0],'Norm');

setlength(TreeData[id],0);
inc(Am,TreeTex[id,2]*TreeTex[id,3]*4);
inc(Rm,TreeSize[id,1]*TreeSize[id,2]*4);
end;
fLog.AppendLog(inttostr(Am div 1024)+'/'+inttostr((Am-Rm) div 1024)+' Kbytes allocated/wasted for trees GFX');
end;

//=============================================
//Making OpenGL textures out of houses
//=============================================
procedure MakeHousesGFX(Sender: TObject);
var i,id:integer; Am,Cm,Rm:integer;
begin
Am:=0; Rm:=0; Cm:=0;
for id:=1 to HouseQty do
  if HouseSize[id,1]*HouseSize[id,2]<>0 then
    begin
      HouseTex[id,2]:=MakePOT(HouseSize[id,1]);
      HouseTex[id,3]:=MakePOT(HouseSize[id,2]);

      GenTexture(@HouseTex[id,1],HouseSize[id,1],HouseSize[id,2],@HouseData[id,0],'Norm');

      for i:=0 to HouseSize[id,2]*HouseSize[id,1]-1 do
        if HouseData[id,i] in [24..30] then begin
          GenTexture(@HouseTex[id,4],HouseSize[id,1],HouseSize[id,2],@HouseData[id,0],'AltID');
          inc(Cm,HouseTex[id,2]*HouseTex[id,3]*4);
          break;
        end;

setlength(HouseData[id],0);
inc(Am,HouseTex[id,2]*HouseTex[id,3]*4);
inc(Rm,HouseSize[id,1]*HouseSize[id,2]*4);
end;
fLog.AppendLog(inttostr(Am div 1024)+'/'+inttostr((Am-Rm) div 1024)+' Kbytes allocated/wasted for houses GFX');
fLog.AppendLog(inttostr(Cm div 1024)+' KBytes for team colors');
end;

//=============================================
//Making OpenGL textures out of units
//=============================================
procedure MakeUnitsGFX(Sender: TObject);
var i,id:integer; Am,Cm,Rm:integer;
begin
Am:=0; Rm:=0; Cm:=0;
for id:=1 to UnitQty do
  if UnitSize[id,1]*UnitSize[id,2]<>0 then
    begin
      UnitTex[id,2]:=MakePOT(UnitSize[id,1]);
      UnitTex[id,3]:=MakePOT(UnitSize[id,2]);

      GenTexture(@UnitTex[id,1],UnitSize[id,1],UnitSize[id,2],@UnitData[id,0],'Norm');

      for i:=0 to UnitSize[id,2]*UnitSize[id,1]-1 do
        if UnitData[id,i] in [24..30] then begin
          GenTexture(@UnitTex[id,4],UnitSize[id,1],UnitSize[id,2],@UnitData[id,0],'AltID');
          inc(Cm,UnitTex[id,2]*UnitTex[id,3]*4);
          break;
        end;

      setlength(UnitData[id],0);
      inc(Am,UnitTex[id,2]*UnitTex[id,3]*4);
      inc(Rm,UnitSize[id,1]*UnitSize[id,2]*4);
    end;
fLog.AppendLog(inttostr(Am div 1024)+'/'+inttostr((Am-Rm) div 1024)+' Kbytes allocated/wasted for units GFX');
fLog.AppendLog(inttostr(Cm div 1024)+' KBytes for team colors');
end;

//=============================================
//Making OpenGL textures out of GUI
//=============================================
procedure MakeGUIGFX(Sender: TObject);
var id:integer; Am,Rm:integer;
begin
Am:=0; Rm:=0;
for id:=1 to GUIQty do begin
GUITex[id].TexW:=MakePOT(GUISize[id,1]);
GUITex[id].TexH:=MakePOT(GUISize[id,2]);

GUITexUV[id].Left:=0;
GUITexUV[id].Right:=GUISize[id,1];
GUITexUV[id].Top:=GUITex[id].TexH-GUISize[id,2];
GUITexUV[id].Bottom:=GUITex[id].TexH;

if GUISize[id,1]*GUISize[id,2]<>0 then
GenTexture(@GUITex[id].TexID,GUISize[id,1],GUISize[id,2],@GUIData[id,0],'Norm');

setlength(GUIData[id],0);
inc(Am,GUITex[id].TexW*GUITex[id].TexH*4);
inc(Rm,GUISize[id,1]*GUISize[id,2]*4);
end;
fLog.AppendLog(inttostr(Am div 1024)+'/'+inttostr((Am-Rm) div 1024)+' Kbytes allocated/wasted for GUIs GFX');
end;



end.
