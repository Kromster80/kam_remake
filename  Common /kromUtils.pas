//some new lines
unit KromUtils;
interface
uses sysutils,windows,forms,typinfo,ExtCtrls,Math, Dialogs;

type
  PSingleArray = ^TSingleArray;
  TSingleArray = array[1..1024000] of Single;
  PStringArray = ^TStringArray;
  TStringArray = array[1..256] of String;
  Vector4f = record X,Y,Z,W:single; end;
  Vector3f = record X,Y,Z:single; end;
  Vector2f = record U,V:single; end;
  PVector3f = ^Vector3f;

  TKMPoint = record X,Y:word; end;
  TKMPointf = record X,Y:single; end;

  TKMouseButton = (kmb_None, kmb_Left, kmb_Right, kmb_Middle);

  TKMDirection = (dir_NA=0, dir_N=1, dir_NE=2, dir_E=3, dir_SE=4, dir_S=5, dir_SW=6, dir_W=7, dir_NW=8);

function KMPoint(X, Y: Integer): TKMPoint;
function KMPointY1(P:TKMPoint): TKMPoint; overload
function KMPointY1(P:TKMPointF): TKMPoint; overload
function ElapsedTime(i1: pcardinal): string;
function ExtractOpenedFileName(in_s: string):string;
function GetFileExt (const FileName: string; len:integer=3): string;
function AssureFileExt(FileName,Ext:string): string;
function GetFileSize(const FileName: string): LongInt;
function CheckFileExists(const FileName: string):boolean;

procedure krintersect(x1,y1,x2,y2,x3,y3:single; SizeX,SizeY:integer; var ot:array of integer);
function ReverseString(s1:string):string;

function real2(c1,c2,c3,c4:char):real;
function unreal2(x:real):string;
function hextoint(st: char): integer;
function int2fix(Number,Len:integer):string;
function float2fix(Number:single; Digits:integer):string;
function int2(c1,c2:char):integer; overload;
function int2(c1,c2,c3,c4:char):integer; overload;
function chr2(x,len:integer):string; overload;
function chr2(t:string; len:integer):string; overload;
procedure Color2RGB(Col:integer; out R,G,B:byte);

function Vectorize(A,B:single):Vector2f; overload;
function Vectorize(A,B,C:single):Vector3f; overload;

function Min(const A,B,C: integer):integer; overload
function Min(const A,B,C: single):single; overload
function Max(const A,B,C: integer):integer; overload
function Max(const A,B,C: single):single; overload
function Ceil(const X: Extended):Integer;
function ArcCos(const X: Extended): Extended;
function ArcSin(const X: Extended): Extended;
function ArcTan2(const Y, X: Extended): Extended;
function Pow(const Base, Exponent: integer): integer;

  function GetLengthSQR(ix,iy,iz:integer): integer;
  function GetLength(ix,iy,iz:single): single; overload
  function GetLength(ix:Vector3f): single; overload  
  function GetLength(ix,iy:single): single; overload

  function InBetween(A,B,X:single): boolean;

  procedure Normalize(ix,iy,iz:single; nx,ny,nz:psingle); overload
  procedure Normalize(var ix,iy,iz:single); overload
  procedure Normalize(var v:Vector3f); overload
  procedure Normalize(var ix,iy:single); overload

  function Mix(x1,x2,MixValue:single):single; overload
  function Mix(x1,x2:integer; MixValue:single):integer; overload
  function Mix(x1,x2:Vector3f; MixValue:single):vector3f; overload

  procedure Matrix2Angles(matrix09:array of single; Qty:integer; i1,i2,i3:pinteger);
  procedure Angles2Matrix(ax,ay,az:single; matrix:pointer; Qty:integer);

  procedure Angles2Vector(degreeX,degreeY,iz:single; out nx,ny,nz:single);

  function DotProduct(x1,y1,z1,x2,y2,z2:single):single; overload
  function DotProduct(v1,v2:Vector3f):single; overload
  procedure Normal2Poly(v1,v2,v3:array of single; nx,ny,nz:psingle); overload
  procedure Normal2Poly(v1,v2,v3:Vector3f; n:PVector3f); overload
  procedure Normal2Poly(u1,v1,u2,v2,u3,v3:single; out n:single); overload

procedure decs(var AText:string; const Len:integer=1); overload;
procedure decs(var AText:widestring; const Len:integer=1); overload;
function  decs(AText:string; Len,RunAsFunction:integer):string; overload;
function GetNumberFromString(AText:string; Position:integer):single;
procedure SwapStr(var A,B:string);
procedure SwapInt(var A,B:word);
procedure SwapFloat(var A,B:single);

procedure ConvertSetToArray(iSet:integer; Ar:pointer);
function WriteLWO(fname:string; PQty,VQty,SQty:integer; xyz:PSingleArray; uv:PSingleArray; v:PIntegerArray; Surf:PStringArray): boolean;
function MakePOT(num:integer):integer;
function Adler32CRC(TextPointer:Pointer; TextLength:integer):integer;
function RandomS(Range_Both_Directions:integer):integer; overload
function RandomS(Range_Both_Directions:single):single; overload
procedure WriteLangFile(Sender:TForm; FileName:string; EraseWritten:boolean);
procedure ReadLangFile(Sender:TForm; FileName:string; EraseWritten:boolean);
function RunOpenDialog(Sender:TOpenDialog; Name,Path,Filter:string):boolean;
function RunSaveDialog(Sender:TSaveDialog; FileName, FilePath, Filter:string; const FileExt:string = ''):boolean;

procedure Triangulate(VerticeCount:integer; Vertice:array of vector3f; out PolyCount:integer; out Polys:array of word; out Result:boolean);

const
  eol:string=#13+#10; //EndOfLine

implementation

function Vectorize(A,B:single):Vector2f; overload;
begin
Result.U:=A;
Result.V:=B;
end;

function Vectorize(A,B,C:single):Vector3f; overload;
begin
Result.X:=A;
Result.Y:=B;
Result.Z:=C;
end;       

function Min(const A,B,C: integer): integer; overload
begin if A < B then if A < C then Result := A else Result := C
               else if B < C then Result := B else Result := C;
end;

function Min(const A,B,C: single): single; overload
begin if A < B then if A < C then Result := A else Result := C
               else if B < C then Result := B else Result := C;
end;

function Max(const A,B,C: integer): integer; overload
begin if A > B then if A > C then Result := A else Result := C
               else if B > C then Result := B else Result := C;
end;

function Max(const A,B,C: single): single; overload
begin if A > B then if A > C then Result := A else Result := C
               else if B > C then Result := B else Result := C;
end;

function KMPoint(X, Y: Integer): TKMPoint;
begin
  Result.X := X;
  Result.Y := Y;
end;

function KMPointY1(P:TKMPoint): TKMPoint; overload
begin
  Result.X := P.X;
  Result.Y := P.Y+1;
end;

function KMPointY1(P:TKMPointF): TKMPoint; overload
begin
  Result.X := round(P.X);
  Result.Y := round(P.Y)+1;
end;

function WriteLWO(fname:string; PQty,VQty,SQty:integer; xyz:PSingleArray; uv:PSingleArray; v:PIntegerArray; Surf:PStringArray): boolean;
var ft:textfile; m:integer; ii:integer; res:string[4];
begin
assignfile(ft,fname); rewrite(ft);
m:=4;
inc(m,8);                                                                       //'LWO2'
if SQty=0 then inc(m,8);                                                        //TAGSxxxx
for ii:=1 to SQty do if length(string(@Surf[ii]))mod 2 = 1 then                 //
inc(m,length(string(@Surf[ii]))+1) else inc(m,length(string(@Surf[ii]))+2);     //Surface names listing
inc(m,8+18);                                                                    //LAYRxxxx,18
inc(m,8+PQty*12);                                                               //PNTS+3D

write(ft,'FORM',chr(m div 16777216),chr(m div 65536),chr(m div 256),chr(m),'LWO2');
m:=0;
for ii:=1 to SQty do if length(string(@Surf[ii]))mod 2 = 1 then                 //
inc(m,length(string(@Surf[ii]))+1) else inc(m,length(string(@Surf[ii]))+2);     //Surface names listing
if SQty=0 then m:=8;
write(ft,'TAGS',#0,#0,chr(m div 256),chr(m));
for ii:=1 to SQty do if length(string(@Surf[ii]))mod 2 = 1 then
write(ft,string(@Surf[ii]),#0) else write(ft,string(@Surf[ii]),#0,#0);
if SQty=0 then write(ft,'Default',#0);

write(ft,'LAYR',#0,#0,#0,#18,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0);

m:=PQty*12;
write(ft,'PNTS',chr(m div 16777216),chr(m div 65536),chr(m div 256),chr(m));
for ii:=1 to PQty do begin
res:=unreal2(xyz[ii*3-2]/10); write(ft,res[4],res[3],res[2],res[1]); //LWO uses
res:=unreal2(xyz[ii*3-1]/10); write(ft,res[4],res[3],res[2],res[1]); //reverse
res:=unreal2(xyz[ii*3-0]/10); write(ft,res[4],res[3],res[2],res[1]); //order
end;

closefile(ft);
result:=true;
end;

function ElapsedTime(i1:pcardinal): string;
begin
result:=' '+inttostr(GetTickCount-i1^)+'ms'; //get time passed
i1^:=GetTickCount;                           //assign new value to source
end;

function hextoint(st: char): integer;
begin st:=uppercase(st)[1];
if (ord(st)>=48)and(ord(st)<=57) then hextoint:=ord(st)-48 else
if (ord(st)>=65)and(ord(st)<=70) then hextoint:=ord(st)-55 else
hextoint:=0;
end;

function ExtractOpenedFileName(in_s: string):string;
var k:word; out_s:string; QMarks:boolean;
begin
k:=0; out_s:=''; QMarks:=false;

repeat      //First of all skip exe path
inc(k);
  if in_s[k]='"' then
  repeat inc(k);
  until(in_s[k]='"');
until((k>=length(in_s))or(in_s[k]=#32));  //in_s[k]=#32 now

inc(k);     //in_s[k]=" or first char

if (length(in_s)>k)and(in_s[k]=#32) then //Skip doublespace, WinXP bug ?
    repeat
    inc(k);
    until((k>=length(in_s))or(in_s[k]<>#32));

if (length(in_s)>k) then begin

    if in_s[k]='"' then begin
    inc(k); //Getting path from "...."
    QMarks:=true;
    end;

    repeat
    out_s:=out_s+in_s[k];
    inc(k);
    until((length(in_s)=k-1)or(in_s[k]='"')or((QMarks=false)and(in_s[k]=' ')));

end else out_s:='';

Result:=out_s;
end;

function GetFileExt(const FileName: string; len:integer=3): string;
var k:integer; s:string;
begin
if length(FileName)<=len then exit;
s:=''; k:=0;
repeat
inc(k); //thats correct
s:=s+FileName[length(FileName)-len+k];
until(len=k);
Result:=uppercase(s);
end;

function AssureFileExt(FileName,Ext:string): string;
begin
if (Ext='')or(GetFileExt(FileName,length(Ext))=UpperCase(Ext)) then
  Result:=FileName
else
  Result:=FileName+'.'+Ext;
end;

function GetFileSize(const FileName: string): LongInt;
var
  SearchRec: TSearchRec;
begin
  try
    if FindFirst(ExpandFileName(FileName), faAnyFile, SearchRec) = 0 then
      Result := SearchRec.Size
    else Result := -1;
  finally
    SysUtils.FindClose(SearchRec);
  end;
end;

function CheckFileExists(const FileName: string):boolean;
begin
if fileexists(FileName) then
  Result:=true
else begin
  ShowMessage('Unable to locate '+#13+'"'+FileName+'" file');
  Result:=false;
end;
end;

function ReverseString(s1:string):string;
var s2:string; i:integer;
begin
s2:=s1; //preparing ?
for i:=1 to length(s1) do
s2[i]:=s1[length(s1)-i+1];
ReverseString:=s2;
end;

procedure krintersect(x1,y1,x2,y2,x3,y3:single; SizeX,SizeY:integer; var ot:array of integer);
var Ax,Ay,Bx,By,Cx,Cy:single; i,k,j,tx,ty:integer;
v1,v2,w1,w2,r1,r2:single;
begin i:=0; Bx:=0; By:=0; Cx:=0; Cy:=0;
//Fit everything to limits
x1:=EnsureRange(x1/256,0,SizeX); y1:=EnsureRange(y1/256,0,SizeY);
x2:=EnsureRange(x2/256,0,SizeX); y2:=EnsureRange(y2/256,0,SizeY);
x3:=EnsureRange(x3/256,0,SizeX); y3:=EnsureRange(y3/256,0,SizeY);

if (trunc(x1)=trunc(x2))and(trunc(x2)=trunc(x3))               //All in one block
and(trunc(y1)=trunc(y2))and(trunc(y2)=trunc(y3)) then begin
ot[0]:=trunc(x1)+1+trunc(y1)*SizeX;
exit; end;          

if y1>y2 then begin                            //Sorting highest
 if y1>y3 then begin Ax:=x1; Ay:=y1; end
          else begin Ax:=x3; Ay:=y3; end       //
end else begin                                 //
 if y2>y3 then begin Ax:=x2; Ay:=y2; end       //
          else begin Ax:=x3; Ay:=y3; end
end;

if (Ax=x1)and(Ay=y1) then if y2>y3 then begin Bx:=x2; By:=y2; Cx:=x3; Cy:=y3; end
                       else begin Bx:=x3; By:=y3; Cx:=x2; Cy:=y2; end;
if (Ax=x2)and(Ay=y2) then if y1>y3 then begin Bx:=x1; By:=y1; Cx:=x3; Cy:=y3; end
                       else begin Bx:=x3; By:=y3; Cx:=x1; Cy:=y1; end;
if (Ax=x3)and(Ay=y3) then if y1>y2 then begin Bx:=x1; By:=y1; Cx:=x2; Cy:=y2; end
                       else begin Bx:=x2; By:=y2; Cx:=x1; Cy:=y1; end;
//A-highest
//B-middle
//C-lowest

if (Ay<>Cy)and(By<>Cy) then              //Below mid-line
for k:=trunc(Cy) to trunc(By)-1 do begin
v1:=Cx+(Min(k+1,Ay)-Cy)*(Ax-Cx)/(Ay-Cy);
w1:=Cx+(Min(k+1,By)-Cy)*(Bx-Cx)/(By-Cy); //upper edge limits
v2:=Cx+(Max(k,Cy)-Cy)*(Ax-Cx)/(Ay-Cy);
w2:=Cx+(Max(k,Cy)-Cy)*(Bx-Cx)/(By-Cy); //lower edge limits
for j:=trunc(Min(Min(v1,w1),Min(v2,w2))) to trunc(Max(Max(v1,w1),Max(v2,w2))) do begin
tx:=j+1; ty:=k; if ty<0 then ty:=0;
if (tx<=SizeX)and(ty<SizeY) then begin ot[i]:=tx+ty*SizeX; inc(i);
if i>=256 then MessageBox(0,'Too large poly in LWO, 2+ km2','Error',MB_OK or MB_ICONERROR); end;
end;
end;

if trunc(Ay)=trunc(Cy) then begin //all in one line
v1:=trunc(Ax); v2:=trunc(Bx); w1:=trunc(Cx); w2:=w1;
end else begin

    if trunc(Ay)=trunc(By) then begin                //A-B line
    v1:=trunc(Ax); v2:=trunc(Bx); end else begin
    v1:=Bx+(Min(trunc(By+1),Ay)-By)*(Ax-Bx)/(Ay-By); v2:=trunc(Bx); end;

    if trunc(By)=trunc(Cy) then begin                //B-C line
    w1:=trunc(Bx); w2:=trunc(Cx); end else begin
    w1:=Cx+(Max(trunc(By),Cy)-Cy)*(Bx-Cx)/(By-Cy); w2:=trunc(Bx); end;

    r1:=Min(Min(v1,w1),Min(v2,w2));
    r2:=Max(Max(v1,w1),Max(v2,w2));
    v1:=r1; v2:=r2;

    w1:=Cx+(Min(trunc(By+1),Ay)-Cy)*(Ax-Cx)/(Ay-Cy); //A-C line
    w2:=Cx+(Max(trunc(By  ),Cy)-Cy)*(Ax-Cx)/(Ay-Cy);
end;
for j:=trunc(Min(Min(v1,w1),Min(v2,w2))) to trunc(Max(Max(v1,w1),Max(v2,w2))) do begin
tx:=j+1; ty:=trunc(By);
if (tx<=SizeX)and(ty<SizeY) then begin ot[i]:=tx+ty*SizeX; inc(i);
if i>=256 then MessageBox(0,'Too large poly in LWO, 2+ km2','Error',MB_OK or MB_ICONERROR); end;
end;

if (Ay<>Cy)and(Ay<>By) then              //Above mid-line
for k:=trunc(By)+1 to trunc(Ay) do begin                      //ceil(By)

v1:=Cx+(Min(k+1,Ay)-Cy)*(Ax-Cx)/(Ay-Cy);
w1:=Bx+(Min(k+1,Ay)-By)*(Ax-Bx)/(Ay-By);

v2:=Cx+(Max(k,Cy)-Cy)*(Ax-Cx)/(Ay-Cy);
w2:=Bx+(Max(k,By)-By)*(Ax-Bx)/(Ay-By);

{v1:=Cx+(k-Cy)*(Ax-Cx)/(Ay-Cy);
w1:=Bx+(k-By)*(Ax-Bx)/(Ay-By);
if k<>trunc(Ay) then v2:=Cx+(k+1-Cy)*(Ax-Cx)/(Ay-Cy) else v2:=v1;
if k<>trunc(Ay) then w2:=Bx+(k+1-By)*(Ax-Bx)/(Ay-By) else w2:=w1;}
for j:=trunc(Min(Min(v1,w1),Min(v2,w2))) to trunc(Max(Max(v1,w1),Max(v2,w2))) do begin
tx:=j+1; ty:=k;
if (tx<=SizeX)and(ty<SizeY) then begin ot[i]:=tx+ty*SizeX; inc(i);
if i>=256 then MessageBox(0,'Too large poly in LWO, 2+ km2','Error',MB_OK or MB_ICONERROR); end;
end;
end;

end;


function real2(c1,c2,c3,c4:char):real;
var //i,sign,exponent:integer; t,mantissa:real;
Tmp:array[1..4] of char;
begin
Tmp[1]:=c1; Tmp[2]:=c2; Tmp[3]:=c3; Tmp[4]:=c4;
Result:=Single(Tmp);
{if ord(c4) div 128 = 1 then Sign:=-1 else Sign:=1;
Exponent:=(ord(c4) mod 128)*2 + (ord(c3) div 128)-127;
Mantissa:=(ord(c3) mod 128)*65536+ord(c2)*256+ord(c1);
t:=(1+Mantissa/8388608)*Sign;
if Exponent=-127 then t:=0 else
if exponent>0 then for i:=1 to exponent do t:=t*2 else
if exponent<0 then for i:=-1 downto exponent do t:=t/2;
real2:=t;}
end;


function unreal2(x:real):string;
var sign,mant,expo:integer;
begin
if x<0 then sign:=1 else sign:=0;
if x<>0 then begin
x:=abs(x); expo:=0;
if x>=2 then repeat                  //normalizes X
x:=x/2; expo:=expo+1;           //so it look like
until(x<2)              // 1.28758...
else if x<1 then repeat                  //
x:=x*2; expo:=expo-1;   //power of 2
until(x>=1)             //to use
else expo:=0;           //
mant:=round((x-1)*8388608); expo:=expo+127;
unreal2:=chr(mant)+chr(mant div 256)+chr((mant div 65536)+(expo mod 2)*128)+chr(expo div 2 + sign*128);
end else unreal2:=#0+#0+#0+#0;
end;

function int2fix(Number,Len:integer):string;
var ss:string; x:byte;
begin
ss:=inttostr(Number);
for x:=length(ss) to Len-1 do
ss:='0'+ss;
if length(ss)>Len then ss:='**********';//ss[99999999]:='0'; //generating an error in lame way
setlength(ss,Len);
int2fix:=ss;
end;

function float2fix(Number:single; Digits:integer):string;
var ss:string;
begin
ss:=FloatToStrF(Number,ffGeneral,3,2);
float2fix:=ss;
end;

function int2(c1,c2:char):integer; overload;
begin int2:=ord(c1)+ord(c2)*256; end;

function int2(c1,c2,c3,c4:char):integer; overload;
var x:integer;
begin if ord(c4)>127 then begin
x:=-(255-ord(c1)+(255-ord(c2))*256+(255-ord(c3))*65536+(255-ord(c4))*16777216);
int2:=x-1;
end else
int2:=ord(c1)+ord(c2)*256+ord(c3)*65536+ord(c4)*16777216; end;

function chr2(x,len:integer):string; overload;
var tmp:integer;
begin tmp:=x;
if x<0 then begin inc(tmp);
chr2:=chr(tmp-1)+char(tmp div 256-1)+char(tmp div 65536-1)+char(tmp div 16777216-1)
end else
chr2:=chr(tmp)+char(tmp div 256)+char(tmp div 65536)+char(tmp div 16777216);
end;

function chr2(t:string; len:integer):string; overload;
var i:integer; begin
for i:=length(t) to len-1 do t:=t+#0;
setlength(t,len);
chr2:=t;
end;

procedure Color2RGB(Col:integer; out R,G,B:byte);
begin
R:=Col AND $FF;
G:=Col AND $FF00 SHR 8;
B:=Col AND $FF0000 SHR 16;
end;

function Ceil(const X: Extended): Integer;
begin
  Result := Integer(Trunc(X));
  if Frac(X) > 0 then
    Inc(Result);
end;

function ArcCos(const X: Extended): Extended;
begin
  Result := ArcTan2(Sqrt(1 - X * X), X);
end;

function ArcSin(const X: Extended): Extended;
begin
  Result := ArcTan2(X, Sqrt(1 - X * X))
end;

function ArcTan2(const Y, X: Extended): Extended;
asm
        FLD     Y
        FLD     X
        FPATAN
        FWAIT
end;

function Pow(const Base, Exponent: integer): integer;
begin
  if Exponent = 0 then
    Result := 1               { n**0 = 1 }
  else
  if (Base = 0) and (Exponent > 0) then
    Result := 0               { 0**n = 0, n > 0 }
  else
    Result := round(IntPower(Base, Exponent))
end;

procedure Angles2Matrix(ax,ay,az:single; matrix:pointer; qty:integer);
var a,b,c,d,e,f:single;  A1:^single; N:integer;
begin
N:=0;
ax:=ax/180*pi;
ay:=ay/180*pi;
az:=az/180*pi;
a:=cos(ax);
b:=sin(ax);
c:=cos(ay);
d:=sin(ay);
e:=cos(az);//1
f:=sin(az);//0
A1:=pointer(integer(matrix)+N*4); A1^:=C*E;         inc(N);
A1:=pointer(integer(matrix)+N*4); A1^:=-C*F;        inc(N);
A1:=pointer(integer(matrix)+N*4); A1^:=D;           inc(N);
if qty=16 then inc(N);
A1:=pointer(integer(matrix)+N*4); A1^:=B*D*E+A*F;   inc(N);
A1:=pointer(integer(matrix)+N*4); A1^:=-B*D*F+A*E;  inc(N);
A1:=pointer(integer(matrix)+N*4); A1^:=-B*C;        inc(N);
if qty=16 then inc(N);
A1:=pointer(integer(matrix)+N*4); A1^:=-A*D*E+B*F;  inc(N);
A1:=pointer(integer(matrix)+N*4); A1^:=A*D*F+B*E;   inc(N);
A1:=pointer(integer(matrix)+N*4); A1^:=A*C;
{        |  CE      -CF       D  |              //  C   0   D      D -BC AC
    M  = |  BDE+AF  -BDF+AE  -BC |              //  BD  A  -BC     0  A  B
         | -ADE+BF   ADF+BE   AC |}             // -AD  B   AC     C BD -AD
end;

procedure Matrix2Angles(matrix09:array of single; Qty:integer; i1,i2,i3:pinteger);
var Ax,Ay,Az:single; Num:byte; a1,a2,a3:integer; m:array[1..9] of single;
begin
if Qty=16 then Num:=1 else Num:=0;
m[1]:=matrix09[0]; m[2]:=matrix09[1]; m[3]:=matrix09[2];
m[4]:=matrix09[3+Num]; m[5]:=matrix09[4+Num]; m[6]:=matrix09[5+Num];
m[7]:=matrix09[6+Num*2]; m[8]:=matrix09[7+Num*2]; m[9]:=matrix09[8+Num*2];

        Ax:=arctan2(-m[6],m[9]);               // -BC : AC
        Az:=arctan2(-m[2],m[1]);               // -CF : CE
        if round(m[9]*1000)<>0 then
          Ay:=arctan2(m[3],m[9]/cos(Ax))       //   D : AC/A
        else 
  	    if round(m[2]*1000)<>0 then
            Ay:=arctan2(m[3],-m[6]/sin(Ax))    //   D :-BC/B
          else 
            if round(m[1]*1000)<>0 then
              Ay:=arctan2(m[3],m[1]/cos(Az))   //   D : CE/E
            else //if round(m[2]*1000)<>0 then
              Ay:=arctan2(m[3],-m[2]/cos(Az)); //   D :-CF/F

{        |  CE      -CF       D  |    1-2-3
    M  = |  BDE+AF  -BDF+AE  -BC |    4-5-6
         | -ADE+BF   ADF+BE   AC |    7-8-9
         }
a1:=round(Ax*180/pi); if a1>180 then dec(a1,360); if a1<-180 then inc(a1,360);
a2:=round(Ay*180/pi); if a2>180 then dec(a2,360); if a2<-180 then inc(a2,360);
a3:=round(Az*180/pi); if a3>180 then dec(a3,360); if a3<-180 then inc(a3,360);
i1^:=a1;
i2^:=a2;
i3^:=a3;
end;

procedure Angles2Vector(degreeX,degreeY,iz:single; out nx,ny,nz:single);
begin
  nx:=sin(degreeX/180*pi) * cos(degreeY/180*pi);
  ny:=sin(degreeY/180*pi);
  nz:=cos(degreeX/180*pi) * cos(degreeY/180*pi);
end;

procedure ConvertSetToArray(iSet:integer; Ar:pointer);
var i,k:integer; A:^integer;
begin
k:=1;
for i:=1 to 24 do
  if iSet and pow(2,i) = pow(2,i) then
    begin
      A:=pointer(integer(Ar)+k*4);
      A^:=i;
      inc(k);
    end;
A:=pointer(integer(Ar));
A^:=k-1;
end;

function MakePOT(num:integer):integer;
var t:single; i:integer;
begin
t:=num; i:=1;
while t>2 do begin t:=t / 2; inc(i); end;
Result:=pow(2,i);
end;

function GetLengthSQR(ix,iy,iz:integer): integer;
begin
  Result:=sqr(ix)+sqr(iy)+sqr(iz);
end;

function GetLength(ix,iy,iz:single): single; overload
begin
  Result:=sqrt(sqr(ix)+sqr(iy)+sqr(iz));
end;

function GetLength(ix:Vector3f): single; overload
begin
  Result:=sqrt(sqr(ix.x)+sqr(ix.y)+sqr(ix.z));
end;

function GetLength(ix,iy:single): single; overload
begin
  Result:=sqrt(sqr(ix)+sqr(iy));
end;


function InBetween(A,B,X:single): boolean;
begin
  if A>B then
    Result:=(A>X)and(X>B)
  else
  if A<B then
    Result:=(A<X)and(X<B)
  else
    Result:=false
end;


procedure Normalize(ix,iy,iz:single; nx,ny,nz:psingle);
var len:single;
begin
  len:=sqrt(sqr(ix)+sqr(iy)+sqr(iz));
  if len=0 then len:=1;
  nx^:=ix/len;
  ny^:=iy/len;
  nz^:=iz/len;
end;

procedure Normalize(var ix,iy,iz:single);
var len:single;
begin
len:=sqrt(sqr(ix)+sqr(iy)+sqr(iz));
if len=0 then len:=1;
ix:=ix/len;
iy:=iy/len;
iz:=iz/len;
end;

procedure Normalize(var v:Vector3f);
var len:single;
begin
len:=sqrt(sqr(v.x)+sqr(v.y)+sqr(v.z));
if len=0 then len:=1;
v.x:=v.x/len;
v.y:=v.y/len;
v.z:=v.z/len;
end;

procedure Normalize(var ix,iy:single);
var len:single;
begin
len:=sqrt(sqr(ix)+sqr(iy));
if len=0 then len:=1;
ix:=ix/len;
iy:=iy/len;
end;

function Mix(x1,x2,MixValue:single):single; overload
begin
Result:=x1*MixValue+x2*(1-MixValue);
end;

function Mix(x1,x2:integer; MixValue:single):integer; overload
begin
Result:=round(x1*MixValue+x2*(1-MixValue));
end;

function Mix(x1,x2:Vector3f; MixValue:single):vector3f; overload
begin
Result.X:=x1.X*MixValue+x2.X*(1-MixValue);
Result.Y:=x1.Y*MixValue+x2.Y*(1-MixValue);
Result.Z:=x1.Z*MixValue+x2.Z*(1-MixValue);
end;

function DotProduct(x1,y1,z1,x2,y2,z2:single):single; overload
begin
Result:=x1*x2+y1*y2+z1*z2;
end;

function DotProduct(v1,v2:Vector3f):single; overload
begin
Result:=v1.X*v2.X+v1.Y*v2.Y+v1.Z*v2.Z;
end;

procedure Normal2Poly(v1,v2,v3:array of single; nx,ny,nz:psingle); overload
begin  //aka Cross product of 2 vectors
nx^:= ((v1[1]-v2[1])*(v1[2]-v3[2])-(v1[2]-v2[2])*(v1[1]-v3[1]))/256;
ny^:=-((v1[0]-v2[0])*(v1[2]-v3[2])-(v1[2]-v2[2])*(v1[0]-v3[0]))/256;
nz^:= ((v1[0]-v2[0])*(v1[1]-v3[1])-(v1[1]-v2[1])*(v1[0]-v3[0]))/256;
end;

procedure Normal2Poly(v1,v2,v3:Vector3f; n:PVector3f); overload
begin  //aka Cross product of 2 vectors
n.x:= ((v1.Y-v2.Y)*(v1.Z-v3.Z)-(v1.Z-v2.Z)*(v1.Y-v3.Y))/256;
n.y:=-((v1.X-v2.X)*(v1.Z-v3.Z)-(v1.Z-v2.Z)*(v1.X-v3.X))/256;
n.z:= ((v1.X-v2.X)*(v1.Y-v3.Y)-(v1.Y-v2.Y)*(v1.X-v3.X))/256;
end;

procedure Normal2Poly(u1,v1,u2,v2,u3,v3:single; out n:single); overload
begin  //aka Cross product of 2 vectors
n:=(u1-u2)*(v1-v3)-(v1-v2)*(u1-u3);
end;

procedure decs(var AText:string; const Len:integer=1);
begin
if length(AText)<=abs(Len) then Atext:=''
else
if Len>=0 then AText:=Copy(AText, 1, length(AText)-Len)
          else AText:=Copy(AText, 1+abs(Len), length(AText)-abs(Len));
end;

procedure decs(var AText:widestring; const Len:integer=1);
begin
if length(AText)<=abs(Len) then Atext:=''
else
if Len>=0 then AText:=Copy(AText, 1, length(AText)-Len)
          else AText:=Copy(AText, 1+abs(Len), length(AText)-abs(Len));
end;

function decs(AText:string; Len,RunAsFunction:integer):string; overload;
begin
if length(AText)<=abs(Len) then result:=''
else
if Len>=0 then result:=Copy(AText, 1, length(AText)-Len)
          else result:=Copy(AText, 1+abs(Len), length(AText)-abs(Len));
end;

function GetNumberFromString(AText:string; Position:integer):single;
var i,Pos1,Pos2:integer; s:string;
begin
  AText:=' '+AText;

  Pos1:=0;
  for i:=1 to Position do
  repeat
  inc(Pos1);
  until((Pos1+1>length(AText))or(
  ((AText[Pos1]=' ')or(AText[Pos1]=#9)or(AText[Pos1]='/'))and(AText[Pos1+1] in [','..'.','0'..'9']) //,-.0..9
  ));

  Pos2:=Pos1+1;
  repeat
    if (Pos2<=length(AText))and( AText[Pos2] in [',','.'] ) then AText[Pos2]:=DecimalSeparator;
    inc(Pos2);
  until((Pos2>length(AText))or(
  ((AText[Pos2]=' ')or(AText[Pos2]=#9)or(AText[Pos2]='/'))and(AText[Pos2-1] in [','..'.','0'..'9'])
  ));

  s:=Copy(AText,Pos1+1,Pos2-1 - Pos1);

  if s='' then Result:=0 else
  Result:=strtofloat(s);

end;

procedure SwapStr(var A,B:string);
var s:string;
begin
s:=A; A:=B; B:=s;
end;

procedure SwapInt(var A,B:word);
var s:integer;
begin
s:=A; A:=B; B:=s;
end;

procedure SwapFloat(var A,B:single);
var s:single;
begin
s:=A; A:=B; B:=s;
end;

function Adler32CRC(TextPointer:Pointer; TextLength:integer):integer;
var i,A,B:integer;
begin
A:=1; B:=0; //A is initialized to 1, B to 0
for i:=1 to TextLength do begin
inc(A,pbyte(integer(TextPointer)+i-1)^);
inc(B,A);
end;
A:=A mod 65521; //65521 (the largest prime number smaller than 2^16)
B:=B mod 65521;
Adler32CRC:=B+A*65536; //reverse order for smaller numbers
end;

function RandomS(Range_Both_Directions:integer):integer; overload
begin
Result:=Random(Range_Both_Directions*2+1)-Range_Both_Directions;
end;

function RandomS(Range_Both_Directions:single):single; overload
begin
Result:=Random(round(Range_Both_Directions*20000)+1)/10000-Range_Both_Directions;
end;

procedure WriteLangFile(Sender:TForm; FileName:string; EraseWritten:boolean);
var ft:textfile; i,k:integer; capt:string;
begin
assignfile(ft,FileName); rewrite(ft);

for i:=0 to Sender.ComponentCount-1 do begin

  if(IsPublishedProp(Sender.Components[i],'Caption')) then begin
  capt:=GetStrProp(Sender.Components[i],'Caption');
    if capt<>'' then begin
    writeln(ft,capt+'<=>'+uppercase(capt));
    if EraseWritten then SetStrProp(Sender.Components[i],'Caption','*');
    end;
  end;

  if Sender.Components[i] is TRadioGroup then begin
  writeln(ft,'//List');
    for k:=0 to TRadioGroup(Sender.Components[i]).Items.Count-1 do begin
    capt:=TRadioGroup(Sender.Components[i]).Items[k];
    writeln(ft,capt+'<=>'+uppercase(capt));
    if EraseWritten then TRadioGroup(Sender.Components[i]).Items[k]:='*';
    end;
  writeln(ft,'//EndList');
  end;
  
end;

closefile(ft);
end;

procedure ReadLangFile(Sender:TForm; FileName:string; EraseWritten:boolean);
var ft:textfile; i,k,row:integer; capt,eng,rus:string; IsList:boolean;
begin     
if not fileexists(FileName) then begin
MessageBox(Sender.Handle,@('Can''t find input file '+FileName)[1],'Error',MB_OK);
exit;
end;

assignfile(ft,FileName); reset(ft);
IsList:=false;
row:=0;

repeat
inc(row);
readln(ft,capt);
if capt='//List' then begin
IsList:=true;
readln(ft,capt);
end;
if capt='//EndList' then begin
IsList:=false;
readln(ft,capt);
end;

k:=1;
repeat inc(k) until((k+1>length(capt))or(capt[k-1]+capt[k]+capt[k+1]='<=>'));

if k+1>length(capt) then begin
MessageBox(Sender.Handle,@('Error on line '+inttostr(row)+'. ')[1],'Error',MB_OK);
//exit;
end;

eng:=decs(capt,length(capt)-k+2,0); //+2 means '<=' thing
rus:=decs(capt,-k-1,0);             //-1 means '>' thing

for i:=0 to Sender.ComponentCount-1 do begin

  if(IsPublishedProp(Sender.Components[i],'Caption')) then begin
  capt:=GetStrProp(Sender.Components[i],'Caption');
  if capt=eng then SetStrProp(Sender.Components[i],'Caption',rus);
  end;

  if IsList then
  if Sender.Components[i] is TRadioGroup then begin
    for k:=0 to TRadioGroup(Sender.Components[i]).Items.Count-1 do begin
    capt:=TRadioGroup(Sender.Components[i]).Items[k];
    if capt=eng then TRadioGroup(Sender.Components[i]).Items[k]:=rus;
    end;
  end;
  
end;

until(eof(ft));
closefile(ft);
end;

function RunOpenDialog(Sender:TOpenDialog; Name,Path,Filter:string):boolean;
begin
Sender.FileName:=Name;
Sender.InitialDir:=Path;
Sender.Filter:=Filter;
Result:=Sender.Execute; //Returns "false" if user pressed "Cancel"
//Result:=Result and FileExists(Sender.FileName); //Already should be enabled in OpenDialog options
end;

function RunSaveDialog(Sender:TSaveDialog; FileName, FilePath, Filter:string; const FileExt:string = ''):boolean;
begin
Sender.FileName:=FileName;
Sender.InitialDir:=FilePath;
Sender.Filter:=Filter;
Result:=Sender.Execute; //Returns "false" if user pressed "Cancel"
Sender.FileName:=AssureFileExt(Sender.FileName,FileExt);
end;

procedure Triangulate(VerticeCount:integer; Vertice:array of vector3f; out PolyCount:integer; out Polys:array of word; out Result:boolean);
var
  h,ii,kk,ci:integer;
  n0,n1,n2:integer;
  SubDrop:array of byte;
  Tmp:vector3f;
  PierceTest:boolean;
begin

  setlength(SubDrop,VerticeCount+1);
  SubDrop[0]:=1;

  n0:=0; ci:=1; kk:=VerticeCount; ii:=1;
  repeat

    h:=0;
    repeat
      inc(n0); inc(h);
      if n0>(VerticeCount) then n0:=1;
    until((SubDrop[n0]=0)or(h=kk));
    if h=kk then break;

    h:=0; n1:=n0;
    repeat
      inc(n1); inc(h);
      if n1>(VerticeCount) then n1:=1;
    until(((n0<>n1)and(SubDrop[n1]=0))or(h=kk));
    if h=kk then break;

    h:=0; n2:=n1;
    repeat
      inc(n2); inc(h);
      if n2>(VerticeCount) then n2:=1;
    until(((n0<>n2)and(n1<>n2)and(SubDrop[n2]=0))or(h=kk));
    if h=kk then break;

    //Check direction poly is facing
    Normal2Poly(Vertice[n0],Vertice[n1],Vertice[n2],@Tmp);
    if Tmp.Y<0 then begin

      PierceTest:=false;
      h:=n2; //Take n0 and n2 as basis and test all other vertices to be on one side
      
      repeat
        inc(h); //starting from n2+1
        if h>(VerticeCount) then h:=1;
        if SubDrop[h]=0 then
          if h<>n0 then //ending at n0-1
          if InBetween(Vertice[n0].X,Vertice[n2].X,Vertice[h].X) then
          if InBetween(Vertice[n0].Z,Vertice[n2].Z,Vertice[h].Z) then
            Normal2Poly(Vertice[n0],Vertice[n2],Vertice[h],@Tmp);
            if Tmp.Y>0 then PierceTest:=true;
      until((PierceTest)or(h=n0));

      if not PierceTest then begin
        Polys[ci]:=n0;
        inc(ci);
        Polys[ci]:=n1;
        inc(ci);
        Polys[ci]:=n2;
        inc(ci);
        SubDrop[n1]:=1;
        inc(n0);
      end;
    end;

    inc(ii);

  until(ii=500); //How long to keep looking for more polys

  PolyCount:=(ci-1) div 3;

  Result:= PolyCount <> VerticeCount-2;
end;

end.
