unit umain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, Buttons, KromUtils, Math;

{Fonts}
type //Indexing should start from 1.
  TKMFont = (fnt_Adam=1, fnt_Antiqua, fnt_Briefing, fnt_Font01, fnt_Game,
             fnt_Grey, fnt_KMLobby0, fnt_KMLobby1, fnt_KMLobby2, fnt_KMLobby3,
             fnt_KMLobby4, fnt_MainA, fnt_MainB, fnt_MainMapGold, fnt_Metal,
             fnt_Mini, fnt_Minimum, fnt_Outline, fnt_System, fnt_Won);
const //Font01.fnt seems to be damaged..
  FontFiles: array[1..20]of string = (
  'adam','antiqua','briefing','font01-damaged','game','grey','kmlobby0','kmlobby1','kmlobby2','kmlobby3',
  'kmlobby4','maina','mainb','mainmapgold','metal','mini','mininum','outline','system','won');
  
//using 0 as default, with exceptions. Only used fonts have been checked, so this will need to be updated as we add new ones.
  FontCharSpacing: array[TKMFont] of shortint = (0,0,0,0,1,-1,0,0,0,0,0,0,0,0,1,1,1,-1,0,0);

  FontPal:array[1..20]of byte =
  //Those 10 are unknown Pal, no existing Pal matches them well
  (10,2,1,10,2,2,1,8,8,9,
   9,8,10,8,2,8,8,2,10,9);

{Palettes}
const
 //Palette filename corresponds with pal_**** constant, except pal_lin which is generated proceduraly (filename doesn't matter for it)
 PalFiles:array[1..13]of string = (
 'map.bbm', 'pal0.bbm', 'pal1.bbm', 'pal2.bbm', 'pal3.bbm', 'pal4.bbm', 'pal5.bbm', 'setup.bbm', 'setup2.bbm', 'map.bbm',
 'mapgold.lbm', 'setup.lbm', 'pal1.lbm');
 pal_map=1; pal_0=2; pal_1=3; pal_2=4; pal_3=5; pal_4=6; pal_5=7; pal_set=8; pal_set2=9; pal_lin=10;
 pal2_mapgold=11; pal2_setup=12; pal2_1=13;


type
  TfrmMain = class(TForm)
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    btnLoadFont: TBitBtn;
    BitBtn1: TBitBtn;
    btnExport: TBitBtn;
    btnImport: TBitBtn;
    Image1: TImage;
    procedure btnLoadFontClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    fFileEditing: string;
    procedure SetFileEditing(aFile:string);
    function LoadFont(filename:string; aFont:TKMFont; WriteFontToBMP:boolean):boolean;
    function LoadPalette(filename:string; PalID:byte):boolean;
  public
    { Public declarations }              
    property FileEditing: string read fFileEditing write SetFileEditing;
  end;

var
  frmMain: TfrmMain;
  ExeDir: string;

  FontData: record
    Title:TKMFont;
    //TexID:GLUint;
    Pal:array[0..255]of byte;
    Letters:array[0..255]of record
      Width,Height:word;
      Add:array[1..4]of word;
      Data:array[1..4096] of byte;
      u1,v1,u2,v2:single;
    end;
    end;
    
  Pal:array[1..13,1..256,1..3]of byte;

implementation

{$R *.dfm}

procedure TfrmMain.SetFileEditing(aFile:string);
begin
  if not FileExists(aFile) then exit;
  fFileEditing := aFile;
  Caption := 'KaM Font Editor - '+aFile;
end;

procedure TfrmMain.btnLoadFontClick(Sender: TObject);
var FileName: string;
begin
  if OpenDialog1.Execute then
  begin
    FileName := OpenDialog1.FileName;
    if not FileExists(FileName) then exit;
    LoadFont(FileName,fnt_Outline,true)
  end;
end;


function TfrmMain.LoadFont(filename:string; aFont:TKMFont; WriteFontToBMP:boolean):boolean;
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
  MyRect: TRect;
begin
  Result:=false;
  MaxHeight:=0;
  if not CheckFileExists(filename, true) then exit;

  assignfile(f,filename); reset(f,1);
  blockread(f,a,2); blockread(f,b,2);
  blockread(f,c,2); blockread(f,d,2);
  blockread(f,FontData.Pal[0],256);

  //Read font data
  for i:=0 to 255 do
    if FontData.Pal[i]<>0 then
      with FontData.Letters[i] do begin
        blockread(f,Width,4);
        blockread(f,Add,8);
        MaxHeight:=max(MaxHeight,Height);
        //fLog.AssertToLog(Width*Height<>0,'Font data Width*Height <> 0'); //Fon01.fnt seems to be damaged..
        blockread(f,Data[1],Width*Height);
      end;
  closefile(f);

  //Special fixes:
  {if aFont=fnt_game then
  for i:=0 to 255 do
    if FontData.Pal[i]<>0 then
      for k:=1 to 4096 do
        if FontData.Letters[i].Data[k]<>0 then
          FontData.Letters[i].Data[k]:=218; //Light grey color in Pal2}


  //Compile texture
  AdvX:=0; AdvY:=0;
  setlength(TD,TexWidth*TexWidth+1);
  FillChar(TD[0],TexWidth*TexWidth+1,$80); //Make some background

  for i:=0 to 255 do
    if FontData.Pal[i]<>0 then
      with FontData.Letters[i] do begin

      //fLog.AssertToLog(FontData.Pal[i]=1,'FontData palette <> 1');

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

    //FontData.TexID := GenTexture(TexWidth,TexWidth,@TD[0],tm_NoCol,FontPal);

    FontData.Letters[32].Width:=7; //"Space" width

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

    MyBitmap.SaveToFile(ExeDir+ExtractFileName(filename)+inttostr(p)+'.bmp');
    MyRect.Left := 0;
    MyRect.Top := 0;
    MyRect.Right := MyBitmap.Width;
    MyRect.Bottom := MyBitmap.Height;

    Image1.Picture.Bitmap.LoadFromFile(ExeDir+ExtractFileName(filename)+inttostr(p)+'.bmp');

    //MyBitmap.Canvas.CopyRect(MyRect,Image1.Canvas,MyRect);
    MyBitmap.Free;
  end;

  setlength(TD,0);
  Result:=true;

end;

function TfrmMain.LoadPalette(filename:string; PalID:byte):boolean;
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

procedure TfrmMain.FormCreate(Sender: TObject);
var i :integer;
begin
  ExeDir:=IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName));
  for i:=1 to length(PalFiles) do
   LoadPalette(ExeDir+'data\gfx\'+PalFiles[i],i);
end;

end.
