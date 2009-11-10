unit umain;
interface
uses
  Windows, Messages, SysUtils, FileCtrl, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, KromUtils, Math, ComCtrls, Buttons;

{Fonts}
type //Indexing should start from 1.
  TKMFont =
  (fnt_Adam=1,   fnt_Antiqua,  fnt_Briefing, fnt_Font01,      fnt_Game,
   fnt_Grey,     fnt_KMLobby0, fnt_KMLobby1, fnt_KMLobby2,    fnt_KMLobby3,
   fnt_KMLobby4, fnt_MainA,    fnt_MainB,    fnt_MainMapGold, fnt_Metal,
   fnt_Mini,     fnt_Minimum,  fnt_Outline,  fnt_System,      fnt_Won);
const //using 0 as default, with exceptions. Only used fonts have been checked, so this will need to be updated as we add new ones.
  FontCharSpacing: array[TKMFont] of shortint =
  ( 0, 0, 0, 0, 1,-1, 0, 0, 0, 0,
    0, 0, 0, 0, 1, 1, 1,-1, 0, 0);

  FontPal:array[1..20]of byte = //Those 10 are unknown Pal, no existing Pal matches them well
  (10, 2, 1,10, 2, 2, 1, 8, 8, 9,
    9, 8,10, 8, 2, 8, 8, 2,10, 9);

{Palettes}
const
 //Palette filename corresponds with pal_**** constant, except pal_lin which is generated proceduraly (filename doesn't matter for it)
 PalFiles:array[1..13]of string = (
 'map.bbm', 'pal0.bbm', 'pal1.bbm', 'pal2.bbm', 'pal3.bbm', 'pal4.bbm', 'pal5.bbm', 'setup.bbm', 'setup2.bbm', 'map.bbm',
 'mapgold.lbm', 'setup.lbm', 'pal1.lbm');
 pal_map=1; pal_0=2; pal_1=3; pal_2=4; pal_3=5; pal_4=6; pal_5=7; pal_set=8; pal_set2=9; pal_lin=10;
 pal2_mapgold=11; pal2_setup=12; pal2_1=13;

{Globals}
 var
  ExeDir: string;
  DataDir: string;

  FontData: record
    Title:TKMFont;
    Pal:array[0..255]of byte;
    Letters:array[0..255]of record
      Width,Height:word;
      Add:array[1..4]of word;
      Data:array[1..4096] of byte;
    end;
  end;

  PalData:array[1..13,1..256,1..3]of byte;
  ActiveLetter:integer;

type
  TfrmMain = class(TForm)
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    BitBtn1: TBitBtn;
    btnExport: TBitBtn;
    btnImport: TBitBtn;
    ListBox1: TListBox;
    RefreshData: TButton;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    Image1: TImage;
    Image2: TImage;
    Shape1: TShape;
    Image3: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Shape2: TShape;
    Edit1: TEdit;
    Image4: TImage;
    Image5: TImage;
    Shape3: TShape;
    StatusBar1: TStatusBar;
    procedure btnLoadFontClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure RefreshDataClick(Sender: TObject);
    procedure ListBox1Click(Sender: TObject);
    procedure btnExportClick(Sender: TObject);
    procedure Image1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure Image1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    { Private declarations }
    fFileEditing: string;
    procedure SetFileEditing(aFile:string);
    procedure ScanDataForPalettesAndFonts(apath:string);
    function LoadFont(filename:string; aFont:TKMFont; WriteFontToBMP:boolean):boolean;
    function LoadPalette(filename:string; PalID:byte):boolean;
  public
    { Public declarations }
    property FileEditing: string read fFileEditing write SetFileEditing;
    procedure ShowLetter(aLetter:integer);
  end;

 var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  ExeDir := IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName));
  DataDir := ExeDir;
  if DirectoryExists('C:\Documents and Settings\Krom\Desktop\Delphi\KaM Remake\') then
    DataDir := 'C:\Documents and Settings\Krom\Desktop\Delphi\KaM Remake\';
  if DirectoryExists('C:\Documents and Settings\А35\Рабочий стол\castlesand\') then
    DataDir := 'C:\Documents and Settings\А35\Рабочий стол\castlesand\';
  if DirectoryExists('C:\Documents and Settings\lewin\My Documents\Projects\Castlesand\') then
    DataDir := 'C:\Documents and Settings\lewin\My Documents\Projects\Castlesand\';
  ScanDataForPalettesAndFonts(DataDir);
end;


procedure TfrmMain.RefreshDataClick(Sender: TObject);
begin
  ScanDataForPalettesAndFonts(DataDir);
end;


procedure TfrmMain.ScanDataForPalettesAndFonts(apath:string);
var i:integer; SearchRec:TSearchRec;
begin
  //0. Clear old list
  ListBox1.Items.Clear;

  //1. Palettes
  for i:=1 to length(PalFiles) do
   LoadPalette(apath+'data\gfx\'+PalFiles[i],i);

  //2. Fonts
  if not DirectoryExists(apath+'data\gfx\fonts\') then exit;

  ChDir(apath+'data\gfx\fonts');
  FindFirst('*', faAnyFile, SearchRec);
  repeat
  if (SearchRec.Attr and faDirectory = 0)and(SearchRec.Name<>'.')and(SearchRec.Name<>'..') then
  begin
    ListBox1.Items.Add(SearchRec.Name);
  end;
  until (FindNext(SearchRec)<>0);
  FindClose(SearchRec);
end;

procedure TfrmMain.SetFileEditing(aFile:string);
begin
  if not FileExists(aFile) then exit;
  fFileEditing := aFile;
  Caption := 'KaM Font Editor - '+aFile;
end;

procedure TfrmMain.btnLoadFontClick(Sender: TObject);
begin
  if not RunOpenDialog(OpenDialog1,'KaM fonts','','*.fnt') then exit;
  if not FileExists(OpenDialog1.FileName) then exit;
  LoadFont(OpenDialog1.FileName,fnt_Outline,true)
end;


function TfrmMain.LoadFont(filename:string; aFont:TKMFont; WriteFontToBMP:boolean):boolean;
const
  TexWidth=512; //Connected to TexData, don't change
var
  f:file;
  p,t:byte;
  a,b,c,d:word;
  i,k,ci,ck:integer;
  MaxHeight, MaxWidth:integer;
  CellX,CellY:integer;
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
        blockread(f, Width, 4);
        blockread(f, Add, 8);
        MaxHeight := max(MaxHeight,Height);
        MaxWidth := max(MaxWidth,Height);
        blockread(f, Data[1], Width*Height);
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
  setlength(TD, TexWidth*TexWidth + 1);
  FillChar(TD[0], TexWidth*TexWidth + 1, $00); //Make some background

  for i:=0 to 255 do
    if FontData.Pal[i]<>0 then
      with FontData.Letters[i] do begin

        CellX := ((i mod 16)*32);
        CellY := ((i div 16)*32);

        for ci:=0 to Height-1 do for ck:=0 to Width-1 do
          TD[(CellY + ci) * TexWidth + CellX + 1 + ck] := Data[ci * Width + ck + 1];

      end;

  FontData.Letters[32].Width:=7; //"Space" width

  MyBitMap := TBitMap.Create;
  MyBitmap.PixelFormat := pf24bit;
  MyBitmap.Width := TexWidth;
  MyBitmap.Height := TexWidth;

  for ci:=0 to TexWidth-1 do for ck:=0 to TexWidth-1 do begin
    p:=FontPal[byte(aFont)];
    //p:=i;
    t:=TD[ci*TexWidth+ck]+1;
    MyBitmap.Canvas.Pixels[ck,ci]:=PalData[p,t,1]+PalData[p,t,2]*256+PalData[p,t,3]*65536;
  end;

  if WriteFontToBMP then
    MyBitmap.SaveToFile(ExeDir+ExtractFileName(filename)+inttostr(p)+'.bmp');

  Image1.Canvas.Draw(0, 0, MyBitmap); //Draw MyBitmap into Image1
  MyBitmap.Free;

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
  blockread(f,PalData[PalID],48); //Unknown and/or unimportant
  blockread(f,PalData[PalID],768); //256*3
  closefile(f);

  if PalID = pal_lin then //Make greyscale linear Pal
    for i:=0 to 255 do begin
      PalData[pal_lin,i+1,1] := i;
      PalData[pal_lin,i+1,2] := i;
      PalData[pal_lin,i+1,3] := i;
    end;

  Result:=true;
end;

procedure TfrmMain.ListBox1Click(Sender: TObject);
begin
  LoadFont(DataDir+'data\gfx\fonts\'+ListBox1.Items[ListBox1.ItemIndex], fnt_Outline, false);
  PageControl1.ActivePageIndex := 0;
  StatusBar1.Panels.Items[0].Text := ListBox1.Items[ListBox1.ItemIndex];
end;

procedure TfrmMain.btnExportClick(Sender: TObject);
begin
  LoadFont(DataDir+'data\gfx\fonts\'+ListBox1.Items[ListBox1.ItemIndex], fnt_Outline, true);
end;

procedure TfrmMain.Image1MouseMove(Sender: TObject; Shift: TShiftState; X,Y: Integer);
begin
  Shape1.Top  := Y div 32 * 32;
  Shape1.Left := X div 32 * 32;
  StatusBar1.Panels.Items[1].Text := IntToStr(Y div 32)+'; '+IntToStr(X div 32);
  StatusBar1.Panels.Items[2].Text := IntToHex( (((Y div 32)*8)+(X div 32)) ,2);
end;

procedure TfrmMain.Image1MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  Shape3.Top := Shape1.Top;
  Shape3.Left := Shape1.Left;
  PageControl1.ActivePageIndex := 1;
  ActiveLetter := (Y div 32)*32 + X div 32 * 32 + 1;
  ShowLetter(ActiveLetter);
end;

procedure TfrmMain.ShowLetter(aLetter:integer);
var MyBitMap:TBitMap; ci,ck:integer; p,t:integer; aFont:byte; MyRect:TRect;
begin

  aFont:=2; //todo: Should be read from file?
  aLetter := 65;

  MyBitMap := TBitMap.Create;
  MyBitmap.PixelFormat := pf24bit;
  MyBitmap.Width := 32;
  MyBitmap.Height := 32;

  for ci:=0 to FontData.Letters[aLetter].Height-1 do for ck:=0 to FontData.Letters[aLetter].Width do begin
    p := FontPal[byte(aFont)];
    t := FontData.Letters[aLetter].Data[ci*FontData.Letters[aLetter].Height+ck+1]+1;
    MyBitmap.Canvas.Pixels[ck,ci] := PalData[p,t,1]+PalData[p,t,2]*256+PalData[p,t,3]*65536;
  end;

  MyRect := Image2.Canvas.ClipRect;

  Image2.Canvas.StretchDraw(MyRect, MyBitmap); //Draw MyBitmap into Image1
  MyBitmap.Free;
  //
end;

end.
