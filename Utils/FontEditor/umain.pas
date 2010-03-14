unit umain;
{$IFDEF FPC} {$Mode Delphi} {$ENDIF}

interface
uses
  {$IFDEF FPC} LCLIntf, LResources, {$ENDIF}
  Windows, Messages, SysUtils, FileCtrl, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, Math, ComCtrls, Buttons, StrUtils, KromUtils, Constants;


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

  { TfrmMain }

  TfrmMain = class(TForm)
    Label3: TLabel;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    BitBtn1: TBitBtn;
    ListBox1: TListBox;
    RefreshData: TButton;
    PageControl1: TPageControl;
    Shape1: TShape;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    Image1: TImage;
    Image2: TImage;
    Image3: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Edit1: TEdit;
    Image4: TImage;
    Image5: TImage;
    StatusBar1: TStatusBar;
    imgColourSelected: TImage;
    RadioGroup1: TRadioGroup;
    btnExportBig: TBitBtn;
    CheckCells: TCheckBox;
    btnImportBig: TBitBtn;
    procedure FormCreate(Sender: TObject);
    procedure RefreshDataClick(Sender: TObject);
    procedure ListBox1Click(Sender: TObject);
    procedure btnExportBigClick(Sender: TObject);
    procedure Image1MouseMove(Sender: TObject; Shift: TShiftState; X,Y: Integer);
    procedure Image1MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure Edit1Change(Sender: TObject);
    procedure Image3MouseMove(Sender: TObject; Shift: TShiftState; X,Y: Integer);
    procedure Image3MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure RadioGroup1Click(Sender: TObject);
    procedure CheckCellsClick(Sender: TObject);
    procedure btnImportBigClick(Sender: TObject);
  private
    { Private declarations }
    fFileEditing: string;
    fColourSelected: byte;
    fCurrentFont: TKMFont;
    function GetFontFromFileName(aFile:string):TKMFont;
    procedure SetFileEditing(aFile:string);
    procedure SetColourSelected(aColour:byte);
    procedure ScanDataForPalettesAndFonts(aPath:string);
    function LoadFont(filename:string; aFont:TKMFont):boolean;
    function LoadPalette(filename:string; PalID:byte):boolean;
  public
    { Public declarations }
    property FileEditing: string read fFileEditing write SetFileEditing;
    property ColourSelected: byte read fColourSelected write SetColourSelected;
    procedure ShowBigImage(ShowCells, WriteFontToBMP:boolean);
    procedure ShowPalette(aPal:integer);
    procedure ShowLetter(aLetter:integer);
  end;

 var
  frmMain: TfrmMain;

implementation
{$IFDEF VER140}
{$R *.dfm}
{$ENDIF}


procedure TfrmMain.FormCreate(Sender: TObject);
begin
  ExeDir := IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName));
  DataDir := ExeDir;
  if DirectoryExists(ExeDir+'..\..\Data\gfx\Fonts\') then //Default location
    DataDir := ExeDir+'..\..\';  //I wonder if it's correct syntax, but it works well [.\Utils\FontEd\..\..\]
  ScanDataForPalettesAndFonts(DataDir);
end;

procedure TfrmMain.RefreshDataClick(Sender: TObject);
begin
  if not DirectoryExists(DataDir) then MessageBox(Self.Handle, 'Data folder not found', 'Error', 0);
  ScanDataForPalettesAndFonts(DataDir);
end;


procedure TfrmMain.ScanDataForPalettesAndFonts(aPath:string);
var i:integer; SearchRec:TSearchRec;
begin
  //0. Clear old list
  ListBox1.Items.Clear;

  //1. Palettes
  for i:=1 to length(PalFiles) do
   LoadPalette(aPath+'data\gfx\'+PalFiles[i],i);

  //2. Fonts
  if not DirectoryExists(aPath+'data\gfx\fonts\') then exit;

  ChDir(aPath+'data\gfx\fonts');
  FindFirst('*', faAnyFile, SearchRec);

  repeat
    if (SearchRec.Attr and faDirectory = 0)and(SearchRec.Name<>'.')and(SearchRec.Name<>'..') then
      ListBox1.Items.Add(SearchRec.Name);
  until (FindNext(SearchRec)<>0);

  FindClose(SearchRec);
end;


procedure TfrmMain.ListBox1Click(Sender: TObject);
begin
  LoadFont(DataDir+'data\gfx\fonts\'+ListBox1.Items[ListBox1.ItemIndex], GetFontFromFileName(ListBox1.Items[ListBox1.ItemIndex]));
  if PageControl1.ActivePageIndex = 0 then ShowBigImage(CheckCells.Checked, false);
  //PageControl1.ActivePageIndex := 0;
  StatusBar1.Panels.Items[0].Text := 'Font: '+ListBox1.Items[ListBox1.ItemIndex];
end;


procedure TfrmMain.SetFileEditing(aFile:string);
begin
  if not FileExists(aFile) then exit;
  fFileEditing := aFile;
  Caption := 'KaM Font Editor - '+aFile;
end;


function TfrmMain.LoadFont(filename:string; aFont:TKMFont):boolean;
var
  f:file;
  a,b,c,d:word;
  i,k,ci,ck:integer;
  MaxHeight, MaxWidth:integer;
begin
  Result := false;
  if not CheckFileExists(filename, true) then exit;

  MaxWidth  := 0;
  MaxHeight := 0;

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
        MaxHeight := Math.max(MaxHeight,Height);
        MaxWidth := Math.max(MaxWidth,Height);
        blockread(f, Data[1], Width*Height);
      end;
  closefile(f);

  //Special fixes:
  FontData.Letters[32].Width:=7; //"Space" width

  {if aFont=fnt_game then
  for i:=0 to 255 do
    if FontData.Pal[i]<>0 then
      for k:=1 to 4096 do
        if FontData.Letters[i].Data[k]<>0 then
          FontData.Letters[i].Data[k]:=218; //Light grey color in Pal2}

  fCurrentFont := aFont;
end;


procedure TfrmMain.ShowBigImage(ShowCells, WriteFontToBMP:boolean);
const
  TexWidth=512; //Connected to TexData, don't change
var
  i,ci,ck:integer;
  CellX,CellY:integer;
  p,t:byte;
  TD:array of byte;
  MyBitMap:TBitMap;
begin
  //Compile texture
  setlength(TD, TexWidth*TexWidth + 1);
  FillChar(TD[0], TexWidth*TexWidth + 1, $05); //Make some background to see real offsets

  for i:=0 to 255 do
    if FontData.Pal[i]<>0 then
      with FontData.Letters[i] do begin

        CellX := ((i mod 16)*32)+1;
        CellY := ((i div 16)*32)+1;

        for ci:=0 to Height-1 do for ck:=0 to Width-1 do
          TD[(CellY + ci) * TexWidth + CellX + ck] := Data[ci * Width + ck + 1];
      end;

  MyBitMap := TBitMap.Create;
  MyBitmap.PixelFormat := pf24bit;
  MyBitmap.Width := TexWidth;
  MyBitmap.Height := TexWidth;

  for ci:=0 to TexWidth-1 do for ck:=0 to TexWidth-1 do begin
    p:=FontPal[byte(fCurrentFont)];
    //p:=i;
    t:=TD[ci*TexWidth+ck]+1;

    //Draw grid lines
    if ShowCells and ((ci mod 32 = 0) or (ck mod 32 = 0)) and (t=1) then
      MyBitmap.Canvas.Pixels[ck,ci] := $00FF00
    else
      MyBitmap.Canvas.Pixels[ck,ci] := PalData[p,t,1]+PalData[p,t,2]*256+PalData[p,t,3]*65536;
  end;

  if WriteFontToBMP then begin
    RunSaveDialog(SaveDialog1, '', ExeDir, 'Bitmaps|*.bmp', 'bmp');
    MyBitmap.SaveToFile(SaveDialog1.FileName);
  end;

  Image1.Canvas.Draw(0, 0, MyBitmap); //Draw MyBitmap into Image1
  MyBitmap.Free;
  setlength(TD,0);

  {if PageControl1.ActivePageIndex = 1 then begin
    ActiveLetter := 65; //Letter "A"
    RadioGroup1.ItemIndex := FontPal[byte(fCurrentFont)] -1;
    ShowPalette(FontPal[byte(fCurrentFont)]);
    ShowLetter(ActiveLetter);
  end;}
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


procedure TfrmMain.btnExportBigClick(Sender: TObject);
begin
  ShowBigImage(CheckCells.Checked, true);
end;


procedure TfrmMain.Image1MouseMove(Sender: TObject; Shift: TShiftState; X,Y: Integer);
begin
  StatusBar1.Panels.Items[1].Text := 'Coordinates: ' + IntToStr(Y div 32)+'; '+IntToStr(X div 32);
  StatusBar1.Panels.Items[2].Text := 'Hex code: ' + IntToHex( (((Y div 32)*8)+(X div 32)) ,2);
end;


procedure TfrmMain.Image1MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  PageControl1.ActivePageIndex := 1;
  ActiveLetter := (Y div 32)*16 + X div 32;
  ShowPalette(FontPal[byte(fCurrentFont)]);
  ShowLetter(ActiveLetter);
end;


procedure TfrmMain.ShowPalette(aPal:integer);
var MyBitMap:TBitMap; i:integer; MyRect:TRect;
begin
  MyBitMap := TBitMap.Create;
  MyBitmap.PixelFormat := pf24bit;
  MyBitmap.Width := 32;
  MyBitmap.Height := 8;

  for i:=0 to 255 do
    MyBitmap.Canvas.Pixels[i mod 32, i div 32] := PalData[aPal,i+1,1]+PalData[aPal,i+1,2]*256+PalData[aPal,i+1,3]*65536;
  //
  MyRect := Image3.Canvas.ClipRect;
  Image3.Canvas.StretchDraw(MyRect, MyBitmap); //Draw MyBitmap into Image1
  MyBitmap.Free;
end;


procedure TfrmMain.ShowLetter(aLetter:integer);
var
  MyBitMap:TBitMap;
  i,k:integer;
  Pal,Col:integer;
  aFont:byte;
  MyRect:TRect;
begin
  aFont := byte(fCurrentFont);

  MyBitMap := TBitMap.Create;
  MyBitmap.PixelFormat := pf24bit;
  MyBitmap.Width := 24;
  MyBitmap.Height := 24;

  for i:=0 to FontData.Letters[aLetter].Height-1 do for k:=0 to FontData.Letters[aLetter].Width-1 do begin
    Pal := FontPal[byte(aFont)];
    Col := FontData.Letters[aLetter].Data[i*FontData.Letters[aLetter].Width+k+1]+1;
    MyBitmap.Canvas.Pixels[k,i] := PalData[Pal,Col,1] + PalData[Pal,Col,2] shl 8 + PalData[Pal,Col,3] shl 16;
  end;

  MyRect := Image2.Canvas.ClipRect;

  Image2.Canvas.StretchDraw(MyRect, MyBitmap); //Draw MyBitmap into Image1
  MyBitmap.Free;
  Edit1Change(Edit1);
end;


procedure TfrmMain.Edit1Change(Sender: TObject);
var MyBitMap:TBitMap; i,ci,ck:integer; AdvX,Pal,t:integer; aFont:TKMFont; MyRect:TRect;
begin
  MyBitMap := TBitMap.Create;
  MyBitmap.PixelFormat := pf24bit;
  MyBitmap.Width := 512;
  MyBitmap.Height := 40;

  AdvX := 0;
  aFont := fCurrentFont;

  //Fill area
  Pal := FontPal[byte(aFont)];
  MyBitmap.Canvas.Brush.Color := PalData[Pal,1,1] + PalData[Pal,1,2] shl 8 + PalData[Pal,1,3] shl 16;
  MyBitmap.Canvas.FillRect(MyBitmap.Canvas.ClipRect);

  for i:=1 to length(Edit1.Text) do
  begin
    for ci:=0 to FontData.Letters[ord(Edit1.Text[i])].Height-1 do for ck:=0 to FontData.Letters[ord(Edit1.Text[i])].Width-1 do begin
      t := FontData.Letters[ord(Edit1.Text[i])].Data[ci*FontData.Letters[ord(Edit1.Text[i])].Width+ck+1]+1;
      MyBitmap.Canvas.Pixels[ck+AdvX,ci] := PalData[Pal,t,1] + PalData[Pal,t,2] shl 8 + PalData[Pal,t,3] shl 16;
    end;
    inc(AdvX,FontData.Letters[ord(Edit1.Text[i])].Width);
  end;

  //Match phrase bounds
  MyBitmap.Width := AdvX;
  MyBitmap.Height := 20;

  Image4.Canvas.Brush.Color := PalData[Pal,1,1] + PalData[Pal,1,2] shl 8 + PalData[Pal,1,3] shl 16;
  Image4.Canvas.FillRect(Image4.Canvas.ClipRect);
  Image4.Canvas.Draw( (Image4.Width - MyBitmap.Width) div 2 , (Image4.Height - MyBitmap.Height) div 2 , MyBitmap); //Draw MyBitmap into Image1

  MyRect.Left := (Image5.Width  - MyBitmap.Width*2 ) div 2;
  MyRect.Top  := (Image5.Height - MyBitmap.Height*2) div 2;
  MyRect.Right  := MyRect.Left + MyBitmap.Width*2;
  MyRect.Bottom := MyRect.Top + MyBitmap.Height*2;

  Image5.Canvas.Brush.Color := PalData[Pal,1,1] + PalData[Pal,1,2] shl 8 + PalData[Pal,1,3] shl 16;
  Image5.Canvas.FillRect(Image5.Canvas.ClipRect);
  Image5.Canvas.StretchDraw(MyRect, MyBitmap); //Draw MyBitmap into Image1

  MyBitmap.Free;
end;


procedure TfrmMain.Image3MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  if not (ssLeft in Shift) then exit;

  ColourSelected := EnsureRange(((Y div 16)*32) + (X div 16),0,255);
  Shape1.Left := Image3.Left + (X div 16)*16 -1; //-1 to compensate outline width
  Shape1.Top  := Image3.Top  + (Y div 16)*16 -1;
end;


procedure TfrmMain.Image3MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then Image3MouseMove(Sender,[ssLeft],X,Y);
end;


procedure TfrmMain.SetColourSelected(aColour:byte);
var MyBitMap:TBitMap; MyRect:TRect; Pal:integer;
begin
  fColourSelected := aColour;
  MyBitMap := TBitMap.Create;
  MyBitmap.PixelFormat := pf24bit;
  MyBitmap.Width := 1;
  MyBitmap.Height := 1;
  Pal := FontPal[byte(fCurrentFont)];

  MyBitmap.Canvas.Pixels[0, 0] := PalData[Pal,ColourSelected+1,1]+PalData[Pal,ColourSelected+1,2]*256+PalData[Pal,ColourSelected+1,3]*65536;
  //
  MyRect := imgColourSelected.Canvas.ClipRect;
  imgColourSelected.Canvas.StretchDraw(MyRect, MyBitmap); //Draw MyBitmap into imgColourSelected
  MyBitmap.Free;
end;


function TfrmMain.GetFontFromFileName(aFile:string):TKMFont;
var i: TKMFont;
begin
  for i := low(TKMFont) to high(TKMFont) do
    if LeftStr(aFile,Length(FontFileNames[i])) = FontFileNames[i] then
    begin
      Result := i;
      exit;
    end;
end;


procedure TfrmMain.RadioGroup1Click(Sender: TObject);
begin
  ActiveLetter := 65; //Letter "A"
  FontPal[byte(fCurrentFont)] := RadioGroup1.ItemIndex +1;
  ShowPalette(FontPal[byte(fCurrentFont)]);
  ShowLetter(ActiveLetter);
end;


procedure TfrmMain.CheckCellsClick(Sender: TObject);
begin
  ShowBigImage(CheckCells.Checked, false);
end;


procedure TfrmMain.btnImportBigClick(Sender: TObject);
var
  i,k,ci,ck,id:integer;
  MyBitmap:TBitmap;
  LetterW,LetterH:integer;
begin
  RunOpenDialog(OpenDialog1, '', ExeDir, 'Bitmaps|*.bmp');

  MyBitmap := TBitmap.Create;
  MyBitmap.LoadFromFile(OpenDialog1.FileName);

  //Scan all letter-boxes
  for i:=1 to 16 do for k:=1 to 16 do
  begin

    LetterW := 0;
    LetterH := 0;

    //Scan all pixels of a single letter to determine its dimensions
    //Excluding 1/1 coords which are for grid lines (irregardless of visibility)
    for ci:=2 to 32 do for ck:=2 to 32 do
    begin
      //if MyBitmap

    end;

    id := (i-1)*16 + k; //1..256

  end;
end;



initialization
{$IFDEF FPC}
{$I umain.lrs}
{$ENDIF}

end.
