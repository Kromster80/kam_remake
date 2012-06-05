unit umain;
{$I ..\..\KaM_Remake.inc}
interface
uses
  {$IFDEF FPC} LCLIntf, LResources, {$ENDIF}
  {$IFDEF MSWindows} Windows, {$ENDIF}
  {$IFDEF Unix} LCLType, {$ENDIF}
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs, ExtCtrls,
  StdCtrls, Math, ComCtrls, Buttons, Spin, StrUtils, KromUtils, Constants;


{Globals}
const
  TexWidth = 512; //Connected to TexData, don't change

var
  ExeDir: string;
  DataDir: string;

  FontData: record
    Title: TKMFont;
    Unk1,WordSpacing,CharOffset,Unk3: smallint; //@Lewin: BaseCharHeight?, Unknown, CharSpacingX, LineOffset?
    Pal: array[0..65000]of byte; //Switch to determine if letter is there
    Letters: array[0..65000]of record
      Width,Height: Word;
      Add1,Add2,YOffset,Add4: smallint; //Add1-4 always 0, YOffset could be negative?
      Data: array[1..4096] of byte;
    end;
  end;

  PalData: array [1..12, 1..256, 1..3] of byte;
  SelectedLetter: Integer;
  SettingFromFont: Boolean;

type
  TfrmMain = class(TForm)
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    ListBox1: TListBox;
    Shape1: TShape;
    SpinEdit1: TSpinEdit;
    SpinEdit2: TSpinEdit;
    SpinEdit3: TSpinEdit;
    SpinEdit4: TSpinEdit;
    SpinEdit5: TSpinEdit;
    StatusBar1: TStatusBar;
    Image1: TImage;
    Image3: TImage;
    Image4: TImage;
    Image5: TImage;
    Edit1: TEdit;
    CheckCells: TCheckBox;
    RGPalette: TRadioGroup;
    RefreshData: TButton;
    BitBtn1: TBitBtn;
    btnImportBig: TBitBtn;
    btnExportBig: TBitBtn;
    ScrollBar1: TScrollBar;
    procedure BitBtn1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure RefreshDataClick(Sender: TObject);
    procedure ListBox1Click(Sender: TObject);
    procedure btnExportBigClick(Sender: TObject);
    procedure Image1MouseMove(Sender: TObject; Shift: TShiftState; X,Y: Integer);
    procedure Image1MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure Edit1Change(Sender: TObject);
    procedure CheckCellsClick(Sender: TObject);
    procedure btnImportBigClick(Sender: TObject);
    procedure RGPaletteClick(Sender: TObject);
    procedure SpinEdit1Change(Sender: TObject);
    procedure SpinEdit5Change(Sender: TObject);
    procedure ScrollBar1Change(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    fCharCount: Word;
    fBmp: TBitmap;
    fPaintBox: TPaintBox;
    function GetFontFromFileName(const aFile: string):TKMFont;
    procedure ScanDataForPalettesAndFonts(const aPath: string);
    function LoadFont(const aFilename: string; aFont:TKMFont):boolean;
    function LoadPalette(const aFilename: string; PalID: Byte): Boolean;
    procedure PaintBox(Sender: TObject);
  public
    procedure ShowBigImage(ShowCells, WriteFontToBMP: Boolean);
    procedure ShowPalette(aPal:integer);
  end;



implementation
//{$IFDEF WDC}
{$R *.dfm}
//{$ENDIF}


procedure TfrmMain.FormCreate(Sender: TObject);
begin
  ExeDir := ExtractFilePath(ParamStr(0));
  DataDir := ExeDir;
  if DirectoryExists(ExeDir + '..\..\Data\gfx\Fonts\') then //Remake project location
    DataDir := ExeDir + '..\..\';  //I wonder if it's correct syntax, but it works well [.\Utils\FontEd\..\..\]
  if DirectoryExists(ExeDir + 'Data\gfx\Fonts\') then //Default location
    DataDir := ExeDir;

  ScanDataForPalettesAndFonts(DataDir);

  FontData.Title := fnt_Nil;
  SelectedLetter := 0;

  //Off-screen bitmap which we draw OnPaint event
  fBmp := TBitmap.Create;
  fBmp.PixelFormat := pf24bit;
  fBmp.Width := TexWidth;
  fBmp.Height := TexWidth;

  //Using PaintBox and doing OnPaint event is faster than direct approach
  fPaintBox := TPaintBox.Create(Self);
  fPaintBox.Parent  := Self;
  fPaintBox.Left    := Image1.Left;
  fPaintBox.Top     := Image1.Top;
  fPaintBox.Width   := Image1.Width;
  fPaintBox.Height  := Image1.Height;
  fPaintBox.OnPaint := PaintBox;

  DoubleBuffered := True;
  Image1.Hide;
end;


procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  fBmp.Free;
end;


procedure TfrmMain.BitBtn1Click(Sender: TObject);
var
  f: file;
  I: Integer;
begin
  if FontData.Title = fnt_Nil then
  begin
    MessageBox(Handle, 'Please select editing font first', 'Error', MB_OK);
    Exit;
  end;

  if not RunSaveDialog(SaveDialog1, ListBox1.Items[ListBox1.ItemIndex], DataDir + 'Data\Gfx\Fonts\', 'KaM Fonts|*.fnt', 'fnt') then
    Exit;

  assignfile(f,SaveDialog1.FileName);
  rewrite(f,1);
  blockwrite(f,FontData.Unk1, 8);
  blockwrite(f,FontData.Pal[0], fCharCount);

  //Write font data
  for I := 0 to fCharCount - 1 do
    if FontData.Pal[I] <> 0 then
      with FontData.Letters[I] do
      begin
        blockwrite(f, Width, 4);
        blockwrite(f, Add1, 8);
        blockwrite(f, Data[1], Width*Height);
      end;

  closefile(f);
end;


procedure TfrmMain.RefreshDataClick(Sender: TObject);
begin
  if not DirectoryExists(DataDir) then MessageBox(Self.Handle, 'Data folder not found', 'Error', 0);
  ScanDataForPalettesAndFonts(DataDir);
end;


procedure TfrmMain.ScanDataForPalettesAndFonts(const aPath: string);
var I: Integer; SearchRec: TSearchRec;
begin
  //0. Clear old list
  ListBox1.Items.Clear;

  //1. Palettes
  for I := 1 to length(PalFiles) do
   LoadPalette(aPath+'data\gfx\'+PalFiles[I],I);

  //2. Fonts
  if not DirectoryExists(aPath+'data\gfx\fonts\') then Exit;

  FindFirst(aPath+'data\gfx\fonts\*.fnt', faAnyFile - faDirectory, SearchRec);
  repeat
    ListBox1.Items.Add(SearchRec.Name);
  until (FindNext(SearchRec)<>0);
  FindClose(SearchRec);
end;


procedure TfrmMain.ListBox1Click(Sender: TObject);
begin
  LoadFont(DataDir+'data\gfx\fonts\'+ListBox1.Items[ListBox1.ItemIndex], GetFontFromFileName(ListBox1.Items[ListBox1.ItemIndex]));
  RGPalette.ItemIndex := FontPal[FontData.Title] - 1;
  ShowBigImage(CheckCells.Checked, false);
  fPaintBox.Repaint;
  ShowPalette(FontPal[FontData.Title]);
  Edit1Change(nil);
  StatusBar1.Panels.Items[0].Text := 'Font: '+ListBox1.Items[ListBox1.ItemIndex]+' Palette: '+PalFiles[FontPal[FontData.Title]];

  ScrollBar1.Max := (fCharCount - 256) div 32;
  ScrollBar1.Enabled := ScrollBar1.Min <> ScrollBar1.Max;
end;


function TfrmMain.LoadFont(const aFilename: string; aFont: TKMFont):boolean;
var
  f: file;
  I: Integer;
  IsUnicode: Boolean;
  MaxHeight, MaxWidth: Integer;
begin
  Result := false;
  if not CheckFileExists(aFilename, true) then Exit;

  MaxWidth  := 0;
  MaxHeight := 0;

  assignfile(f,aFilename); reset(f,1);

  blockread(f,FontData.Unk1,8);
  blockread(f,FontData.Pal[0], 65000, I);

  IsUnicode := (I = 65000); //Unicode variant has exactly 65'000 characters

  if IsUnicode then
  begin
    reset(f,1);
    fCharCount := 65000;
    blockread(f,FontData.Unk1, 8);
    blockread(f,FontData.Pal[0], fCharCount);
  end else
  begin
    reset(f,1);
    fCharCount := 256;
    blockread(f,FontData.Unk1,8);
    blockread(f,FontData.Pal[0], fCharCount);
  end;

  //Read font data
  for I := 0 to fCharCount - 1 do
    if FontData.Pal[I] <> 0 then
      with FontData.Letters[I] do begin
        blockread(f, Width, 4);
        blockread(f, Add1, 8);

        Width := Width and $FF;
        Height := Height and $FF;

        MaxHeight := Math.max(MaxHeight, Height);
        MaxWidth := Math.max(MaxWidth, Height);
        blockread(f, Data[1], Width*Height);
      end
    else
      FillChar(FontData.Letters[I], SizeOf(FontData.Letters[I]), #0);

  closefile(f);

  SettingFromFont := true;
  SpinEdit1.Value := FontData.Unk1;
  SpinEdit2.Value := FontData.WordSpacing;
  SpinEdit3.Value := FontData.CharOffset;
  SpinEdit4.Value := FontData.Unk3;
  SpinEdit5.Value := FontData.Letters[127].Width;
  SettingFromFont := false;

  //Remember the font
  FontData.Title := aFont;
end;


procedure TfrmMain.ShowBigImage(ShowCells, WriteFontToBMP: Boolean);
var
  i,k,ci,ck: integer;
  CellX,CellY: integer;
  Pal,t: word;
  TD:array of byte;
begin
  //Compile texture
  SetLength(TD, TexWidth*TexWidth + 1);

  for I := 0 to 255 do
  if I + ScrollBar1.Position * 32 < fCharCount then
    if FontData.Pal[I + ScrollBar1.Position * 32] <> 0 then
    begin
      CellX := ((I mod 16) * 32) + 1;
      CellY := ((I div 16) * 32) + 1;

      with FontData.Letters[I + ScrollBar1.Position * 32] do
      for ci := 0 to Height - 1 do
      for ck := 0 to Width - 1 do
        TD[Min((CellY + ci) * TexWidth + CellX + ck, High(TD))] := Data[ci * Width + ck + 1];
    end;

  Pal := FontPal[FontData.Title];
  for ci := 0 to TexWidth - 1 do
  for ck := 0 to TexWidth - 1 do
  begin
    t := TD[ci*TexWidth+ck]+1;

    //Draw grid lines
    if ShowCells and ((ci mod 32 = 0) or (ck mod 32 = 0)) then
      fBmp.Canvas.Pixels[ck,ci] := $FFFFFF - (PalData[Pal,1,1] + PalData[Pal,1,2] shl 8 + PalData[Pal,1,3] shl 16)
    else
      fBmp.Canvas.Pixels[ck,ci] := PalData[Pal,t,1] + PalData[Pal,t,2] shl 8 + PalData[Pal,t,3] shl 16;
  end;

  if WriteFontToBMP then
  begin
    if not RunSaveDialog(SaveDialog1, '', ExeDir, 'Bitmaps|*.bmp', 'bmp') then Exit;

    //Append used palette to ease up editing, with color samples 16x16px
    fBmp.Height := fBmp.Height + 8*16; //32x8 cells
    for I:=1 to 128 do for k:=1 to TexWidth do begin
      t := ((i-1) div 16)*32 + ((k-1) div 16 mod 32) + 1;
      fBmp.Canvas.Pixels[k-1,TexWidth+i-1] := PalData[Pal, t, 1] + PalData[Pal, t, 2] shl 8 + PalData[Pal, t, 3] shl 16;
    end;

    fBmp.SaveToFile(SaveDialog1.FileName);
  end;

  SetLength(TD, 0);
end;


function TfrmMain.LoadPalette(const aFilename: string; PalID: Byte): Boolean;
var f:file; I: Integer;
begin
  Result := False;
  if not CheckFileExists(aFilename, True) then
    Exit;

  assignfile(f, aFilename);
  reset(f,1);
  blockread(f,PalData[PalID],48); //Unknown and/or unimportant
  blockread(f,PalData[PalID],768); //256*3
  closefile(f);

  if PalID = pal_lin then //Make greyscale linear Pal
    for I := 0 to 255 do begin
      PalData[pal_lin,i+1,1] := i;
      PalData[pal_lin,i+1,2] := i;
      PalData[pal_lin,i+1,3] := i;
    end;

  Result := True;
end;


procedure TfrmMain.PaintBox(Sender: TObject);
begin
  fPaintBox.Canvas.Draw(0, 0, fBmp);
end;


procedure TfrmMain.btnExportBigClick(Sender: TObject);
begin
  ShowBigImage(CheckCells.Checked, true);
  fPaintBox.Repaint;
end;


procedure TfrmMain.Image1MouseMove(Sender: TObject; Shift: TShiftState; X,Y: Integer);
begin
  StatusBar1.Panels.Items[1].Text := 'Character: ' + IntToStr((Y div 32) *16 + X div 32) + ' ('+
                                     IntToHex( (((Y div 32)*16)+(X div 32)) ,2)+'h)';
  StatusBar1.Panels.Items[2].Text :=
  'Width '+int2fix(FontData.Letters[(((Y div 32)*16)+(X div 32))].Width,2)+', '+
  'Height '+int2fix(FontData.Letters[(((Y div 32)*16)+(X div 32))].Height,2)+', '+
  inttostr(FontData.Letters[(((Y div 32)*16)+(X div 32))].Add1)+'? . '+
  inttostr(FontData.Letters[(((Y div 32)*16)+(X div 32))].Add2)+'? . '+
  inttostr(FontData.Letters[(((Y div 32)*16)+(X div 32))].YOffset)+'? . '+
  inttostr(FontData.Letters[(((Y div 32)*16)+(X div 32))].Add4)+'?';
end;


procedure TfrmMain.Image1MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  Shape1.Left := Image1.Left + (X div 32) * 32;
  Shape1.Top := Image1.Top + (Y div 32) * 32;
  SelectedLetter := (Y div 32) *16 + X div 32;

  SpinEdit5.Value := FontData.Letters[SelectedLetter].YOffset;
end;


procedure TfrmMain.ShowPalette(aPal: Integer);
var Bmp: TBitmap; I: Integer; MyRect: TRect;
begin
  Bmp := TBitmap.Create;
  Bmp.PixelFormat := pf24bit;
  Bmp.Width := 8;
  Bmp.Height := 32;

  for I := 0 to 255 do
    Bmp.Canvas.Pixels[I mod 8, I div 8] := PalData[aPal,I+1,1] + PalData[aPal,I+1,2] shl 8 + PalData[aPal,I+1,3] shl 16;

  MyRect := Image3.Canvas.ClipRect;
  Image3.Canvas.StretchDraw(MyRect, Bmp); //Draw MyBitmap into Image1
  Bmp.Free;
end;


procedure TfrmMain.Edit1Change(Sender: TObject);
var
  Bmp: TBitmap;
  I, ci, ck: integer;
  AdvX, Pal, t: integer;
  MyRect: TRect;
  Text: string;
begin
  Bmp := TBitmap.Create;
  Bmp.PixelFormat := pf24bit;
  Bmp.Width := 512;
  Bmp.Height := 20;

  AdvX := 0;

  {$IFDEF FPC} //FPC uses unicode strings in Edit1
    Text := UTF8ToAnsi(Edit1.Text);
  {$ENDIF}
  {$IFDEF WDC} //Delphi uses ansi strings
    Text := Edit1.Text;
  {$ENDIF}

  //Fill area
  Pal := FontPal[FontData.Title];
  Bmp.Canvas.Brush.Color := PalData[Pal,1,1] + PalData[Pal,1,2] shl 8 + PalData[Pal,1,3] shl 16;
  Bmp.Canvas.FillRect(Bmp.Canvas.ClipRect);

  for I:=1 to length(Text) do
  if Text[i]<>' ' then
  begin
    for ci:=0 to FontData.Letters[ord(Text[i])].Height-1 do for ck:=0 to FontData.Letters[ord(Text[i])].Width-1 do begin
      t := FontData.Letters[ord(Text[i])].Data[ci*FontData.Letters[ord(Text[i])].Width+ck+1]+1;
      if t<>1 then //don't bother for clear pixels, speed-up
      Bmp.Canvas.Pixels[ck+AdvX,ci+FontData.Letters[ord(Text[i])].YOffset] := PalData[Pal,t,1] + PalData[Pal,t,2] shl 8 + PalData[Pal,t,3] shl 16;
    end;
    inc(AdvX,FontData.Letters[ord(Text[i])].Width+FontData.CharOffset);
  end else
    inc(AdvX,FontData.WordSpacing);

  //Match phrase bounds
  Bmp.Width := AdvX+1;
  Bmp.Height := 20;

  Image4.Canvas.Brush.Color := PalData[Pal,1,1] + PalData[Pal,1,2] shl 8 + PalData[Pal,1,3] shl 16;
  Image4.Canvas.FillRect(Image4.Canvas.ClipRect);
  Image4.Canvas.Draw( (Image4.Width - Bmp.Width) div 2 , (Image4.Height - Bmp.Height) div 2 + 2, Bmp); //Draw MyBitmap into Image1

  MyRect.Left := (Image5.Width  - Bmp.Width*2 ) div 2;
  MyRect.Top  := (Image5.Height - Bmp.Height*2) div 2;
  MyRect.Right  := MyRect.Left + Bmp.Width*2;
  MyRect.Bottom := MyRect.Top + Bmp.Height*2;

  Image5.Canvas.Brush.Color := PalData[Pal,1,1] + PalData[Pal,1,2] shl 8 + PalData[Pal,1,3] shl 16;
  Image5.Canvas.FillRect(Image5.Canvas.ClipRect);
  Image5.Canvas.StretchDraw(MyRect, Bmp); //Draw MyBitmap into Image1

  Bmp.Free;
end;


function TfrmMain.GetFontFromFileName(const aFile: string): TKMFont;
var I: TKMFont;
begin
  Result := fnt_Nil; //Deliberate error
  for I := High(TKMFont) downto Low(TKMFont) do
    if LeftStr(aFile, Length(FontFileNames[I])) = FontFileNames[I] then
    begin
      Result := I;
      Exit;
    end;
end;


procedure TfrmMain.CheckCellsClick(Sender: TObject);
begin
  ShowBigImage(CheckCells.Checked, false);
  fPaintBox.Repaint;
end;


procedure TfrmMain.btnImportBigClick(Sender: TObject);
  function FindBestPaletteColor(aCol:TColor):Byte;
  var
    I: Integer;
    usePal: Byte; //What palette to use?
    tRMS, RMS: Integer; //How different is sampled color vs. used one
  begin
    RMS := MaxInt;
    Result := 0;
    usePal := FontPal[FontData.Title]; //Use palette from current font
    for I := 1 to 256 do begin
      tRMS := GetLengthSQR(PalData[usePal, I, 1] - (aCol and $FF), PalData[usePal, I, 2] - ((aCol shr 8) and $FF), PalData[usePal, I, 3] - (aCol shr 16) and $FF);
      if (I = 1) or (tRMS < RMS) then begin
        Result := I-1; //Byte = 0..255
        RMS := tRMS;
        if RMS = 0 then Exit;
      end;
    end;
  end;

var
  I,K,ci,ck,x,y:integer;
  MyBitmap: TBitmap;
  LetterID,LetterW,LetterH:integer;
  Pixels:array[1..512,1..512]of byte;
  ErrS:string;
begin

  if FontData.Title = fnt_Nil then begin
    ErrS := 'Please select editing font first';
    MessageBox(Handle, @ErrS[1], 'Error', MB_OK);
    Exit;
  end;

  RunOpenDialog(OpenDialog1, '', ExeDir, 'Bitmaps|*.bmp');
  if not FileExists(OpenDialog1.FileName) then begin
    ErrS := OpenDialog1.FileName + ' couldn''t be found';
    MessageBox(Handle, @ErrS[1], 'Error', MB_OK);
    Exit;
  end;

  MyBitmap := TBitmap.Create;
  MyBitmap.LoadFromFile(OpenDialog1.FileName);
  MyBitmap.PixelFormat := pf24bit;

  if MyBitmap.Width <> TexWidth then begin
    MessageBox(Handle, PWideChar(Format('Image should be %d pixels wide', [TexWidth])),'Error',MB_OK);
    MyBitmap.Free;
    Exit;
  end;

  //Convert 24bit to palette colors
  for I:=1 to TexWidth do for k:=1 to TexWidth do
    Pixels[i,k] := FindBestPaletteColor(MyBitmap.Canvas.Pixels[k-1,i-1]); //Canvas uses [X:Y] addressing, while I prefer [Y:X] order

  MyBitmap.Free;

  //Scan all letter-boxes
  for I := 1 to 16 do for K := 1 to 16 do
  begin
    LetterW := 0;
    LetterH := 0;

    //Scan all pixels of a single letter to determine its dimensions
    //Excluding 1/1 coords which are for grid lines (irregardless of visibility)
    for ci:=2 to 32 do for ck:=2 to 32 do
    begin

      x := (k-1)*32 + ck; //Pixel coords
      y := (i-1)*32 + ci;

      if Pixels[y,x] <> 0 then begin
        LetterW := math.max(LetterW, ck);
        LetterH := math.max(LetterH, ci);
      end;
    end;

    //Remove offset that grid lines have added
    LetterW := math.max(LetterW-1,0);
    LetterH := math.max(LetterH-1,0);
    LetterID := (i-1)*16 + k - 1; //0..255

    FontData.Pal[LetterID] := byte(LetterW*LetterH<>0); //switch

    with FontData.Letters[LetterID] do begin
      Width  := LetterW;
      Height := LetterH;
      for ci:=1 to LetterH do for ck:=1 to LetterW do
      begin
        x := (k-1)*32 + ck + 1; //Pixel coords
        y := (i-1)*32 + ci + 1;
        Data[(ci-1)*LetterW + ck] := Pixels[y,x];
      end;
    end;

  end;

  //Special fixes:
  FontData.Letters[32].Width := 7; //"Space" width

  ShowBigImage(CheckCells.Checked, false); //Show the result
end;


procedure TfrmMain.RGPaletteClick(Sender: TObject);
begin
  FontPal[FontData.Title] := RGPalette.ItemIndex + 1;
  ShowBigImage(CheckCells.Checked, false);
  fPaintBox.Repaint;
  ShowPalette(FontPal[FontData.Title]);
  Edit1Change(nil);
  if ListBox1.ItemIndex <> -1 then
    StatusBar1.Panels.Items[0].Text := 'Font: '+ListBox1.Items[ListBox1.ItemIndex]+' Palette: '+PalFiles[FontPal[FontData.Title]];
end;


procedure TfrmMain.SpinEdit1Change(Sender: TObject);
begin
  if SettingFromFont then Exit;

  if (Sender is TSpinEdit) and (TSpinEdit(Sender).Text = '') then Exit;

  FontData.Unk1        := SpinEdit1.Value;
  FontData.WordSpacing := SpinEdit2.Value;
  FontData.CharOffset  := SpinEdit3.Value;
  FontData.Unk3        := SpinEdit4.Value;

  if Sender = SpinEdit5 then begin
    FontData.Letters[127].Height := 10;
    FontData.Letters[127].Width := SpinEdit5.Value;
    FillChar(FontData.Letters[127].Data[1], 4096, #0);
    FontData.Letters[127].YOffset := 0;
    ShowBigImage(CheckCells.Checked, false);
  end;
  Edit1Change(nil);
end;


procedure TfrmMain.SpinEdit5Change(Sender: TObject);
begin
  if SelectedLetter = 0 then Exit;
  FontData.Letters[SelectedLetter].YOffset := SpinEdit5.Value;
  Edit1Change(nil);
end;


procedure TfrmMain.ScrollBar1Change(Sender: TObject);
begin
  ShowBigImage(CheckCells.Checked, False);
  fPaintBox.Repaint;
end;


initialization
{$IFDEF FPC}
{$I umain.lrs}
{$ENDIF}


end.
