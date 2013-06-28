unit umain;
{$I ..\..\KaM_Remake.inc}
interface
uses
  {$IFDEF FPC} LCLIntf, LResources, {$ENDIF}
  {$IFDEF MSWindows} Windows, {$ENDIF}
  {$IFDEF Unix} LCLType, {$ENDIF}
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs, ExtCtrls,
  StdCtrls, Math, ComCtrls, Buttons, Spin, StrUtils, KromUtils,
  KM_Defaults, KM_ResourceFonts, KM_ResourceFontsEdit, KM_ResourcePalettes,
  Constants; //Declared last to override TKMFont (we could redesign that later, but for now it works okay)


type
  TfrmMain = class(TForm)
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    lbFonts: TListBox;
    Shape1: TShape;
    SpinEdit1: TSpinEdit;
    SpinEdit2: TSpinEdit;
    SpinEdit3: TSpinEdit;
    SpinEdit4: TSpinEdit;
    SpinEdit5: TSpinEdit;
    StatusBar1: TStatusBar;
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
    PaintBox1: TPaintBox;
    procedure BitBtn1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure RefreshDataClick(Sender: TObject);
    procedure lbFontsClick(Sender: TObject);
    procedure btnExportBigClick(Sender: TObject);
    procedure PaintBox1MouseMove(Sender: TObject; Shift: TShiftState; X,Y: Integer);
    procedure PaintBox1MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure Edit1Change(Sender: TObject);
    procedure CheckCellsClick(Sender: TObject);
    procedure btnImportBigClick(Sender: TObject);
    procedure RGPaletteClick(Sender: TObject);
    procedure SpinEdit1Change(Sender: TObject);
    procedure SpinEdit5Change(Sender: TObject);
    procedure ScrollBar1Change(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure PaintBox1Paint(Sender: TObject);
  private
    fCharCount: Word;
    fBmp: TBitmap;
    Pals: TKMPalettes;
    fActivePalette: TKMPal;
    fFnt: TKMFontDataEdit;
    function FontPreferredPalette(aName: string): TKMPal;
    function GetFontFromFileName(const aFile: string):TKMFont;
    procedure ScanDataForPalettesAndFonts(const aPath: string);
    function LoadFont(const aFilename: string):boolean;
  public
    procedure ShowBigImage(ShowCells, WriteFontToBMP: Boolean);
    procedure ShowPalette;
  end;


{Globals}
const
  TexWidth = 512; //Connected to TexData, don't change

var
  ExeDir: string;
  DataDir: string;

  SelectedLetter: Integer;
  SettingFromFont: Boolean;


implementation
//{$IFDEF WDC}
{$R *.dfm}
//{$ENDIF}


function TfrmMain.FontPreferredPalette(aName: string): TKMPal;
const
  FontFileNames: array [0..21] of string = (
    'adam', 'antiqua', 'briefing', 'font01', 'game', 'grey', 'kmlobby0', 'kmlobby1', 'kmlobby2', 'kmlobby3',
    'kmlobby4', 'maina', 'mainb', 'mainmapgold', 'metal', 'mini', 'mininum','outline', 'system', 'won',
    'unicode', 'unicode_game');
  FontPal: array [0..21] of TKMPal = (
   pal_bw, pal_0, pal_map,pal_lin, pal_0, pal_0, pal2_setup,pal2_setup,pal2_setup,pal2_setup,
   pal2_setup, pal_set,pal_lin,pal2_mapgold, pal_0, pal_set, pal_set, pal_0,pal_lin, pal_set2,
   pal2_setup, pal_0);
var
  I: Integer;
begin
  for I := 0 to 21 do
  if LeftStr(aName, Length(FontFileNames[I])) = FontFileNames[I] then
  begin
    Result := FontPal[I];
    Exit;
  end;
  Result := pal_0;
end;


procedure TfrmMain.FormCreate(Sender: TObject);
begin
  Caption := 'KaM Font Editor (' + GAME_REVISION + ')';

  ExeDir := ExtractFilePath(ParamStr(0));
  DataDir := ExeDir;
  if DirectoryExists(ExeDir + '..\..\Data\gfx\Fonts\') then //Remake project location
    DataDir := ExeDir + '..\..\';
  if DirectoryExists(ExeDir + 'Data\gfx\Fonts\') then //Default location
    DataDir := ExeDir;

  Pals := TKMPalettes.Create;

  ScanDataForPalettesAndFonts(DataDir);

  SelectedLetter := 0;

  //Off-screen bitmap which we draw OnPaint event
  fBmp := TBitmap.Create;
  fBmp.PixelFormat := pf24bit;
  fBmp.Width := TexWidth;
  fBmp.Height := TexWidth;

  DoubleBuffered := True;
end;


procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  fBmp.Free;
end;


procedure TfrmMain.BitBtn1Click(Sender: TObject);
begin
  if fFnt = nil then
  begin
    MessageBox(Handle, 'Please select editing font first', 'Error', MB_OK);
    Exit;
  end;

  if not RunSaveDialog(SaveDialog1, lbFonts.Items[lbFonts.ItemIndex], DataDir + 'Data\Gfx\Fonts\', 'KaM Fonts|*.fnt', 'fnt') then
    Exit;

  fFnt.SaveToFont(SaveDialog1.FileName);
end;


procedure TfrmMain.RefreshDataClick(Sender: TObject);
begin
  Assert(DirectoryExists(DataDir), 'Data folder not found');
  ScanDataForPalettesAndFonts(DataDir);
end;


procedure TfrmMain.ScanDataForPalettesAndFonts(const aPath: string);
var
  I: Integer;
  SearchRec: TSearchRec;
begin
  //0. Clear old list
  lbFonts.Clear;

  //1. Palettes
  Pals.LoadPalettes(aPath+'data\gfx\');

  //2. Fonts
  if not DirectoryExists(aPath+'data\gfx\fonts\') then Exit;

  FindFirst(aPath+'data\gfx\fonts\*.fnt', faAnyFile - faDirectory, SearchRec);
  repeat
    lbFonts.Items.Add(SearchRec.Name);
  until (FindNext(SearchRec)<>0);
  FindClose(SearchRec);
end;


procedure TfrmMain.lbFontsClick(Sender: TObject);
var
  FontName: string;
begin
  LoadFont(DataDir+'data\gfx\fonts\'+lbFonts.Items[lbFonts.ItemIndex]);

  fActivePalette := FontPreferredPalette(lbFonts.Items[lbFonts.ItemIndex]);
  RGPalette.ItemIndex := Byte(fActivePalette);
  ShowBigImage(CheckCells.Checked, false);
  PaintBox1.Repaint;
  ShowPalette;
  Edit1Change(nil);
  StatusBar1.Panels.Items[0].Text := 'Font: ' + lbFonts.Items[lbFonts.ItemIndex] +
                                   '  Palette: ' + Pals.PalFile(fActivePalette);

  ScrollBar1.Max := (fCharCount - 256) div 32;
  ScrollBar1.Enabled := ScrollBar1.Min <> ScrollBar1.Max;
end;


function TfrmMain.LoadFont(const aFilename: string):boolean;
var
  f: file;
  I: Integer;
  IsUnicode: Boolean;
  MaxHeight, MaxWidth: Integer;
begin
  Result := false;
  if not CheckFileExists(aFilename, true) then Exit;

  FreeAndNil(fFnt);
  fFnt := TKMFontDataEdit.Create;

  fFnt.LoadFont(aFilename, Pals[fActivePalette]);

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
      with FontData.Letters[I] do
      begin
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
end;


procedure TfrmMain.ShowBigImage(ShowCells, WriteFontToBMP: Boolean);
var
  i,k,ci,ck: integer;
  CellX,CellY: integer;
  t: Byte;
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

  for ci := 0 to TexWidth - 1 do
  for ck := 0 to TexWidth - 1 do
  begin
    t := TD[ci*TexWidth+ck];

    //Draw grid lines
    if ShowCells and ((ci mod 32 = 0) or (ck mod 32 = 0)) then
      fBmp.Canvas.Pixels[ck,ci] := $FFFFFF - (Pals[fActivePalette].Color32(0) and $FFFFFF)
    else
      fBmp.Canvas.Pixels[ck,ci] := Pals[fActivePalette].Color32(t) and $FFFFFF;
  end;

  if WriteFontToBMP then
  begin
    if not RunSaveDialog(SaveDialog1, '', ExeDir, 'Bitmaps|*.bmp', 'bmp') then Exit;

    //Append used palette to ease up editing, with color samples 16x16px
    fBmp.Height := fBmp.Height + 8*16; //32x8 cells
    for I:=1 to 128 do for k:=1 to TexWidth do
    begin
      t := ((i-1) div 16)*32 + ((k-1) div 16 mod 32);
      fBmp.Canvas.Pixels[k-1, TexWidth+i-1] := Pals[fActivePalette].Color32(t);
    end;

    fBmp.SaveToFile(SaveDialog1.FileName);
  end;

  SetLength(TD, 0);
end;


procedure TfrmMain.PaintBox1Paint(Sender: TObject);
begin
  PaintBox1.Canvas.Draw(0, 0, fBmp);
end;


procedure TfrmMain.btnExportBigClick(Sender: TObject);
begin
  Assert(fCharCount = 256);
  ShowBigImage(CheckCells.Checked, true);
  PaintBox1.Repaint;
end;


procedure TfrmMain.PaintBox1MouseMove(Sender: TObject; Shift: TShiftState; X,Y: Integer);
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


procedure TfrmMain.PaintBox1MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  Shape1.Left := PaintBox1.Left + (X div 32) * 32;
  Shape1.Top := PaintBox1.Top + (Y div 32) * 32;
  SelectedLetter := (Y div 32) *16 + X div 32;

  SpinEdit5.Value := FontData.Letters[SelectedLetter].YOffset;
end;


procedure TfrmMain.ShowPalette;
var
  Bmp: TBitmap;
  I: Integer;
begin
  Bmp := TBitmap.Create;
  Bmp.PixelFormat := pf24bit;
  Bmp.Width := 8;
  Bmp.Height := 32;

  for I := 0 to 255 do
    Bmp.Canvas.Pixels[I mod 8, I div 8] := Pals[fActivePalette].Color32(I) and $FFFFFF;

  Image3.Canvas.StretchDraw(Image3.Canvas.ClipRect, Bmp); //Draw MyBitmap into Image1
  Bmp.Free;
end;


procedure TfrmMain.Edit1Change(Sender: TObject);
var
  Bmp: TBitmap;
  I, ci, ck: integer;
  t: Byte;
  AdvX: integer;
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
  Bmp.Canvas.Brush.Color := Pals[fActivePalette].Color32(0);
  Bmp.Canvas.FillRect(Bmp.Canvas.ClipRect);

  for I:=1 to length(Text) do
  if Text[i]<>' ' then
  begin
    for ci:=0 to FontData.Letters[ord(Text[i])].Height-1 do for ck:=0 to FontData.Letters[ord(Text[i])].Width-1 do
    begin
      t := FontData.Letters[ord(Text[i])].Data[ci*FontData.Letters[ord(Text[i])].Width+ck+1];
      if t <> 0 then //don't bother for clear pixels, speed-up
        Bmp.Canvas.Pixels[ck+AdvX,ci+FontData.Letters[ord(Text[i])].YOffset] := Pals[fActivePalette].Color32(t) and $FFFFFF;
    end;
    inc(AdvX,FontData.Letters[ord(Text[i])].Width+FontData.CharOffset);
  end else
    inc(AdvX,FontData.WordSpacing);

  //Match phrase bounds
  Bmp.Width := AdvX+1;
  Bmp.Height := 20;

  Image4.Canvas.Brush.Color := Pals[fActivePalette].Color32(0);
  Image4.Canvas.FillRect(Image4.Canvas.ClipRect);
  Image4.Canvas.Draw( (Image4.Width - Bmp.Width) div 2 , (Image4.Height - Bmp.Height) div 2 + 2, Bmp); //Draw MyBitmap into Image1

  MyRect.Left := (Image5.Width  - Bmp.Width*2 ) div 2;
  MyRect.Top  := (Image5.Height - Bmp.Height*2) div 2;
  MyRect.Right  := MyRect.Left + Bmp.Width*2;
  MyRect.Bottom := MyRect.Top + Bmp.Height*2;

  Image5.Canvas.Brush.Color := Pals[fActivePalette].Color32(0);
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
  PaintBox1.Repaint;
end;


procedure TfrmMain.btnImportBigClick(Sender: TObject);
  function FindBestPaletteColor(aCol:TColor):Byte;
  var
    I: Integer;
    tRMS, RMS: Integer; //How different is sampled color vs. used one
  begin
    RMS := MaxInt;
    Result := 0;
    for I := 0 to 255 do
    begin
      tRMS := GetLengthSQR(
        (Pals[fActivePalette].Color32(I) and $FF) - (aCol and $FF),
        ((Pals[fActivePalette].Color32(I) shr 8) and $FF) - ((aCol shr 8) and $FF),
        ((Pals[fActivePalette].Color32(I) shr 16) and $FF) - ((aCol shr 16) and $FF));
      if (I = 0) or (tRMS < RMS) then
      begin
        Result := I; //Byte = 0..255
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
  Assert(fCharCount = 256);

  if FontData.Title = fnt_Nil then
  begin
    ErrS := 'Please select editing font first';
    MessageBox(Handle, @ErrS[1], 'Error', MB_OK);
    Exit;
  end;

  RunOpenDialog(OpenDialog1, '', ExeDir, 'Bitmaps|*.bmp');
  if not FileExists(OpenDialog1.FileName) then
  begin
    ErrS := OpenDialog1.FileName + ' couldn''t be found';
    MessageBox(Handle, @ErrS[1], 'Error', MB_OK);
    Exit;
  end;

  MyBitmap := TBitmap.Create;
  MyBitmap.LoadFromFile(OpenDialog1.FileName);
  MyBitmap.PixelFormat := pf24bit;

  if MyBitmap.Width <> TexWidth then
  begin
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

      if Pixels[y,x] <> 0 then
      begin
        LetterW := math.max(LetterW, ck);
        LetterH := math.max(LetterH, ci);
      end;
    end;

    //Remove offset that grid lines have added
    LetterW := math.max(LetterW-1,0);
    LetterH := math.max(LetterH-1,0);
    LetterID := (i-1)*16 + k - 1; //0..255

    FontData.Pal[LetterID] := byte(LetterW*LetterH<>0); //switch

    with FontData.Letters[LetterID] do
    begin
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
  fActivePalette := TKMPal(RGPalette.ItemIndex);

  ShowBigImage(CheckCells.Checked, false);
  PaintBox1.Repaint;
  ShowPalette;
  Edit1Change(nil);
  if lbFonts.ItemIndex <> -1 then
    StatusBar1.Panels.Items[0].Text := 'Font: ' + lbFonts.Items[lbFonts.ItemIndex] +
                                     '  Palette: ' + Pals.PalFile(fActivePalette);
end;


procedure TfrmMain.SpinEdit1Change(Sender: TObject);
begin
  if SettingFromFont then Exit;

  if (Sender is TSpinEdit) and (TSpinEdit(Sender).Text = '') then Exit;

  FontData.Unk1        := SpinEdit1.Value;
  FontData.WordSpacing := SpinEdit2.Value;
  FontData.CharOffset  := SpinEdit3.Value;
  FontData.Unk3        := SpinEdit4.Value;

  if Sender = SpinEdit5 then
  begin
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
  PaintBox1.Repaint;
end;


initialization
{$IFDEF FPC}
{$I umain.lrs}
{$ENDIF}


end.
