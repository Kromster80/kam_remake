unit umain;
{$I ..\..\KaM_Remake.inc}
interface
uses
  {$IFDEF FPC} LCLIntf, LResources, {$ENDIF}
  {$IFDEF MSWindows} Windows, {$ENDIF}
  {$IFDEF Unix} LCLType, {$ENDIF}
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs, ExtCtrls,
  StdCtrls, Math, ComCtrls, Buttons, Spin, StrUtils, KromUtils,
  KM_Defaults, KM_ResFonts, KM_ResFontsEdit,
  Constants; //Declared last to override TKMFont (we could redesign that later, but for now it works okay)


type
  TfrmMain = class(TForm)
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    lbFonts: TListBox;
    Shape1: TShape;
    StatusBar1: TStatusBar;
    Image4: TImage;
    Image5: TImage;
    Edit1: TEdit;
    CheckCells: TCheckBox;
    btnSaveFont: TBitBtn;
    btnImportBitmap: TBitBtn;
    btnExportBitmap: TBitBtn;
    ScrollBar1: TScrollBar;
    PaintBox1: TPaintBox;
    GroupBox1: TGroupBox;
    seLetterY: TSpinEdit;
    Label7: TLabel;
    GroupBox2: TGroupBox;
    seCharSpacing: TSpinEdit;
    seLineSpacing: TSpinEdit;
    seWordSpacing: TSpinEdit;
    seBaseHeight: TSpinEdit;
    Label6: TLabel;
    Label5: TLabel;
    Label4: TLabel;
    Label3: TLabel;
    seAllYOffset: TSpinEdit;
    Label8: TLabel;
    GroupBox3: TGroupBox;
    sePadRight: TSpinEdit;
    sePadLeft: TSpinEdit;
    sePadBottom: TSpinEdit;
    sePadTop: TSpinEdit;
    Label2: TLabel;
    Label1: TLabel;
    procedure btnSaveFontClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure lbFontsClick(Sender: TObject);
    procedure btnExportPngClick(Sender: TObject);
    procedure PaintBox1MouseMove(Sender: TObject; Shift: TShiftState; X,Y: Integer);
    procedure PaintBox1MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure Edit1Change(Sender: TObject);
    procedure CheckCellsClick(Sender: TObject);
    procedure btnImportPngClick(Sender: TObject);
    procedure seFontPropsChange(Sender: TObject);
    procedure seLetterYChange(Sender: TObject);
    procedure ScrollBar1Change(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure PaintBox1Paint(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    fBmp: TBitmap;
    fFnt: TKMFontDataEdit;
    fCellX: Byte;
    fCellY: Byte;
    fRows: Word;
    fCols: Word;
    fSelectedLetter: Integer;
    fUpdating: Boolean;
    procedure ScanFonts(const aPath: string);
    procedure LoadFont(const aFilename: string);
  public
    procedure ShowBigImage(aShowCells: Boolean);
  end;


const
  TEX_SIZE = 512;
  BG_COLOR = $AF6B6B;
var
  ExeDir: string;
  DataDir: string;


implementation
{$R *.dfm}


procedure TfrmMain.FormCreate(Sender: TObject);
begin
  Caption := 'KaM FontX Editor (' + GAME_REVISION + ')';

  ExeDir := ExtractFilePath(ParamStr(0));
  DataDir := ExeDir;
  if DirectoryExists(ExeDir + '..\..\data\gfx\fonts\') then //Remake project location
    DataDir := ExeDir + '..\..\';
  if DirectoryExists(ExeDir + 'data\gfx\fonts\') then //Default location
    DataDir := ExeDir;

  ScanFonts(DataDir);

  fSelectedLetter := 0;

  //Off-screen bitmap which we draw OnPaint event
  fBmp := TBitmap.Create;
  fBmp.PixelFormat := pf24bit;
  fBmp.Width := TEX_SIZE;
  fBmp.Height := TEX_SIZE;

  DoubleBuffered := True;
end;


procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  fBmp.Free;
end;


procedure TfrmMain.FormResize(Sender: TObject);
begin
  fBmp.Width := PaintBox1.Width;
  fBmp.Height := PaintBox1.Height;

  ShowBigImage(CheckCells.Checked);
  PaintBox1.Repaint;
end;


procedure TfrmMain.btnSaveFontClick(Sender: TObject);
begin
  if fFnt = nil then
  begin
    MessageBox(Handle, 'Please select editing font first', 'Error', MB_OK);
    Exit;
  end;

  if not RunSaveDialog(SaveDialog1, lbFonts.Items[lbFonts.ItemIndex], DataDir + 'Data\Gfx\Fonts\', 'KaM FontX|*.fntx', 'fntx') then
    Exit;

  fFnt.SaveToFontX(SaveDialog1.FileName);
end;


procedure TfrmMain.ScanFonts(const aPath: string);
var
  SearchRec: TSearchRec;
begin
  lbFonts.Clear;

  if not DirectoryExists(aPath + 'data\gfx\fonts\') then Exit;

  FindFirst(aPath + 'data\gfx\fonts\*.fntx', faAnyFile - faDirectory, SearchRec);
  repeat
    lbFonts.Items.Add(SearchRec.Name);
  until (FindNext(SearchRec) <> 0);
  FindClose(SearchRec);
end;


procedure TfrmMain.lbFontsClick(Sender: TObject);
begin
  LoadFont(DataDir + 'data\gfx\fonts\' + lbFonts.Items[lbFonts.ItemIndex]);

  ShowBigImage(CheckCells.Checked);
  PaintBox1.Repaint;
  Edit1Change(nil);
  StatusBar1.Panels.Items[0].Text := 'Font: ' + lbFonts.Items[lbFonts.ItemIndex];
end;


procedure TfrmMain.LoadFont(const aFilename: string);
begin
  if not FileExists(aFilename) then Exit;

  FreeAndNil(fFnt);
  fFnt := TKMFontDataEdit.Create;

  fFnt.LoadFontX(aFilename);

  fCellX := fFnt.MaxLetterWidth + 1;
  fCellY := fFnt.MaxLetterHeight + 1;

  fUpdating := True;
  seBaseHeight.Value := fFnt.BaseHeight;
  seWordSpacing.Value := fFnt.WordSpacing;
  seCharSpacing.Value := fFnt.CharSpacing;
  seLineSpacing.Value := fFnt.Unknown;
  fUpdating := False;
end;


procedure TfrmMain.ShowBigImage(aShowCells: Boolean);
var
  I,K,L,M: Integer;
  pX, pY: Word;
  T, R: Cardinal;
  Let: TKMLetter;
  offset: Word;
  letter: Integer;
begin
  fBmp.Canvas.Brush.Color := BG_COLOR;
  fBmp.Canvas.FillRect(fBmp.Canvas.ClipRect);

  if fFnt = nil then Exit;

  fCols := fBmp.Width div fCellX;
  fRows := fBmp.Height div fCellY;

  offset := ScrollBar1.Position;

  for I := 0 to fRows - 1 do
  for K := 0 to fCols - 1 do
  begin
    letter := offset + I * fCols + K;

    //Dont render anything beyond 65535 if we have Scroll positioned past last char
    if letter > 65535 then Break;
    if fFnt.Used[letter] = 0 then Continue;

    Let := fFnt.Letters[letter];
    for L := 0 to Let.Height - 1 do
    for M := 0 to Let.Width - 1 do
    begin
      pX := Round(Let.u1 * fFnt.TexSizeX) + M;
      pY := Round(Let.v1 * fFnt.TexSizeY) + L;
      T := fFnt.TexData[Let.AtlasId][pY * fFnt.TexSizeX + pX];

      //Blend with background
      R := Round(Lerp(BG_COLOR and $FF, T and $FF, T shr 24 / 255)) +
           Round(Lerp(BG_COLOR shr 8 and $FF, T shr 8 and $FF, T shr 24 / 255)) shl 8 +
           Round(Lerp(BG_COLOR shr 16 and $FF, T shr 16 and $FF, T shr 24 / 255)) shl 16;

      fBmp.Canvas.Pixels[K * fCellX + M + 1, I * fCellY + L + 1] := R;
    end;
  end;

  if aShowCells then
  begin
    fBmp.Canvas.Pen.Color := $AAAAAA;

    for I := 0 to fRows - 1 do
    begin
      fBmp.Canvas.MoveTo(0, I * fCellY);
      fBmp.Canvas.LineTo(fBmp.Width - 1, I * fCellY);
    end;

    for K := 0 to fCols - 1 do
    begin
      fBmp.Canvas.MoveTo(K * fCellX, 0);
      fBmp.Canvas.LineTo(K * fCellX, fBmp.Height - 1);
    end;
  end;

  PaintBox1.Canvas.Draw(0, 0, fBmp);
end;


procedure TfrmMain.PaintBox1Paint(Sender: TObject);
begin
  PaintBox1.Canvas.Draw(0, 0, fBmp);
end;


procedure TfrmMain.btnExportPngClick(Sender: TObject);
begin
  if not RunSaveDialog(SaveDialog1, '', ExeDir, 'PNG images|*.png', 'png') then Exit;

  fFnt.ExportGridPng(SaveDialog1.FileName, Rect(sePadLeft.Value, sePadTop.Value, sePadRight.Value, sePadBottom.Value));
end;


procedure TfrmMain.btnImportPngClick(Sender: TObject);
begin
  Assert(fFnt <> nil, 'Import needs donor font selected');

  if not RunOpenDialog(OpenDialog1, '', ExeDir, 'PNG images|*.png') then Exit;

  fFnt.ImportGridPng(OpenDialog1.FileName);

  fCellX := fFnt.MaxLetterWidth + 1;
  fCellY := fFnt.MaxLetterHeight + 1;

  ShowBigImage(CheckCells.Checked);
  PaintBox1.Repaint;
  Edit1Change(nil);
end;


procedure TfrmMain.PaintBox1MouseMove(Sender: TObject; Shift: TShiftState; X,Y: Integer);
var
  id, offset: Word;
  Let: TKMLetter;
begin
  if fFnt = nil then Exit;

  //Information about character below
  offset := ScrollBar1.Position;
  id := offset + (Y div fCellY) * fCols + X div fCellX;
  Let := fFnt.Letters[id];
  StatusBar1.Panels.Items[1].Text := 'Character: ' + IntToStr(id) + ' (' + IntToHex(id, 2) + 'h)';
  StatusBar1.Panels.Items[2].Text := Format('Width %d, Height %d, %d . %d',
                                            [Let.Width, Let.Height, Let.AtlasId, Let.YOffset]);
end;


procedure TfrmMain.PaintBox1MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  offset: Word;
begin
  if fFnt = nil then Exit;

  offset := ScrollBar1.Position;

  fSelectedLetter := offset + (Y div fCellY) * fCols + X div fCellX;
  GroupBox1.Caption := ' Letter: ' + IntToStr(fSelectedLetter) + ' (' + IntToHex(fSelectedLetter, 2) + 'h) ';

  Shape1.Width := fFnt.Letters[fSelectedLetter].Width + 4;
  Shape1.Height := fFnt.Letters[fSelectedLetter].Height + 4;

  Shape1.Left := PaintBox1.Left + (fSelectedLetter - offset) mod fCols * fCellX - 1;
  Shape1.Top := PaintBox1.Top + (fSelectedLetter - offset) div fCols * fCellY - 1;

  fUpdating := True;
  seLetterY.Value := fFnt.Letters[fSelectedLetter].YOffset;
  fUpdating := False;
end;


procedure TfrmMain.Edit1Change(Sender: TObject);
const
  PAD = 2;
var
  Bmp: TBitmap;
  I, L, M: Integer;
  srcCol, dstCol: Integer;
  AdvX: Integer;
  MyRect: TRect;
  Text: UnicodeString;
  Let: TKMLetter;
  pX, pY: Word;
  alpha: Byte;
begin
  Bmp := TBitmap.Create;
  Bmp.PixelFormat := pf32bit;
  Bmp.Width := 512;
  Bmp.Height := 22;

  AdvX := 0;

  {$IFDEF FPC} //FPC uses unicode strings in Edit1
    Text := Trim(UTF8ToAnsi(Edit1.Text));
  {$ENDIF}
  {$IFDEF WDC} //Delphi uses ansi strings
    Text := Trim(Edit1.Text);
  {$ENDIF}

  //Fill area
  Bmp.Canvas.Brush.Color := BG_COLOR;
  Bmp.Canvas.FillRect(Bmp.Canvas.ClipRect);

  for I := 1 to Length(Text) do
  if Text[I] <> ' ' then
  begin
    Let := fFnt.Letters[Ord(Text[I])];

    for L := 0 to Let.Height - 1 do
    for M := 0 to Let.Width - 1 do
    begin
      pX := Round(Let.u1 * fFnt.TexSizeX) + M;
      pY := Round(Let.v1 * fFnt.TexSizeY) + L;

      srcCol := fFnt.TexData[Let.AtlasId][pY * fFnt.TexSizeX + pX] and $FFFFFF;
      dstCol := Bmp.Canvas.Pixels[AdvX + M, Let.YOffset + L + PAD] and $FFFFFF;
      alpha := 255 - (fFnt.TexData[Let.AtlasId][pY * fFnt.TexSizeX + pX] shr 24) and $FF;
      //srcCol + (dstCol - srcCol) * alpha
      Bmp.Canvas.Pixels[AdvX + M, Let.YOffset + L + PAD] :=
        ((srcCol and $FF) + ((dstCol and $FF - srcCol and $FF) * alpha) div 256) +
        ((srcCol shr 8 and $FF) + ((dstCol shr 8 and $FF - srcCol shr 8 and $FF) * alpha) div 256) shl 8 +
        ((srcCol shr 16 and $FF) + ((dstCol shr 16 and $FF - srcCol shr 16 and $FF) * alpha) div 256) shl 16;
    end;

    Inc(AdvX, Let.Width + fFnt.CharSpacing);
  end else
    Inc(AdvX, fFnt.WordSpacing);

  //Match phrase bounds
  Bmp.Width := Max(AdvX - Min(fFnt.CharSpacing, 0), 0); //Revert last char overlap (if spacing is negative)
  Bmp.Height := 20;

  Image4.Canvas.Brush.Color := BG_COLOR;
  Image4.Canvas.FillRect(Image4.Canvas.ClipRect);
  Image4.Canvas.Draw((Image4.Width - Bmp.Width) div 2, (Image4.Height - Bmp.Height) div 2 + 2, Bmp); //Draw MyBitmap into Image1

  MyRect.Left := (Image5.Width  - Bmp.Width*2 ) div 2;
  MyRect.Top  := (Image5.Height - Bmp.Height*2) div 2;
  MyRect.Right  := MyRect.Left + Bmp.Width*2;
  MyRect.Bottom := MyRect.Top + Bmp.Height*2;

  Image5.Canvas.Brush.Color := BG_COLOR;
  Image5.Canvas.FillRect(Image5.Canvas.ClipRect);
  Image5.Canvas.StretchDraw(MyRect, Bmp); //Draw MyBitmap into Image1

  Bmp.Free;
end;


procedure TfrmMain.CheckCellsClick(Sender: TObject);
begin
  ShowBigImage(CheckCells.Checked);
  PaintBox1.Repaint;
end;


procedure TfrmMain.seFontPropsChange(Sender: TObject);
var
  I: Integer;
begin
  if fUpdating then Exit;

  if (Sender is TSpinEdit) and (TSpinEdit(Sender).Text = '') then Exit;

  fFnt.BaseHeight  := seBaseHeight.Value;
  fFnt.WordSpacing := seWordSpacing.Value;
  fFnt.CharSpacing := seCharSpacing.Value;
  fFnt.Unknown     := seLineSpacing.Value;

  if Sender = seAllYOffset then
  for I := 0 to fFnt.CharCount - 1 do
  if fFnt.Letters[I].Width > 0 then
    fFnt.Letters[I].YOffset := seAllYOffset.Value;

  Edit1Change(nil);
end;


procedure TfrmMain.seLetterYChange(Sender: TObject);
begin
  if fUpdating then Exit;
  if fSelectedLetter = 0 then Exit;

  fFnt.Letters[fSelectedLetter].YOffset := seLetterY.Value;
  Edit1Change(nil);
end;


procedure TfrmMain.ScrollBar1Change(Sender: TObject);
var
  offset: Word;
begin
  ShowBigImage(CheckCells.Checked);
  PaintBox1.Repaint;

  offset := ScrollBar1.Position;

  Shape1.Left := PaintBox1.Left + (fSelectedLetter - offset) mod fCols * fCellX - 1;
  Shape1.Top := PaintBox1.Top + (fSelectedLetter - offset) div fCols * fCellY - 1;
end;


initialization
{$IFDEF FPC}
{$I umain.lrs}
{$ENDIF}


end.
