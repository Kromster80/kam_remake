unit Unit_Main;
{$I ..\..\KaM_Remake.inc}
interface
uses
  {$IFDEF WDC} Windows, {$ENDIF} //Declared first to get TBitmap overriden with VCL version
  {$IFDEF FPC} lconvencoding, {$ENDIF}
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs, ExtCtrls, StdCtrls, Spin, StrUtils,
  KM_CommonTypes, KM_Defaults, KM_FontCollator, KM_ResFonts, KM_ResFontsEdit, KM_ResPalettes;


type
  TForm1 = class(TForm)
    Label4: TLabel;
    Image1: TImage;
    btnSave: TButton;
    dlgSave: TSaveDialog;
    btnExportTex: TButton;
    dlgOpen: TOpenDialog;
    btnImportTex: TButton;
    GroupBox1: TGroupBox;
    sePadding: TSpinEdit;
    Label5: TLabel;
    GroupBox3: TGroupBox;
    ListBox1: TListBox;
    btnCollate: TButton;
    rgSizeX: TRadioGroup;
    rgSizeY: TRadioGroup;
    btnCollateAuto: TButton;
    cbCells: TCheckBox;
    procedure btnSaveClick(Sender: TObject);
    procedure btnExportTexClick(Sender: TObject);
    procedure btnImportTexClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnCollateClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnCollateAllClick(Sender: TObject);
  private
    Fnt: TKMFontDataEdit;
    Collator: TKMFontCollator;
  end;


var
  Form1: TForm1;


implementation
{$R *.dfm}


procedure TForm1.FormCreate(Sender: TObject);
var
  fntId: TKMFont;
begin
  Caption := 'KaM FontX Collator (' + GAME_REVISION + ')';
  ExeDir := ExtractFilePath(Application.ExeName);

  Collator := TKMFontCollator.Create;

  //Scan fonts folder

  //Available fonts
  for fntId := Low(TKMFont) to High(TKMFont) do
    ListBox1.Items.Add(FontInfo[fntId].FontFile);
end;


procedure TForm1.FormDestroy(Sender: TObject);
begin
  FreeAndNil(Collator);
  FreeAndNil(Fnt);
end;


procedure TForm1.btnSaveClick(Sender: TObject);
begin
  dlgSave.DefaultExt := 'fntx';
  dlgSave.FileName := ListBox1.Items[ListBox1.ItemIndex];
  dlgSave.InitialDir := ExpandFileName(ExeDir + '..\..\data\gfx\fonts\');
  if not dlgSave.Execute then Exit;

  Fnt.SaveToFontX(dlgSave.FileName);
end;


procedure TForm1.btnCollateClick(Sender: TObject);
var
  fntId: TKMFont;
begin
  if ListBox1.ItemIndex = -1 then Exit;

  fntId := TKMFont(ListBox1.ItemIndex);

  //Recreate clean Font
  FreeAndNil(Fnt);
  Fnt := TKMFontDataEdit.Create;

  Collator.Collate(FontInfo[fntId].FontFile, FontInfo[fntId].Pal, sePadding.Value,
                   StrToInt(rgSizeX.Items[rgSizeX.ItemIndex]),
                   StrToInt(rgSizeY.Items[rgSizeY.ItemIndex]),
                   Fnt);

  Fnt.ExportBimap(Image1.Picture.Bitmap, False, cbCells.Checked);
end;


procedure TForm1.btnCollateAllClick(Sender: TObject);
var
  I: TKMFont;
begin
  //256x512 - enough for any fonts yet
  rgSizeX.ItemIndex := 1;
  rgSizeY.ItemIndex := 2;

  for I := Low(TKMFont) to High(TKMFont) do
  begin
    ListBox1.ItemIndex := Byte(I);
    btnCollateClick(Self);
    btnSaveClick(Self);
  end;
end;


procedure TForm1.btnExportTexClick(Sender: TObject);
begin
  dlgSave.DefaultExt := 'png';
  if not dlgSave.Execute then Exit;

  Fnt.ExportPng(dlgSave.FileName);
end;


procedure TForm1.btnImportTexClick(Sender: TObject);
begin
  if not dlgOpen.Execute then Exit;

  Fnt.ImportPng(dlgOpen.FileName);
  Fnt.ExportBimap(Image1.Picture.Bitmap, False, cbCells.Checked);
end;


end.
