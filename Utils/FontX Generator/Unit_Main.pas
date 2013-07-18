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
    GroupBox2: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    btnGenerate: TButton;
    Memo1: TMemo;
    edtFontName: TEdit;
    seFontSize: TSpinEdit;
    cbBold: TCheckBox;
    cbItalic: TCheckBox;
    btnCollectChars: TButton;
    GroupBox3: TGroupBox;
    ListBox1: TListBox;
    btnCollate: TButton;
    rgSizeX: TRadioGroup;
    rgSizeY: TRadioGroup;
    btnCollateAuto: TButton;
    cbCells: TCheckBox;
    btnOneClick: TButton;
    procedure btnGenerateClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure btnExportTexClick(Sender: TObject);
    procedure btnImportTexClick(Sender: TObject);
    procedure btnCollectCharsClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnCollateClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnCollateAllClick(Sender: TObject);
    procedure btnOneClickClick(Sender: TObject);
    procedure UpdateCaption(const aString: UnicodeString);
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
  Caption := 'KaM FontX Generator (' + GAME_REVISION + ')';
  ExeDir := ExtractFilePath(Application.ExeName);

  Collator := TKMFontCollator.Create;


  //Available fonts
  for fntId := Low(TKMFont) to High(TKMFont) do
    ListBox1.Items.Add(FontInfo[fntId].FontFile);
end;


procedure TForm1.FormDestroy(Sender: TObject);
begin
  FreeAndNil(Collator);
  FreeAndNil(Fnt);
end;


procedure TForm1.btnGenerateClick(Sender: TObject);
var
  chars: UnicodeString;
  useChars: array of WideChar;
  fntStyle: TFontStyles;
begin
  FreeAndNil(Fnt);
  Fnt := TKMFontDataEdit.Create;

  {$IFDEF WDC}
  chars := Memo1.Text;
  {$ENDIF}
  {$IFDEF FPC}
  chars := UTF8Decode(Memo1.Text);
  {$ENDIF}
  SetLength(useChars, Length(chars));
  Move(chars[1], useChars[0], Length(chars) * SizeOf(WideChar));

  fntStyle := [];
  if cbBold.Checked then
    fntStyle := fntStyle + [fsBold];
  if cbItalic.Checked then
    fntStyle := fntStyle + [fsItalic];

  Fnt.TexPadding := sePadding.Value;
  Fnt.TexSizeX := StrToInt(rgSizeX.Items[rgSizeX.ItemIndex]);
  Fnt.TexSizeY := StrToInt(rgSizeY.Items[rgSizeY.ItemIndex]);
  Fnt.CreateFont(edtFontName.Text, seFontSize.Value, fntStyle, useChars);

  Fnt.ExportBimap(Image1.Picture.Bitmap, True, cbCells.Checked);
end;


procedure TForm1.btnSaveClick(Sender: TObject);
begin
  dlgSave.DefaultExt := 'fntx';
  dlgSave.FileName := ListBox1.Items[ListBox1.ItemIndex];
  dlgSave.InitialDir := ExpandFileName(ExeDir + '..\..\data\gfx\fonts\');
  if not dlgSave.Execute then Exit;

  Fnt.SaveToFontX(dlgSave.FileName);
end;


procedure TForm1.btnOneClickClick(Sender: TObject);
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


procedure TForm1.UpdateCaption(const aString: UnicodeString);
begin
  btnCollectChars.Caption := aString;
end;


procedure TForm1.btnCollectCharsClick(Sender: TObject);
var
  lab: string;
  uniText: UnicodeString;
begin
  lab := btnCollectChars.Caption;
  try
    uniText := Collator.CollectChars(UpdateCaption);

    {$IFDEF WDC}
      Memo1.Text := uniText;
    {$ENDIF}
    {$IFDEF FPC}
      //FPC controls need utf8 strings
      Memo1.Text := UTF8Encode(uniText);
    {$ENDIF}
  finally
    btnCollectChars.Caption := lab;
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
end;


end.
