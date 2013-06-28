unit Unit_Main;
{$I ..\..\KaM_Remake.inc}
interface
uses
  Windows, //Declared first to get TBitmap overriden with VCL version
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs, ExtCtrls, StdCtrls, Spin, StrUtils,
  KM_Defaults, KM_ResourceFonts, KM_ResourceFontsEdit, KM_ResourcePalettes;


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
    procedure btnGenerateClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure btnExportTexClick(Sender: TObject);
    procedure btnImportTexClick(Sender: TObject);
    procedure btnCollectCharsClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnCollateClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnCollateAllClick(Sender: TObject);
  private
    Pals: TKMPalettes;
    Fnt: TKMFontDataEdit;
  end;


var
  Form1: TForm1;


implementation
{$R *.dfm}
uses KM_Locales;


procedure TForm1.FormCreate(Sender: TObject);
var
  fntId: TKMFont;
begin
  Caption := 'KaM FontX Generator (' + GAME_REVISION + ')';
  ExeDir := ExtractFilePath(Application.ExeName);

  //Palettes
  Pals := TKMPalettes.Create;
  Pals.LoadPalettes(ExeDir + '..\..\data\gfx\');

  fLocales := TKMLocales.Create(ExeDir + '..\..\data\locales.txt');

  //Available fonts
  for fntId := Low(TKMFont) to High(TKMFont) do
    ListBox1.Items.Add(FontInfo[fntId].FontFile);
end;


procedure TForm1.FormDestroy(Sender: TObject);
begin
  FreeAndNil(Fnt);
  FreeAndNil(Pals);
end;


procedure TForm1.btnGenerateClick(Sender: TObject);
var
  useChars: array of Char;
  fntStyle: TFontStyles;
begin
  FreeAndNil(Fnt);
  Fnt := TKMFontDataEdit.Create;

  SetLength(useChars, Length(Memo1.Text));
  Move(Memo1.Text[1], useChars[0], Length(Memo1.Text) * SizeOf(Char));

  fntStyle := [];
  if cbBold.Checked then
    fntStyle := fntStyle + [fsBold];
  if cbItalic.Checked then
    fntStyle := fntStyle + [fsItalic];

  Fnt.TexPadding := sePadding.Value;
  Fnt.TexSizeX := StrToInt(rgSizeX.Items[rgSizeX.ItemIndex]);
  Fnt.TexSizeY := StrToInt(rgSizeY.Items[rgSizeY.ItemIndex]);
  Fnt.CreateFont(edtFontName.Text, seFontSize.Value, fntStyle, useChars);

  Fnt.ExportBimap(Image1.Picture.Bitmap, True);
end;


procedure TForm1.btnSaveClick(Sender: TObject);
begin
  dlgSave.DefaultExt := 'fntx';
  dlgSave.InitialDir := ExeDir + '..\..\data\gfx\fonts\';
  if not dlgSave.Execute then Exit;

  Fnt.SaveToFontX(dlgSave.FileName);
  //Fnt.SaveToFontX(ExeDir + '..\..\data\gfx\fonts\arialuni.fntx');
end;


procedure TForm1.btnCollateClick(Sender: TObject);
const
  CODE_PAGES: array [0..4] of Word = (1250, 1251, 1252, 1254, 1257);
var
  fntId: TKMFont;
  srcFont: array [0..4] of TKMFontDataEdit;
  I: Integer;
begin
  if ListBox1.ItemIndex = -1 then Exit;

  fntId := TKMFont(ListBox1.ItemIndex);

  for I := 0 to 4 do
  begin
    srcFont[I] := TKMFontDataEdit.Create;
    srcFont[I].LoadFont(ExeDir + '..\..\data\gfx\fonts\' + FontInfo[fntId].FontFile + '.' + IntToStr(CODE_PAGES[I]) + '.fnt', Pals[FontInfo[fntId].Pal]);
  end;

  FreeAndNil(Fnt);
  Fnt := TKMFontDataEdit.Create;
  Fnt.TexPadding := sePadding.Value;
  Fnt.TexSizeX := StrToInt(rgSizeX.Items[rgSizeX.ItemIndex]);
  Fnt.TexSizeY := StrToInt(rgSizeY.Items[rgSizeY.ItemIndex]);
  Fnt.CollateFont(srcFont, CODE_PAGES);

  for I := 0 to 4 do
    srcFont[I].Free;

  Fnt.ExportBimap(Image1.Picture.Bitmap, False);
end;


procedure TForm1.btnCollateAllClick(Sender: TObject);
var
  I: TKMFont;
begin
  //256x512 - enough for any fonst yet
  rgSizeX.ItemIndex := 1;
  rgSizeY.ItemIndex := 2;

  for I := Low(TKMFont) to High(TKMFont) do
  begin
    ListBox1.ItemIndex := Byte(I);
    btnCollateClick(Self);
    btnSaveClick(Self);
  end;
end;


procedure TForm1.btnCollectCharsClick(Sender: TObject);
  procedure GetAllTextPaths(aExeDir: string; aList: TStringList);
  var
    I: Integer;
    SearchRec: TSearchRec;
    PathToMaps: TStringList;
  begin
    aList.Clear;

    PathToMaps := TStringList.Create;
    try
      PathToMaps.Add(aExeDir + 'Maps' + PathDelim);
      PathToMaps.Add(aExeDir + 'MapsMP' + PathDelim);
      PathToMaps.Add(aExeDir + 'Tutorials' + PathDelim);
      PathToMaps.Add(aExeDir + 'data' + PathDelim + 'text' + PathDelim);

      //Include all campaigns maps
      FindFirst(aExeDir + 'Campaigns' + PathDelim + '*', faDirectory, SearchRec);
      repeat
        if (SearchRec.Name <> '.') and (SearchRec.Name <> '..') then
          PathToMaps.Add(aExeDir + 'Campaigns' + PathDelim + SearchRec.Name + PathDelim);
      until (FindNext(SearchRec) <> 0);
      FindClose(SearchRec);

      //Use reversed loop because we want to append paths to the same list
      for I := PathToMaps.Count - 1 downto 0 do
      if DirectoryExists(PathToMaps[I]) then
      begin
        FindFirst(PathToMaps[I] + '*', faDirectory, SearchRec);
        repeat
          if (SearchRec.Name <> '.') and (SearchRec.Name <> '..') then
            PathToMaps.Add(PathToMaps[I] + SearchRec.Name + PathDelim);
        until (FindNext(SearchRec) <> 0);
        FindClose(SearchRec);
      end;

      //Collect all libx files
      for I := 0 to PathToMaps.Count - 1 do
      if DirectoryExists(PathToMaps[I]) then
      begin
        FindFirst(PathToMaps[I] + '*.libx', faAnyFile - faDirectory, SearchRec);
        repeat
          if RightStr(SearchRec.Name, 4) = 'libx' then
            aList.Add(PathToMaps[I] + SearchRec.Name);
        until (FindNext(SearchRec) <> 0);
        FindClose(SearchRec);
      end;
    finally
      PathToMaps.Free;
    end;
  end;
var
  libxList: TStringList;
  lang: string;
  libx: TStringList;//Stream;
  chars: array [0..High(Word)] of Char;
  I, K: Integer;
  libTxt, uniText: string;
  lab: string;
begin
  //Collect list of library files
  libxList := TStringList.Create;
  libx := TStringList.Create;
  lab := btnCollectChars.Caption;
  try
    GetAllTextPaths(ExeDir + '..\..\', libxList);

    //libxList.Append(ExtractFilePath(Application.ExeName) + 'ger.libx');
    //libxList.Append(ExtractFilePath(Application.ExeName) + 'uni.txt');

    for I := 0 to libxList.Count - 1 do
    if FileExists(libxList[I]) then
    begin
      btnCollectChars.Caption := IntToStr(I) + '/' + IntToStr(libxList.Count);

      lang := Copy(libxList[I], Length(libxList[I]) - 7, 3);
      libx.DefaultEncoding := TEncoding.GetEncoding(fLocales.GetLocale(lang).FontCodepage);
      libx.LoadFromFile(libxList[I]);

      libTxt := libx.Text;
      for K := 0 to Length(libTxt) - 1 do
      //if Word(libTxt[K+1]) = $9A then
        chars[Word(libTxt[K+1])] := libTxt[K+1];
    end;

    chars[10] := #0; //End of line chars are not needed
    chars[13] := #0; //End of line chars are not needed
    chars[32] := #0; // space symbol, KaM uses word spacing property instead
    chars[124] := #0; // | symbol, end of line in KaM

    for I := 0 to High(Word) do
    if chars[I] <> #0 then
      uniText := uniText + chars[I];

    Memo1.Text := uniText;
  finally
    libxList.Free;
    libx.Free;
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
