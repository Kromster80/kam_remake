unit Unit_Main;
{$I ..\..\KaM_Remake.inc}
interface
uses
  {$IFDEF WDC} Windows, {$ENDIF} //Declared first to get TBitmap overriden with VCL version
  {$IFDEF FPC} lconvencoding, {$ENDIF}
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs, ExtCtrls, StdCtrls, Spin, StrUtils,
  KM_CommonTypes, KM_Defaults, KM_ResourceFonts, KM_ResourceFontsEdit, KM_ResourcePalettes;


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

  fLocales := TKMLocales.Create(ExeDir + '..\..\data\locales.txt');

  //Available fonts
  for fntId := Low(TKMFont) to High(TKMFont) do
    ListBox1.Items.Add(FontInfo[fntId].FontFile);
end;


procedure TForm1.FormDestroy(Sender: TObject);
begin
  FreeAndNil(Fnt);
end;


procedure TForm1.btnGenerateClick(Sender: TObject);
var
  chars: WideString;
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


procedure TForm1.btnCollateClick(Sender: TObject);
var
  pals: TKMPalettes;
  codePages: TKMWordArray;
  fntId: TKMFont;
  I: Integer;
  srcFont: array of TKMFontDataEdit;
  fntFile: string;
begin
  if ListBox1.ItemIndex = -1 then Exit;

  fntId := TKMFont(ListBox1.ItemIndex);

  //We need palettes to properly load FNT files
  pals := TKMPalettes.Create;
  try
    pals.LoadPalettes(ExeDir + '..\..\data\gfx\');

    //List of codepages we need to collate
    codePages := fLocales.CodePagesList;
    SetLength(srcFont, Length(codePages));

    for I := Low(codePages) to High(codePages) do
    begin
      srcFont[I] := TKMFontDataEdit.Create;
      fntFile := ExeDir + '..\..\data\gfx\fonts\' + FontInfo[fntId].FontFile + '.' + IntToStr(codePages[I]) + '.fnt';
      srcFont[I].LoadFont(fntFile, pals[FontInfo[fntId].Pal]);
    end;

    //Recreate clean Font
    FreeAndNil(Fnt);
    Fnt := TKMFontDataEdit.Create;

    Fnt.TexPadding := sePadding.Value;
    Fnt.TexSizeX := StrToInt(rgSizeX.Items[rgSizeX.ItemIndex]);
    Fnt.TexSizeY := StrToInt(rgSizeY.Items[rgSizeY.ItemIndex]);
    Fnt.CollateFont(srcFont, codePages);

    for I := Low(codePages) to High(codePages) do
      srcFont[I].Free;
  finally
    pals.Free;
  end;

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
  langCode: string;
  libx: TStringList;
  chars: array [0..High(Word)] of WideChar;
  I, K: Integer;
  libTxt: WideString;
  uniText: WideString;
  lab: string;
begin
  //Collect list of library files
  libxList := TStringList.Create;
  libx := TStringList.Create;
  lab := btnCollectChars.Caption;

  FillChar(chars, SizeOf(chars), #0);
  try
    GetAllTextPaths(ExeDir + '..\..\', libxList);

    //libxList.Append(ExtractFilePath(Application.ExeName) + 'ger.libx');
    //libxList.Append(ExtractFilePath(Application.ExeName) + 'uni.txt');

    for I := 0 to libxList.Count - 1 do
    if FileExists(libxList[I]) then
    begin
      btnCollectChars.Caption := IntToStr(I) + '/' + IntToStr(libxList.Count);

      //Load ANSI file with codepage we say into unicode string
      langCode := Copy(libxList[I], Length(libxList[I]) - 7, 3);
      {$IFDEF WDC}
        //Load the text file with default ANSI encoding. If file has embedded BOM it will be used
        libx.DefaultEncoding := TEncoding.GetEncoding(fLocales.GetLocale(langCode).FontCodepage);
        libx.LoadFromFile(libxList[I]);
        libTxt := libx.Text;
      {$ENDIF}
      {$IFDEF FPC}
        libx.LoadFromFile(libxList[I]);
        libTxt := UTF8Decode(ConvertEncoding(libx.Text, 'cp' + IntToStr(fLocales.GetLocale(langCode).FontCodepage), EncodingUTF8));
      {$ENDIF}

      for K := 0 to Length(libTxt) - 1 do
        chars[Ord(libTxt[K+1])] := #1;
    end;

    chars[10] := #0; //End of line chars are not needed
    chars[13] := #0; //End of line chars are not needed
    chars[32] := #0; // space symbol, KaM uses word spacing property instead
    chars[124] := #0; // | symbol, end of line in KaM

    uniText := '';
    for I := 0 to High(Word) do
    if chars[I] <> #0 then
      uniText := uniText + WideChar(I);

    {$IFDEF WDC}
      Memo1.Text := uniText;
    {$ENDIF}
    {$IFDEF FPC}
      //FPC controls need utf8 strings
      Memo1.Text := UTF8Encode(uniText);
    {$ENDIF}
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
