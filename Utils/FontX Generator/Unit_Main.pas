unit Unit_Main;
{$I ..\..\KaM_Remake.inc}
interface
uses
  Windows, //Declared first to get TBitmap overriden with VCL version
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs, ExtCtrls, StdCtrls, Spin, StrUtils,
  KM_Defaults, KM_ResourceFonts, KM_ResourcePalettes;


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
    SpinEdit1: TSpinEdit;
    Label6: TLabel;
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
    procedure btnGenerateClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure btnExportTexClick(Sender: TObject);
    procedure btnImportTexClick(Sender: TObject);
    procedure btnCollectCharsClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnCollateClick(Sender: TObject);
  private
    Pals: TKMPalettes;
    Fnt: TKMFontData;
  end;


var
  Form1: TForm1;


implementation
{$R *.dfm}


procedure TForm1.btnGenerateClick(Sender: TObject);
var
  useChars: array of Char;
  fntStyle: TFontStyles;
  Bmp: TBitmap;
begin
  Fnt := TKMFontData.Create;

  SetLength(useChars, Length(Memo1.Text));
  Move(Memo1.Text[1], useChars[0], Length(Memo1.Text) * SizeOf(Char));

  fntStyle := [];
  if cbBold.Checked then
    fntStyle := fntStyle + [fsBold];
  if cbItalic.Checked then
    fntStyle := fntStyle + [fsItalic];

  Fnt.TexPadding := sePadding.Value;
  Fnt.TexSizeX := SpinEdit1.Value;
  Fnt.TexSizeY := SpinEdit1.Value;
  Fnt.CreateFont(edtFontName.Text, seFontSize.Value, fntStyle, useChars);

  Bmp := TBitmap.Create;
  Fnt.ExportBimap(Image1.Picture.Bitmap, True);
  Bmp.Free;
end;


procedure TForm1.btnSaveClick(Sender: TObject);
begin
  dlgSave.DefaultExt := 'fntx';
  //if not dlgSave.Execute then Exit;

  //Fnt.SaveToFontX(dlgSave.FileName);
  Fnt.SaveToFontX(ExeDir + '..\..\data\gfx\fonts\arialuni.fntx');
end;


procedure TForm1.FormCreate(Sender: TObject);
var
  fntId: TKMFont;
begin
  Caption := 'KaM Font Editor (' + GAME_REVISION + ')';
  ExeDir := ExtractFilePath(Application.ExeName);

  //Palettes
  Pals := TKMPalettes.Create;
  Pals.LoadPalettes(ExeDir + '..\..\data\gfx\');

  //Available fonts
  for fntId := Low(TKMFont) to High(TKMFont) do
    ListBox1.Items.Add(FontInfo[fntId].FontFile);
end;


procedure TForm1.btnCollateClick(Sender: TObject);
const
  CODE_PAGES: array [0..3] of Word = (1250, 1251, 1254, 1257);
var
  fntId: TKMFont;
  srcFont: array [0..3] of TKMFontData;
  newFont: TKMFontData;
  I: Integer;
begin
  if ListBox1.ItemIndex = -1 then Exit;

  fntId := TKMFont(ListBox1.ItemIndex);

  for I in CODE_PAGES do
  begin
    srcFont[I] := TKMFontData.Create;
    srcFont[I].LoadFont(ExeDir + '..\..\data\gfx\fonts\' + FontInfo[fntId].FontFile + '.' + IntToStr(CODE_PAGES[I]) + '.fnt', FontInfo[fntId].Pal);
  end;

  newFont := TKMFontData.Create;
  newFont.CollateFont(srcFont);

  for I in CODE_PAGES do
    srcFont[I].Free;
end;


procedure TForm1.btnCollectCharsClick(Sender: TObject);
  function GetCodepage(aLang: string): Word;
  begin
    //Using slower but more compact comparisons
    if Pos(aLang, 'bel,rus,bul,ukr') <> 0 then
      Result := 1251
    else if Pos(aLang, 'pol,hun,cze,svk,rom') <> 0 then
      Result := 1250
    else if Pos(aLang, 'tur') <> 0 then
      Result := 1254
    else if Pos(aLang, 'lit,lat') <> 0 then
      Result := 1257
    else if Pos(aLang, 'eng,spa,ita,nor,chn,dut,est,ptb,fre,ger,jpn,swe') <> 0 then
      Result := 1252
    else
      Result := 1252;
  end;

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
  libx := TStringList.Create();//'', TEncoding.UTF8);
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
      libx.DefaultEncoding := TEncoding.GetEncoding(GetCodepage(lang));
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
