unit Unit_Main;
{$I ..\..\KaM_Remake.inc}
interface
uses
  Windows, //Declared first to get TBitmap overriden with VCL version
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs, ExtCtrls, StdCtrls, Spin, StrUtils,
  KM_ResourceFonts;


type
  TForm1 = class(TForm)
    btnGenerate: TButton;
    Memo1: TMemo;
    Label1: TLabel;
    edtFontName: TEdit;
    Label2: TLabel;
    seFontSize: TSpinEdit;
    Label3: TLabel;
    Label4: TLabel;
    Image1: TImage;
    btnSave: TButton;
    cbBold: TCheckBox;
    cbItalic: TCheckBox;
    dlgSave: TSaveDialog;
    sePadding: TSpinEdit;
    Label5: TLabel;
    btnExportTex: TButton;
    dlgOpen: TOpenDialog;
    btnImportTex: TButton;
    btnCollectChars: TButton;
    SpinEdit1: TSpinEdit;
    Label6: TLabel;
    procedure btnGenerateClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure btnExportTexClick(Sender: TObject);
    procedure btnImportTexClick(Sender: TObject);
    procedure btnCollectCharsClick(Sender: TObject);
  private
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
  Fnt.TexSize := SpinEdit1.Value;
  Fnt.CreateFont(edtFontName.Text, seFontSize.Value, fntStyle, useChars);

  Bmp := TBitmap.Create;
  Fnt.ExportBimap(Image1.Picture.Bitmap, True);
  Bmp.Free;
end;


procedure TForm1.btnSaveClick(Sender: TObject);
begin
  dlgSave.DefaultExt := 'fntx';
  if not dlgSave.Execute then Exit;

  Fnt.SaveToFontX(dlgSave.FileName);
end;


procedure TForm1.btnCollectCharsClick(Sender: TObject);
  function GetCodepage(aLang: string): Word;
  begin
    //Using slower but more compact comparisons
    if Pos(aLang, 'bel,rus,bul,ukr') <> 0 then
      Result := 28595 //ISO 8859-5 (Cyrillic)
    else if Pos(aLang, 'pol,hun,cze,svk,rom') <> 0 then
      Result := 28592 //ISO 8859-2 (Central European)
    else if Pos(aLang, 'tur') <> 0 then
      Result := 28599 //ISO 8859-9 (Turkish)
    else if Pos(aLang, 'lit,lat') <> 0 then
      Result := 28594 //ISO 8859-4 (Baltic)
    else if Pos(aLang, 'eng,ger,spa,ita,nor,chn,dut,est,ptb,fre,jpn,swe') <> 0 then
      Result := 28591  //ISO 8859-1 (Latin 1)
    else
      Result := 28591;
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
    GetAllTextPaths(ExtractFilePath(Application.ExeName) + '..\..\', libxList);

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

    //End of line chars are not needed
    chars[10] := #0;
    chars[13] := #0;

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
