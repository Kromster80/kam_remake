unit KM_FontCollator;
{$I ..\..\KaM_Remake.inc}
interface
uses
  {$IFDEF WDC} Windows, {$ENDIF} //Declared first to get TBitmap overriden with VCL version
  {$IFDEF FPC} lconvencoding, {$ENDIF}
  Classes, StrUtils, SysUtils,
  KM_CommonTypes, KM_Defaults, KM_FileIO,
  KM_ResFontsEdit, KM_ResPalettes;


type
  TKMFontCollator = class
  private
    fFontDir: string;
    fFonts: TStringList;
  public
    constructor Create;
    destructor Destroy; override;

    property Fonts: TStringList read fFonts;
    procedure Collate(aIndex: Integer; aX, aY, aPad: Word; aFiles: TStringArray; var aFont: TKMFontDataEdit);
    function FontCodepages(aIndex: Integer): string;
    procedure ListFonts(aPath: string);
  end;


implementation
uses KM_ResLocales, KM_ResFonts;


{ TKMFontCollator }
constructor TKMFontCollator.Create;
begin
  inherited;

  fFonts := TStringList.Create;

  gResLocales := TKMLocales.Create(ExeDir + '..\..\data\locales.txt', DEFAULT_LOCALE);
end;


destructor TKMFontCollator.Destroy;
begin
  gResLocales.Free;
  fFonts.Free;

  inherited;
end;


procedure TKMFontCollator.Collate(aIndex: Integer; aX, aY, aPad: Word; aFiles: TStringArray; var aFont: TKMFontDataEdit);
var
  I: Integer;
  srcFonts: array of TKMFontDataEdit;
  pals: TKMResPalettes;
  fntPal: TKMPal;
  srcFontFile: string;
begin
  SetLength(srcFonts, Length(aFiles));

  for I := Low(aFiles) to High(aFiles) do
  begin
    srcFonts[I] := TKMFontDataEdit.Create;
    srcFontFile := ExeDir + '..\..\data\gfx\fonts\' + aFiles[I];

    //Guess font palette from filename
    pals := TKMResPalettes.Create;
    pals.LoadPalettes(ExeDir + '..\..\data\gfx\');

    fntPal := TKMResFonts.GuessPalette(srcFontFile);

    if srcFontFile[Length(srcFontFile)] = 'x' then
      srcFonts[I].LoadFontX(srcFontFile)
    else
      srcFonts[I].LoadFont(srcFontFile, pals[fntPal]);
  end;

  aFont := TKMFontDataEdit.Create;
  aFont.TexPadding := aPad;
  aFont.TexSizeX := aX;
  aFont.TexSizeY := aY;
  aFont.CollateFonts(srcFonts);

  for I := Low(aFiles) to High(aFiles) do
    srcFonts[I].Free;
end;


function TKMFontCollator.FontCodepages(aIndex: Integer): string;
var
  SearchRec: TSearchRec;
begin
  Result := '';

  FindFirst(fFontDir + fFonts[aIndex] + '*.fnt?', faAnyFile - faDirectory, SearchRec);
  repeat
    Result := Result + SearchRec.Name + #13#10;
  until (FindNext(SearchRec) <> 0);
  FindClose(SearchRec);
end;


procedure TKMFontCollator.ListFonts(aPath: string);
var
  SearchRec: TSearchRec;
  I: Integer;
  fntName: string;
begin
  fFonts.Clear;
  fFontDir := '';

  if not DirectoryExists(aPath) then Exit;

  fFontDir := aPath;
  FindFirst(fFontDir + '*.fnt?', faAnyFile - faDirectory, SearchRec);
  repeat
    I := Pos('.', SearchRec.Name);

    if I = 0 then Continue;

    fntName := Copy(SearchRec.Name, 1, I-1);

    if fFonts.IndexOf(fntName) = -1 then
      fFonts.Append(fntName);
  until (FindNext(SearchRec) <> 0);

  FindClose(SearchRec);
end;


end.
