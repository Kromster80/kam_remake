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
  public
    constructor Create;
    destructor Destroy; override;

    procedure Collate(aFontName: string; aPal: TKMPal; aPad, aX, aY: Word; aFnt: TKMFontDataEdit);
  end;


implementation
uses KM_ResLocales, KM_ResFonts;


{ TKMFontCollator }
constructor TKMFontCollator.Create;
begin
  inherited;

  gResLocales := TKMLocales.Create(ExeDir + '..\..\data\locales.txt', DEFAULT_LOCALE);
end;


destructor TKMFontCollator.Destroy;
begin
  gResLocales.Free;

  inherited;
end;


procedure TKMFontCollator.Collate(aFontName: string; aPal: TKMPal; aPad, aX, aY: Word; aFnt: TKMFontDataEdit);
var
  pals: TKMPalettes;
  codePages: TKMWordArray;
  I: Integer;
  srcFont: array of TKMFontDataEdit;
  fntFile: string;
begin
  //We need palettes to properly load FNT files
  pals := TKMPalettes.Create;
  try
    pals.LoadPalettes(ExeDir + '..\..\data\gfx\');

    //List of codepages we need to collate
    codePages := gResLocales.CodePagesList;

    SetLength(codePages, Length(codePages)+1);
    codePages[High(codePages)] := 9999;
    SetLength(srcFont, Length(codePages));

    for I := Low(codePages) to High(codePages) do
    begin
      srcFont[I] := TKMFontDataEdit.Create;
      fntFile := ExeDir + '..\..\data\gfx\fonts\' + aFontName + '.' + IntToStr(codePages[I]) + '.fnt';
      srcFont[I].LoadFont(fntFile, pals[aPal]);
    end;

    aFnt.TexPadding := aPad;
    aFnt.TexSizeX := aX;
    aFnt.TexSizeY := aY;
    aFnt.CollateFont(srcFont, codePages);

    for I := Low(codePages) to High(codePages) do
      srcFont[I].Free;
  finally
    pals.Free;
  end;
end;


end.
