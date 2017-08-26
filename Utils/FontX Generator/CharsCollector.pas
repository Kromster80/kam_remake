unit CharsCollector;
{$I ..\..\KaM_Remake.inc}
interface
uses
  {$IFDEF WDC} Windows, {$ENDIF} //Declared first to get TBitmap overriden with VCL version
  {$IFDEF FPC} lconvencoding, {$ENDIF}
  Classes, StrUtils, SysUtils,
  KM_CommonTypes, KM_Defaults, KM_FileIO,
  KM_ResFontsEdit, KM_ResPalettes;


  function CollectChars(aProgress: TUnicodeStringEvent): UnicodeString;


implementation
uses KM_ResLocales, KM_ResFonts;


function CollectChars(aProgress: TUnicodeStringEvent): UnicodeString;
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
      PathToMaps.Add(aExeDir + 'TextSamples' + PathDelim);
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
  chars: array [0..High(Word)] of WideChar;
  I, K: Integer;
  libTxt: UnicodeString;
begin
  //Collect list of library files
  libxList := TStringList.Create;

  FillChar(chars, SizeOf(chars), #0);
  gResLocales := TKMLocales.Create(ExeDir + '..\..\data\locales.txt', DEFAULT_LOCALE);
  try
    GetAllTextPaths(ExeDir + '..\..\', libxList);

    //libxList.Append(ExtractFilePath(ParamStr(0)) + 'ger.libx');
    //libxList.Append(ExtractFilePath(ParamStr(0)) + 'uni.txt');

    for I := 0 to libxList.Count - 1 do
    if FileExists(libxList[I]) then
    begin
      aProgress(IntToStr(I) + '/' + IntToStr(libxList.Count));

      //Load ANSI file with codepage we say into unicode string
      langCode := Copy(libxList[I], Length(libxList[I]) - 7, 3);
      libTxt := ReadTextU(libxList[I], gResLocales.LocaleByCode(langCode).FontCodepage);

      for K := 0 to Length(libTxt) - 1 do
        chars[Ord(libTxt[K+1])] := #1;
    end;

    chars[10] := #0; //End of line chars are not needed
    chars[13] := #0; //End of line chars are not needed
    chars[32] := #0; // space symbol, KaM uses word spacing property instead
    chars[124] := #0; // | symbol, end of line in KaM

    Result := '';
    for I := 0 to High(Word) do
    if chars[I] <> #0 then
      Result := Result + WideChar(I);
  finally
    libxList.Free;
    gResLocales.Free;
  end;
end;


end.
