unit KM_FileIO;
interface
uses Classes, SysUtils;


//Read text file into ANSI string (scripts, locale texts)
function ReadTextA(afilename: UnicodeString): AnsiString;

//Read text file into unicode string (locale texts)
function ReadTextU(afilename: UnicodeString; aEncoding: Word): UnicodeString;


implementation


function ReadTextA(afilename: UnicodeString): AnsiString;
begin
  Result := '';
end;


//Load ANSI file with codepage we say into unicode string
function ReadTextU(afilename: UnicodeString; aEncoding: Word): UnicodeString;
var
  SL: TStringList;
begin
  SL := TStringList.Create;
  try
    {$IFDEF WDC}
      //Load the text file with default ANSI encoding. If file has embedded BOM it will be used
      SL.DefaultEncoding := TEncoding.GetEncoding(aEncoding);
      SL.LoadFromFile(FilePath);
      Result := SL.Text;
    {$ENDIF}
    {$IFDEF FPC}
      SL.LoadFromFile(FilePath);
      Result := UTF8Decode(ConvertEncoding(SL.Text, 'cp' + IntToStr(aEncoding), EncodingUTF8));
    {$ENDIF}
  finally
    SL.Free;
  end;
end;


end.
