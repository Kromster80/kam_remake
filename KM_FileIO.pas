unit KM_FileIO;
{$I KaM_Remake.inc}
interface
uses
  {$IFDEF FPC} lconvencoding, {$ENDIF}
  Classes, SysUtils;

//Read text file into ANSI string (scripts, locale texts)
function ReadTextA(afilename: UnicodeString): AnsiString;

//Read text file into unicode string (locale texts)
function ReadTextU(afilename: UnicodeString; aEncoding: Word): UnicodeString;


implementation


function ReadTextA(aFilename: UnicodeString): AnsiString;
var
  MS: TMemoryStream;
  Head: Cardinal;
begin
  MS := TMemoryStream.Create;
  try
    //We can't rely on StringList because it applies default codepage encoding,
    //which may differ between MP players.
    //Instead we read plain ansi text. If there's BOM - clip it
    MS.LoadFromFile(aFileName);

    MS.Read(Head, 4);

    //Trim UTF8 BOM (don't know how to deal with others yet)
    if (Head xor $EFBBBF = 0) then
      MS.Position := 3
    else
      MS.Position := 0;

    SetLength(Result, MS.Size - MS.Position);
    MS.Read(Result[1], MS.Size - MS.Position);
  finally
    MS.Free;
  end;
end;


//Load ANSI file with codepage we say into unicode string
function ReadTextU(aFilename: UnicodeString; aEncoding: Word): UnicodeString;
var
  SL: TStringList;
begin
  SL := TStringList.Create;
  try
    {$IFDEF WDC}
      //Load the text file with default ANSI encoding. If file has embedded BOM it will be used
      SL.DefaultEncoding := TEncoding.GetEncoding(aEncoding);
      SL.LoadFromFile(aFilename);
      Result := SL.Text;
    {$ENDIF}
    {$IFDEF FPC}
      SL.LoadFromFile(aFilename);
      Result := UTF8Decode(ConvertEncoding(SL.Text, 'cp' + IntToStr(aEncoding), EncodingUTF8));
    {$ENDIF}
  finally
    SL.Free;
  end;
end;


end.
