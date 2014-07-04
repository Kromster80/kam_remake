unit KM_FileIO;
{$I KaM_Remake.inc}
interface
uses
  {$IFDEF FPC} lconvencoding, FileUtil, LazUTF8, {$ENDIF}
  {$IFDEF WDC} System.IOUtils, {$ENDIF}
  Classes, SysUtils;

//Read text file into ANSI string (scripts, locale texts)
function ReadTextA(afilename: UnicodeString): AnsiString;

//Read text file into unicode string (locale texts)
function ReadTextU(afilename: UnicodeString; aEncoding: Word): UnicodeString;

//Copy a file (CopyFile is different between Delphi and Lazarus)
procedure KMCopyFile(aSrc, aDest: UnicodeString);


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
    if Head and $FFFFFF = $BFBBEF then
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
  {$IFDEF WDC}
    SL: TStringList;
  {$ENDIF}
  {$IFDEF FPC}
    MS: TMemoryStream;
    Head: Cardinal;
    HasBOM: Boolean;
    TmpA: AnsiString;
  {$ENDIF}
begin
  {$IFDEF WDC}
    SL := TStringList.Create;
    try
      //Load the text file with default ANSI encoding. If file has embedded BOM it will be used
      SL.DefaultEncoding := TEncoding.GetEncoding(aEncoding);
      SL.LoadFromFile(aFilename);
      Result := SL.Text;
    finally
      SL.Free;
    end;
  {$ENDIF}
  {$IFDEF FPC}
    MS := TMemoryStream.Create;
    try
      MS.LoadFromFile(aFileName);
      MS.Read(Head, 4);

      //Trim UTF8 BOM (don't know how to deal with others yet)
      HasBOM := Head and $FFFFFF = $BFBBEF;

      if HasBOM then
        MS.Position := 3
      else
        MS.Position := 0;

      SetLength(TmpA, MS.Size - MS.Position);
      MS.Read(TmpA[1], MS.Size - MS.Position);

      //Non-UTF8 files must be converted from their native encoding
      if not HasBOM then
        TmpA := ConvertEncoding(TmpA, 'cp' + IntToStr(aEncoding), EncodingUTF8);

      Result := UTF8ToUTF16(TmpA);
    finally
      MS.Free;
    end;
  {$ENDIF}
end;


procedure KMCopyFile(aSrc, aDest: UnicodeString);
begin
  {$IFDEF FPC}
  CopyFile(aSrc, aDest);
  {$ENDIF}
  {$IFDEF WDC}
  TFile.Copy(aSrc, aDest);
  {$ENDIF}
end;


end.
