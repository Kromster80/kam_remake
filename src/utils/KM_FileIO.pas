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

  //Delete a folder (DeleteFolder is different between Delphi and Lazarus)
  procedure KMDeleteFolder(aPath: UnicodeString);

  //Rename a file (RenameFile is different between Delphi and Lazarus)
  procedure KMRenamePath(aSourcePath, aDestPath: UnicodeString);

  //Move a folder and rename all the files inside it (MoveFolder is different between Delphi and Lazarus)
  function KMMoveFolder(aSourceFolder, aDestFolder: UnicodeString): Boolean;


  function IsFilePath(aPath: UnicodeString): Boolean;


implementation
uses
  StrUtils, KM_CommonUtils;


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
    if MS.Size - MS.Position > 0 then
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
    DefaultEncoding: TEncoding;
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
    DefaultEncoding := TEncoding.GetEncoding(aEncoding);
    try
      //Load the text file with default ANSI encoding. If file has embedded BOM it will be used
      SL.DefaultEncoding := DefaultEncoding;
      SL.LoadFromFile(aFilename);
      Result := SL.Text;
    finally
      SL.Free;
      DefaultEncoding.Free;
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


procedure KMDeleteFolder(aPath: UnicodeString);
begin
  if DirectoryExists(aPath) then
    {$IFDEF FPC} DeleteDirectory(aPath, False); {$ENDIF}
    {$IFDEF WDC} TDirectory.Delete(aPath, True); {$ENDIF}
end;


function IsFilePath(aPath: UnicodeString): Boolean;
begin
  //For now we assume, that folder path always ends with PathDelim
  Result := RightStr(aPath, 1) <> PathDelim;
end;


procedure KMRenamePath(aSourcePath, aDestPath: UnicodeString);
begin
  if IsFilePath(aSourcePath) then
  begin
    if FileExists(aSourcePath) then
      {$IFDEF FPC} RenameFile(aSourcePath, aDestPath); {$ENDIF}
      {$IFDEF WDC} TFile.Move(aSourcePath, aDestPath); {$ENDIF}
  end
  else
  if DirectoryExists(aSourcePath) then
  begin
    if DirectoryExists(aDestPath) then
      KMDeleteFolder(aDestPath);
    {$IFDEF FPC} RenameFile(aSourcePath, aDestPath); {$ENDIF}
    {$IFDEF WDC} TDirectory.Move(aSourcePath, aDestPath); {$ENDIF}
  end;
end;


//Move folder and rename all files inside by pattern _old_name_suffix to _new_name_suffix
//Pattern thatwe use for most of the files for our maps/saves
function KMMoveFolder(aSourceFolder, aDestFolder: UnicodeString): Boolean;
var
  I: Integer;
  SrcName, DestName, RenamedFile: UnicodeString;
  SearchRec: TSearchRec;
  FilesToMove: TStringList;
begin
  Result := False;
  if (Trim(aSourceFolder) = '')
    or (Trim(aDestFolder) = '')
    or (aSourceFolder = aDestFolder)
    or not DirectoryExists(aSourceFolder) then Exit;

  SrcName := GetFileDirName(aSourceFolder);
  DestName := GetFileDirName(aDestFolder);

  FilesToMove := TStringList.Create;
  try
    //Remove existing dest directory
    KMDeleteFolder(aDestFolder);

    //Move directory to dest
    KMRenamePath(aSourceFolder, aDestFolder);

    //Find all files to move in dest
    //Need to find them first, rename later, because we can possibly find files, that were already renamed, in case NewName = OldName + Smth
    FindFirst(aDestFolder + SrcName + '*', faAnyFile - faDirectory, SearchRec);
    repeat
      if (SearchRec.Name <> '.') and (SearchRec.Name <> '..')
        and (Length(SearchRec.Name) > Length(SrcName)) then
        FilesToMove.Add(SearchRec.Name);
    until (FindNext(SearchRec) <> 0);
    FindClose(SearchRec);

    //Move all previously finded files
    for I := 0 to FilesToMove.Count - 1 do
    begin
       RenamedFile := aDestFolder + DestName + RightStr(FilesToMove[I], Length(FilesToMove[I]) - Length(SrcName));
       if not FileExists(RenamedFile) and (aDestFolder + FilesToMove[I] <> RenamedFile) then
         KMRenamePath(aDestFolder + FilesToMove[I], RenamedFile);
    end;
  finally
    FilesToMove.Free;
  end;
  Result := True;
end;


end.
