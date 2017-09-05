unit KM_Utils;
{$I KaM_Remake.inc}
interface
uses
  {$IFDEF MSWindows}
  Windows,
  {$ENDIF}
  {$IFDEF Unix}
  unix, baseunix, UnixUtil,
  {$ENDIF}
  {$IFDEF FPC} FileUtil, {$ENDIF}
  {$IFDEF WDC} IOUtils, {$ENDIF}
	SysUtils, StrUtils, Classes, Controls, KM_CommonTypes;

  function GetHintWHotKey(aTextId, aHotkeyId: Integer): String;

	function GetShiftState(aButton: TMouseButton): TShiftState;
  function GetMultiplicator(aButton: TMouseButton): Word; overload;
  function GetMultiplicator(aShift: TShiftState): Word; overload;

  function MoveFolder(aSourceFolder, aDestFolder: UnicodeString): Boolean;

implementation
uses
  KM_CommonUtils, KM_ResTexts, KM_ResKeys;


function MoveFolder(aSourceFolder, aDestFolder: UnicodeString): Boolean;
var
  I: Integer;
  SrcName, DestName, RenamedFile: UnicodeString;
  SearchRec: TSearchRec;
  FilesToMove: TStringList;
begin
  Result := False;
  if (Trim(aSourceFolder) = '') or (Trim(aDestFolder) = '')
    or (aSourceFolder = aDestFolder) then Exit;

  SrcName := GetFileDirName(aSourceFolder);
  DestName := GetFileDirName(aDestFolder);

  FilesToMove := TStringList.Create;
  try
    //Remove existing dest directory
    if DirectoryExists(aDestFolder) then
    begin
     {$IFDEF FPC} DeleteDirectory(aDestFolder, False); {$ENDIF}
     {$IFDEF WDC} TDirectory.Delete(aDestFolder, True); {$ENDIF}
    end;

    //Move directory to dest
    {$IFDEF FPC} RenameFile(aSourceFolder, aDestFolder); {$ENDIF}
    {$IFDEF WDC} TDirectory.Move(aSourceFolder, aDestFolder); {$ENDIF}

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
         {$IFDEF FPC} RenameFile(aDestFolder + FilesToMove[I], RenamedFile); {$ENDIF}
         {$IFDEF WDC} TFile.Move(aDestFolder + FilesToMove[I], RenamedFile); {$ENDIF}
    end;
  finally
    FilesToMove.Free;
  end;
  Result := True;
end;


function GetShiftState(aButton: TMouseButton): TShiftState;
begin
  Result := [];
  case aButton of
    mbLeft:   Include(Result, ssLeft);
    mbRight:  Include(Result, ssRight);
  end;

  if GetKeyState(VK_SHIFT) < 0 then
    Include(Result, ssShift);
end;


function GetMultiplicator(aButton: TMouseButton): Word;
begin
  Result := GetMultiplicator(GetShiftState(aButton));
end;


function GetMultiplicator(aShift: TShiftState): Word;
begin
  Result := Byte(aShift = [ssLeft]) + Byte(aShift = [ssRight]) * 10 + Byte(aShift = [ssShift, ssLeft]) * 100 + Byte(aShift = [ssShift, ssRight]) * 1000;
end;


function GetHintWHotKey(aTextId, aHotkeyId: Integer): String;
var
  HotKeyStr: String;
begin
  Result := gResTexts[aTextId];
  HotKeyStr := gResKeys.GetKeyNameById(aHotkeyId);
  if HotKeyStr <> '' then
    Result := Result + Format(' (''%s'')', [HotKeyStr]);

end;


end.

