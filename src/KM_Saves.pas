unit KM_Saves;
{$I KaM_Remake.inc}
interface
uses
  Classes, KromUtils, Math, Windows, SysUtils, SyncObjs,
  KM_CommonClasses, KM_Defaults, KM_GameInfo, KM_GameOptions, KM_Minimap, KM_ResTexts, KM_Resource;


type
  TSavesSortMethod = (
    smByFileNameAsc, smByFileNameDesc,
    smByDescriptionAsc, smByDescriptionDesc,
    smByTimeAsc, smByTimeDesc,
    smByDateAsc, smByDateDesc,
    smByPlayerCountAsc, smByPlayerCountDesc,
    smByModeAsc, smByModeDesc);

  TKMSaveInfo = class;
  TSaveEvent = procedure (aSave: TKMSaveInfo) of object;

  //Savegame info, most of which is stored in TKMGameInfo structure
  TKMSaveInfo = class
  private
    fPath: string; //TKMGameInfo does not stores paths, because they mean different things for Maps and Saves
    fFileName: string; //without extension
    fCRC: Cardinal;
    fSaveError: string;
    fInfo: TKMGameInfo;
    fGameOptions: TKMGameOptions;
    procedure ScanSave;
  public
    constructor Create(const aPath, aFileName: String);
    destructor Destroy; override;

    property Info: TKMGameInfo read fInfo;
    property GameOptions: TKMGameOptions read fGameOptions;
    property Path: string read fPath;
    property FileName: string read fFileName;
    property CRC: Cardinal read fCRC;

    function IsValid: Boolean;
    function IsReplayValid: Boolean;
    function LoadMinimap(aMinimap: TKMMinimap): Boolean;
  end;

  TTSavesScanner = class(TThread)
  private
    fMultiplayerPath: Boolean;
    fOnSaveAdd: TSaveEvent;
    fOnSaveAddDone: TNotifyEvent;
  public
    constructor Create(aMultiplayerPath: Boolean; aOnSaveAdd: TSaveEvent; aOnSaveAddDone, aOnComplete: TNotifyEvent);
    procedure Execute; override;
  end;

  TKMSavesCollection = class
  private
    fCount: Word;
    fSaves: array of TKMSaveInfo;
    fSortMethod: TSavesSortMethod;
    CS: TCriticalSection;
    fScanner: TTSavesScanner;
    fScanning: Boolean;
    fScanFinished: Boolean;
    fUpdateNeeded: Boolean;
    fOnRefresh: TNotifyEvent;
    fOnComplete: TNotifyEvent;
    procedure Clear;
    procedure SaveAdd(aSave: TKMSaveInfo);
    procedure SaveAddDone(Sender: TObject);
    procedure ScanComplete(Sender: TObject);
    procedure DoSort;
    function GetSave(aIndex: Integer): TKMSaveInfo;
  public
    constructor Create(aSortMethod: TSavesSortMethod = smByFileNameDesc);
    destructor Destroy; override;

    property Count: Word read fCount;
    property SavegameInfo[aIndex: Integer]: TKMSaveInfo read GetSave; default;
    procedure Lock;
    procedure Unlock;

    procedure Refresh(aOnRefresh: TNotifyEvent; aMultiplayerPath: Boolean; aOnComplete: TNotifyEvent = nil);
    procedure TerminateScan;
    procedure Sort(aSortMethod: TSavesSortMethod; aOnSortComplete: TNotifyEvent);
    property SortMethod: TSavesSortMethod read fSortMethod; //Read-only because we should not change it while Refreshing
    property ScanFinished: Boolean read fScanFinished;

    function Contains(aNewName: UnicodeString): Boolean;
    procedure DeleteSave(aIndex: Integer);
    procedure RenameSave(aIndex: Integer; aName: UnicodeString);

    function SavesList: UnicodeString;
    procedure UpdateState;
  end;


implementation


{ TKMSaveInfo }
constructor TKMSaveInfo.Create(const aPath, aFileName: String);
begin
  inherited Create;
  fPath := aPath;
  fFileName := aFileName;
  fInfo := TKMGameInfo.Create;
  fGameOptions := TKMGameOptions.Create;

  //We could postpone this step till info is actually required
  //but we do need title and TickCount right away, so it's better just to scan it ASAP
  ScanSave;
end;


destructor TKMSaveInfo.Destroy;
begin
  fInfo.Free;
  fGameOptions.Free;
  inherited;
end;


procedure TKMSaveInfo.ScanSave;
var
  LoadStream: TKMemoryStream;
begin
  if not FileExists(fPath + fFileName + '.sav') then
  begin
    fSaveError := 'File not exists';
    Exit;
  end;

  fCRC := Adler32CRC(fPath + fFileName + '.sav');

  LoadStream := TKMemoryStream.Create; //Read data from file into stream
  LoadStream.LoadFromFile(fPath + fFileName + '.sav');

  fInfo.Load(LoadStream);
  fGameOptions.Load(LoadStream);
  fSaveError := fInfo.ParseError;

  if (fSaveError = '') and (fInfo.DATCRC <> gRes.GetDATCRC) then
    fSaveError := gResTexts[TX_SAVE_UNSUPPORTED_MODS];

  if fSaveError <> '' then
    fInfo.Title := fSaveError;

  LoadStream.Free;
end;


function TKMSaveInfo.LoadMinimap(aMinimap: TKMMinimap): Boolean;
var
  LoadStream, LoadMnmStream: TKMemoryStream;
  DummyInfo: TKMGameInfo;
  DummyOptions: TKMGameOptions;
  IsMultiplayer: Boolean;
  MinimapFilePath: String;
begin
  Result := False;
  if not FileExists(fPath + fFileName + '.sav') then Exit;

  DummyInfo := TKMGameInfo.Create;
  DummyOptions := TKMGameOptions.Create;
  LoadStream := TKMemoryStream.Create; //Read data from file into stream
  try
    LoadStream.LoadFromFile(fPath + fFileName + '.sav');

    DummyInfo.Load(LoadStream); //We don't care, we just need to skip past it correctly
    DummyOptions.Load(LoadStream); //We don't care, we just need to skip past it correctly
    LoadStream.Read(IsMultiplayer);
    if not IsMultiplayer then
    begin
      aMinimap.LoadFromStream(LoadStream);
      Result := True;
    end else begin
      // Lets try to load Minimap for MP save
      LoadMnmStream := TKMemoryStream.Create;
      try
        try
          MinimapFilePath := fPath + fFileName + '.' + MP_MINIMAP_SAVE_EXT;
          if FileExists(MinimapFilePath) then
          begin
            LoadMnmStream.LoadFromFile(MinimapFilePath); // try to load minimap from file
            aMinimap.LoadFromStream(LoadMnmStream);
            Result := True;
          end;
        except
          // Ignore any errors, because MP minimap is optional
        end;
      finally
        LoadMnmStream.Free;
      end;
    end;

  finally
    DummyInfo.Free;
    DummyOptions.Free;
    LoadStream.Free;
  end;
end;


function TKMSaveInfo.IsValid: Boolean;
begin
  Result := FileExists(fPath + fFileName + '.sav') and (fSaveError = '') and fInfo.IsValid(True);
end;


//Check if replay files exist at location
function TKMSaveInfo.IsReplayValid: Boolean;
begin
  Result := FileExists(fPath + fFileName + '.bas') and
            FileExists(fPath + fFileName + '.rpl');
end;


{ TKMSavesCollection }
constructor TKMSavesCollection.Create(aSortMethod: TSavesSortMethod = smByFileNameDesc);
begin
  inherited Create;
  fSortMethod := aSortMethod;
  fScanFInished := True;

  //CS is used to guard sections of code to allow only one thread at once to access them
  //We mostly don't need it, as UI should access Maps only when map events are signaled
  //it acts as a safenet mostly
  CS := TCriticalSection.Create;
end;


destructor TKMSavesCollection.Destroy;
begin
  //Terminate and release the Scanner if we have one working or finished
  TerminateScan;

  //Release TKMapInfo objects
  Clear;

  CS.Free;
  inherited;
end;


procedure TKMSavesCollection.Lock;
begin
  CS.Enter;
end;


procedure TKMSavesCollection.Unlock;
begin
  CS.Leave;
end;


procedure TKMSavesCollection.Clear;
var
  I: Integer;
begin
  Assert(not fScanning, 'Guarding from access to inconsistent data');
  for I := 0 to fCount - 1 do
    fSaves[i].Free;
  fCount := 0;
end;


function TKMSavesCollection.GetSave(aIndex: Integer): TKMSaveInfo;
begin
  //No point locking/unlocking here since we return a TObject that could be modified/freed
  //by another thread before the caller uses it.
  Assert(InRange(aIndex, 0, fCount-1));
  Result := fSaves[aIndex];
end;


function TKMSavesCollection.Contains(aNewName: UnicodeString): Boolean;
var
  I: Integer;
begin
  Result := False;

  for I := 0 to fCount - 1 do
    if LowerCase(fSaves[I].FileName) = LowerCase(aNewName) then
    begin
      Result := True;
      Exit;
    end;
end;


procedure TKMSavesCollection.DeleteSave(aIndex: Integer);
var
  I: Integer;
begin
  Lock;
  try
    Assert(InRange(aIndex, 0, fCount-1));
    DeleteFile(fSaves[aIndex].Path + fSaves[aIndex].fFileName + '.sav');
    DeleteFile(fSaves[aIndex].Path + fSaves[aIndex].fFileName + '.rpl');
    DeleteFile(fSaves[aIndex].Path + fSaves[aIndex].fFileName + '.bas');
    fSaves[aIndex].Free;
    for I := aIndex to fCount - 2 do
      fSaves[I] := fSaves[I+1]; //Move them down
    dec(fCount);
    SetLength(fSaves, fCount);
  finally
    Unlock;
  end;
end;


procedure TKMSavesCollection.RenameSave(aIndex: Integer; aName: UnicodeString);
var
  fileOld, fileNew: UnicodeString;
begin
  Lock;
  try
    fileOld := fSaves[aIndex].Path + fSaves[aIndex].fFileName;
    fileNew := fSaves[aIndex].Path + aName;
    RenameFile(fileOld + '.sav', fileNew + '.sav');
    RenameFile(fileOld + '.rpl', fileNew + '.rpl');
    RenameFile(fileOld + '.bas', fileNew + '.bas');
  finally
    Unlock;
  end;
end;


//For private acces, where CS is managed by the caller
procedure TKMSavesCollection.DoSort;
var TempSaves: array of TKMSaveInfo;
  //Return True if items should be exchanged
  function Compare(A, B: TKMSaveInfo): Boolean;
  begin
    Result := False; //By default everything remains in place
    case fSortMethod of
      smByFileNameAsc:     Result := CompareText(A.FileName, B.FileName) < 0;
      smByFileNameDesc:    Result := CompareText(A.FileName, B.FileName) > 0;
      smByDescriptionAsc:  Result := CompareText(A.Info.GetTitleWithTime, B.Info.GetTitleWithTime) < 0;
      smByDescriptionDesc: Result := CompareText(A.Info.GetTitleWithTime, B.Info.GetTitleWithTime) > 0;
      smByTimeAsc:         Result := A.Info.TickCount < B.Info.TickCount;
      smByTimeDesc:        Result := A.Info.TickCount > B.Info.TickCount;
      smByDateAsc:         Result := A.Info.SaveTimestamp > B.Info.SaveTimestamp;
      smByDateDesc:        Result := A.Info.SaveTimestamp < B.Info.SaveTimestamp;
      smByPlayerCountAsc:  Result := A.Info.PlayerCount < B.Info.PlayerCount;
      smByPlayerCountDesc: Result := A.Info.PlayerCount > B.Info.PlayerCount;
      smByModeAsc:         Result := A.Info.MissionMode < B.Info.MissionMode;
      smByModeDesc:        Result := A.Info.MissionMode > B.Info.MissionMode;
    end;
  end;

  procedure MergeSort(left, right: integer);
  var middle, i, j, ind1, ind2: integer;
  begin
    if right <= left then
      exit;

    middle := (left+right) div 2;
    MergeSort(left, middle);
    Inc(middle);
    MergeSort(middle, right);
    ind1 := left;
    ind2 := middle;
    for i := left to right do
    begin
      if (ind1 < middle) and ((ind2 > right) or not Compare(fSaves[ind1], fSaves[ind2])) then
      begin
        TempSaves[i] := fSaves[ind1];
        Inc(ind1);
      end
      else
      begin
        TempSaves[i] := fSaves[ind2];
        Inc(ind2);
      end;
    end;
    for j := left to right do
      fSaves[j] := TempSaves[j];
  end;
begin
  SetLength(TempSaves, Length(fSaves));
  MergeSort(Low(fSaves), High(fSaves));
end;


function TKMSavesCollection.SavesList: UnicodeString;
var
  I: Integer;
begin
  Lock;
  try
    Result := '';
    for I := 0 to fCount - 1 do
      Result := Result + fSaves[I].FileName + EolW;
  finally
    Unlock;
  end;
end;


procedure TKMSavesCollection.UpdateState;
begin
  if fUpdateNeeded then
  begin
    if Assigned(fOnRefresh) then
      fOnRefresh(Self);

    fUpdateNeeded := False;
  end;
end;


//For public access
//Apply new Sort within Critical Section, as we could be in the Refresh phase
//note that we need to preserve fScanning flag
procedure TKMSavesCollection.Sort(aSortMethod: TSavesSortMethod; aOnSortComplete: TNotifyEvent);
begin
  Lock;
  try
    if fScanning then
    begin
      fScanning := False;
      fSortMethod := aSortMethod;
      DoSort;
      if Assigned(aOnSortComplete) then
        aOnSortComplete(Self);
      fScanning := True;
    end
    else
    begin
      fSortMethod := aSortMethod;
      DoSort;
      if Assigned(aOnSortComplete) then
        aOnSortComplete(Self);
    end;
  finally
    Unlock;
  end;
end;


procedure TKMSavesCollection.TerminateScan;
begin
  if (fScanner <> nil) then
  begin
    fScanner.Terminate;
    fScanner.WaitFor;
    fScanner.Free;
    fScanner := nil;
    fScanning := False;
  end;
  fUpdateNeeded := False; //If the scan was terminated we should not run fOnRefresh next UpdateState
end;


//Start the refresh of maplist
procedure TKMSavesCollection.Refresh(aOnRefresh: TNotifyEvent; aMultiplayerPath: Boolean; aOnComplete: TNotifyEvent = nil);
begin
  //Terminate previous Scanner if two scans were launched consequentialy
  TerminateScan;
  Clear;

  fScanFinished := False;
  fOnRefresh := aOnRefresh;
  fOnComplete := aOnComplete;

  //Scan will launch upon create automatcally
  fScanning := True;
  fScanner := TTSavesScanner.Create(aMultiplayerPath, SaveAdd, SaveAddDone, ScanComplete);
end;


procedure TKMSavesCollection.SaveAdd(aSave: TKMSaveInfo);
begin
  Lock;
  try
    SetLength(fSaves, fCount + 1);
    fSaves[fCount] := aSave;
    Inc(fCount);

    //Set the scanning to false so we could Sort
    fScanning := False;

    //Keep the saves sorted
    //We signal from Locked section, so everything caused by event can safely access our Saves
    DoSort;

    fScanning := True;
  finally
    Unlock;
  end;
end;


procedure TKMSavesCollection.SaveAddDone(Sender: TObject);
begin
  fUpdateNeeded := True; //Next time the GUI thread calls UpdateState we will run fOnRefresh
end;


//All saves have been scanned
//No need to resort since that was done in last SaveAdd event
procedure TKMSavesCollection.ScanComplete(Sender: TObject);
begin
  Lock;
  try
    fScanning := False;
    fScanFinished := True;
    if Assigned(fOnComplete) then
      fOnComplete(Self);
  finally
    Unlock;
  end;
end;


{ TTSavesScanner }
//aOnSaveAdd - signal that there's new save that should be added
//aOnSaveAddDone - signal that save has been added
//aOnComplete - scan is complete
constructor TTSavesScanner.Create(aMultiplayerPath: Boolean; aOnSaveAdd: TSaveEvent; aOnSaveAddDone, aOnComplete: TNotifyEvent);
begin
  //Thread isn't started until all constructors have run to completion
  //so Create(False) may be put in front as well
  inherited Create(False);

  Assert(Assigned(aOnSaveAdd));

  fMultiplayerPath := aMultiplayerPath;
  fOnSaveAdd := aOnSaveAdd;
  fOnSaveAddDone := aOnSaveAddDone;
  OnTerminate := aOnComplete;
  FreeOnTerminate := False;
end;


procedure TTSavesScanner.Execute;
var
  pathToSaves: string;
  SearchRec: TSearchRec;
  Save: TKMSaveInfo;
begin
  if fMultiplayerPath then
    pathToSaves := ExeDir + 'SavesMP' + PathDelim
  else
    pathToSaves := ExeDir + 'Saves' + PathDelim;

  if not DirectoryExists(pathToSaves) then Exit;

  if FindFirst(pathToSaves + '*.sav', faAnyFile, SearchRec) = 0 then
  repeat
    if (SearchRec.Attr and faDirectory <> faDirectory) //Only files
    and (SearchRec.Name <> '.') and (SearchRec.Name <> '..')
    then
    begin
      Save :=  TKMSaveInfo.Create(pathToSaves, TruncateExt(SearchRec.Name));
      if SLOW_SAVE_SCAN then
        Sleep(50);
      fOnSaveAdd(Save);
      fOnSaveAddDone(Self);
    end;
  until (FindNext(SearchRec) <> 0) or Terminated;
  FindClose(SearchRec);
end;


end.
