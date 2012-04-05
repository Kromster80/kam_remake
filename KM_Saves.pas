unit KM_Saves;
{$I KaM_Remake.inc}
interface
uses
  Classes, KromUtils, Math, SysUtils, SyncObjs,
  KM_CommonClasses, KM_Defaults, KM_GameInfo, KM_GameOptions, KM_MapView;


type
  TSavesSortMethod = (
    smByFileNameAsc, smByFileNameDesc,
    smByDescriptionAsc, smByDescriptionDesc);

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

    function IsValid:boolean;
    procedure LoadMinimap(aMapView:TKMMapView);
  end;

  TTSavesScanner = class(TThread)
  private
    fMultiplayerPath: Boolean;
    fOnSaveAdd: TSaveEvent;
    fOnSaveAddDone: TNotifyEvent;
  public
    constructor Create(aMultiplayerPath: Boolean; aOnSaveAdd: TSaveEvent; aOnSaveAddDone, aOnComplete: TNotifyEvent);
    procedure SaveAddDone;
    procedure Execute; override;
  end;

  TKMSavesCollection = class
  private
    fCount: Word;
    fSaves: array of TKMSaveInfo;
    fMultiplayerPath: Boolean;
    fSortMethod: TSavesSortMethod;
    CS: TCriticalSection;
    fScanner: TTSavesScanner;
    fScanning: Boolean;
    fScanFinished: Boolean;
    fOnRefresh: TNotifyEvent;
    procedure Lock;
    procedure Unlock;
    procedure Clear;
    procedure SaveAdd(aSave: TKMSaveInfo);
    procedure SaveAddDone(Sender: TObject);
    procedure ScanComplete(Sender: TObject);
    procedure DoSort;
    function GetSave(aIndex: Integer): TKMSaveInfo;
  public
    constructor Create(aMultiplayerPath: Boolean; aSortMethod: TSavesSortMethod = smByFileNameDesc);
    destructor Destroy; override;

    property Count: Word read fCount;
    property SavegameInfo[aIndex: Integer]: TKMSaveInfo read GetSave; default;

    procedure Refresh(aOnRefresh: TNotifyEvent; aMultiplayerPath: Boolean);
    procedure TerminateScan;
    procedure Sort(aSortMethod: TSavesSortMethod; aOnSortComplete: TNotifyEvent);
    property SortMethod: TSavesSortMethod read fSortMethod; //Read-only because we should not change it while Refreshing
    property ScanFinished: Boolean read fScanFinished;

    procedure DeleteSave(aIndex: Integer);

    function SavesList: string;
  end;


implementation
uses KM_Log;

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

  if fSaveError <> '' then
    fInfo.Title := fSaveError;

  LoadStream.Free;
end;


procedure TKMSaveInfo.LoadMinimap(aMapView:TKMMapView);
var
  LoadStream: TKMemoryStream;
  DummyInfo: TKMGameInfo;
  DummyOptions: TKMGameOptions;
  IsMultiplayer: Boolean;
begin
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
      aMapView.Load(LoadStream);

  finally
    DummyInfo.Free;
    DummyOptions.Free;
    LoadStream.Free;
  end;
end;


function TKMSaveInfo.IsValid: Boolean;
begin
  Result := FileExists(fPath + fFileName + '.sav') and (fSaveError = '') and fInfo.IsValid;
//  fLog.AppendLog(fPath + fFileName + '.sav');
//  fLog.AppendLog(fSaveError);
end;


{ TKMSavesCollection }
constructor TKMSavesCollection.Create(aMultiplayerPath: Boolean; aSortMethod: TSavesSortMethod = smByFileNameDesc);
begin
  inherited Create;
  fMultiplayerPath := aMultiplayerPath;
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
var I: Integer;
begin
  Assert(not fScanning, 'Guarding from access to inconsistent data');
  for I := 0 to fCount - 1 do
    fSaves[i].Free;
  fCount := 0;
end;


function TKMSavesCollection.GetSave(aIndex: Integer): TKMSaveInfo;
begin
  Lock;
  try
    Assert(InRange(aIndex, 0, fCount-1));
    Result := fSaves[aIndex];
  finally
    Unlock;
  end;
end;


procedure TKMSavesCollection.DeleteSave(aIndex: Integer);
var I: Integer;
begin
  Assert(InRange(aIndex, 0, fCount-1));
  DeleteFile(fSaves[aIndex].Path + fSaves[aIndex].fFileName + '.sav');
  DeleteFile(fSaves[aIndex].Path + fSaves[aIndex].fFileName + '.rpl');
  DeleteFile(fSaves[aIndex].Path + fSaves[aIndex].fFileName + '.bas');
  fSaves[aIndex].Free;
  for I := aIndex to fCount - 2 do
    fSaves[I] := fSaves[I+1]; //Move them down
  dec(fCount);
  SetLength(fSaves, fCount);
end;


//For private acces, where CS is managed by the caller
procedure TKMSavesCollection.DoSort;
  //Return True if items should be exchanged
  function Compare(A, B: TKMSaveInfo; aMethod: TSavesSortMethod): Boolean;
  begin
    Result := False; //By default everything remains in place
    case aMethod of
      smByFileNameAsc:     Result := CompareText(A.FileName, B.FileName) < 0;
      smByFileNameDesc:    Result := CompareText(A.FileName, B.FileName) > 0;
      smByDescriptionAsc:  Result := CompareText(A.Info.GetTitleWithTime, B.Info.GetTitleWithTime) < 0;
      smByDescriptionDesc: Result := CompareText(A.Info.GetTitleWithTime, B.Info.GetTitleWithTime) > 0;
    end;
  end;
var
  I, K: Integer;
begin
  for I := 0 to fCount - 1 do
  for K := I to fCount - 1 do
  if Compare(fSaves[I], fSaves[K], fSortMethod) then
    SwapInt(Cardinal(fSaves[I]), Cardinal(fSaves[K])); //Exchange only pointers to MapInfo objects
end;


function TKMSavesCollection.SavesList: string;
var I: Integer;
begin
  Assert(not fScanning, 'Guarding from access to inconsistent data');
  Result := '';
  for I := 0 to fCount - 1 do
    Result := Result + fSaves[I].FileName + eol;
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
  end;
end;


//Start the refresh of maplist
procedure TKMSavesCollection.Refresh(aOnRefresh: TNotifyEvent; aMultiplayerPath: Boolean);
begin
  //Terminate previous Scanner if two scans were launched consequentialy
  TerminateScan;
  Clear;

  fScanFinished := False;
  fOnRefresh := aOnRefresh;
  fMultiplayerPath := aMultiplayerPath;

  //Scan will launch upon create automatcally
  fScanning := True;
  fScanner := TTSavesScanner.Create(fMultiplayerPath, SaveAdd, SaveAddDone, ScanComplete);
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
  //Signal that we let UI access Count and Saves list safely, as they check for fScanning flag
  fScanning := False;

  if Assigned(fOnRefresh) then
    fOnRefresh(Self);

  fScanning := True;
end;


//All saves have been scanned
//No need to resort since that was done in last SaveAdd event
procedure TKMSavesCollection.ScanComplete(Sender: TObject);
begin
  Lock;
  try
    fScanning := False;
    fScanFinished := True;
  finally
    Unlock;
  end;

end;


{ TTSavesScanner }
//aOnSaveAdd - signal that there's new save that should be added
//aOnSaveAddDone - signal that save has been added. Can safely call main thread methods since it's executed in Synchronize
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


//We need this wrapper since Synchronize can only call parameterless methods
procedure TTSavesScanner.SaveAddDone;
begin
  fOnSaveAddDone(Self);
end;


procedure TTSavesScanner.Execute;
var
  PathToSaves: String;
  SearchRec: TSearchRec;
  Save: TKMSaveInfo;
begin
  if fMultiplayerPath then PathToSaves := ExeDir + 'SavesMP\'
                   else PathToSaves := ExeDir + 'Saves\';

  if not DirectoryExists(PathToSaves) then Exit;

  FindFirst(PathToSaves + '*.sav', faAnyFile, SearchRec);
  repeat
    if (SearchRec.Attr and faDirectory <> faDirectory) //Only files
      and (SearchRec.Name <> '.') and (SearchRec.Name <> '..')
      and(TruncateExt(SearchRec.Name) <> 'basesave') //Ignore basesave - it is only used for crash reports
    then
    begin
      Save :=  TKMSaveInfo.Create(PathToSaves, TruncateExt(SearchRec.Name));
      if SLOW_SAVE_SCAN then
        Sleep(50);
      fOnSaveAdd(Save);
      Synchronize(SaveAddDone);
    end;
  until (FindNext(SearchRec) <> 0);
  FindClose(SearchRec);
end;


end.
