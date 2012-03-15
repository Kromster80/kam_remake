unit KM_Saves;
{$I KaM_Remake.inc}
interface
uses
  Classes, KromUtils, Math, SysUtils,
  KM_CommonClasses, KM_Defaults, KM_GameInfo, KM_GameOptions, KM_MapView;


type
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


  TKMSavesCollection = class
  private
    fCount: Word;
    fSaves: array of TKMSaveInfo;
    function GetSave(aIndex: Integer): TKMSaveInfo;
  public
    destructor Destroy; override;

    procedure Clear;
    property Count: Word read fCount;
    property SavegameInfo[aIndex: Integer]: TKMSaveInfo read GetSave; default;
    procedure ScanSavesFolder(IsMultiplayer: Boolean);
    procedure DeleteSave(aIndex: Integer);

    function SavesList: string;
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
  LoadStream:TKMemoryStream;
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
  LoadStream:TKMemoryStream;
  DummyInfo: TKMGameInfo;
  DummyOptions: TKMGameOptions;
  IsMultiplayer: Boolean;
begin
  if not FileExists(fPath + fFileName + '.sav') then Exit;

  LoadStream := TKMemoryStream.Create; //Read data from file into stream
  LoadStream.LoadFromFile(fPath + fFileName + '.sav');

  DummyInfo := TKMGameInfo.Create;
  DummyOptions := TKMGameOptions.Create;
  DummyInfo.Load(LoadStream); //We don't care, we just need to skip past it correctly
  DummyOptions.Load(LoadStream); //We don't care, we just need to skip past it correctly
  LoadStream.Read(IsMultiplayer);
  if not IsMultiplayer then aMapView.Load(LoadStream);

  DummyInfo.Free;
  DummyOptions.Free;
  LoadStream.Free;
end;


function TKMSaveInfo.IsValid: Boolean;
begin
  Result := FileExists(fPath + fFileName + '.sav') and (fSaveError = '') and fInfo.IsValid;
end;


{ TKMSavesCollection }
destructor TKMSavesCollection.Destroy;
begin
  Clear;
  inherited;
end;


procedure TKMSavesCollection.Clear;
var I: Integer;
begin
  for I := 0 to fCount - 1 do
    fSaves[i].Free;
  fCount := 0;
end;


function TKMSavesCollection.GetSave(aIndex: Integer): TKMSaveInfo;
begin
  Assert(InRange(aIndex, 0, fCount-1));
  Result := fSaves[aIndex];
end;


procedure TKMSavesCollection.DeleteSave(aIndex: Integer);
var i:integer;
begin
  Assert(InRange(aIndex, 0, fCount-1));
  DeleteFile(fSaves[aIndex].Path + fSaves[aIndex].fFileName + '.sav');
  DeleteFile(fSaves[aIndex].Path + fSaves[aIndex].fFileName + '.rpl');
  DeleteFile(fSaves[aIndex].Path + fSaves[aIndex].fFileName + '.bas');
  fSaves[aIndex].Free;
  for i := aIndex to fCount - 2 do
    fSaves[i] := fSaves[i+1]; //Move them down
  dec(fCount);
  SetLength(fSaves, fCount);
end;


//Scan either single or multiplayer saves
function TKMSavesCollection.SavesList: string;
var I: Integer;
begin
  Result := '';
  for I := 0 to fCount - 1 do
    Result := Result + fSaves[I].FileName + eol;
end;


procedure TKMSavesCollection.ScanSavesFolder(IsMultiplayer: Boolean);
var
  PathToSaves: String;
  SearchRec: TSearchRec;
begin
  Clear;

  if IsMultiplayer then PathToSaves := ExeDir+'SavesMP\'
                   else PathToSaves := ExeDir+'Saves\';

  if not DirectoryExists(PathToSaves) then Exit;

  FindFirst(PathToSaves+'*.sav', faAnyFile, SearchRec);
  repeat
    if (SearchRec.Attr and faDirectory <> faDirectory) //Only files
    and(SearchRec.Name<>'.')and(SearchRec.Name<>'..')
    and(TruncateExt(SearchRec.Name)<>'basesave') //Ignore basesave - it is only used for crash reports
    then
    begin
      inc(fCount);
      SetLength(fSaves, fCount);
      fSaves[fCount-1] := TKMSaveInfo.Create(PathToSaves, TruncateExt(SearchRec.Name));
    end;
  until (FindNext(SearchRec) <> 0);
  FindClose(SearchRec);
end;


end.

