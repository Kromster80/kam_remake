unit KM_Saves;
{$I KaM_Remake.inc}
interface
uses
  Classes, KromUtils, Math, Windows, SysUtils,
  KM_Defaults, KM_GameInfo;


type
  //Savegame info, most of which is stored in TKMGameInfo structure
  TKMSaveInfo = class
  private
    fPath: string; //TKMGameInfo does not stores paths, it would make no sense
    fFilename: string; //without extension
    fCRC: Cardinal;
    fSaveError: string;
    fInfo: TKMGameInfo;
    procedure ScanSave;
  public
    constructor Create(const aPath, aFilename: String);
    destructor Destroy; override;

    property Info: TKMGameInfo read fInfo;
    property Path: string read fPath;
    property Filename: string read fFilename;
    property CRC: Cardinal read fCRC;

    function IsValid:boolean;
  end;


  TKMSavesCollection = class
  private
    fCount: word;
    fSaves: array of TKMSaveInfo;
    function GetSave(Index:integer):TKMSaveInfo;
  public
    destructor Destroy; override;

    procedure Clear;
    property Count: Word read fCount;
    property SavegameInfo[Index:integer]:TKMSaveInfo read GetSave; default;
    procedure ScanSavesFolder(IsMultiplayer: Boolean);
    procedure DeleteSave(Index:integer);

    function SavesList: string;

    procedure AutoSave;
    procedure BaseSave;
  end;


implementation
uses KM_Game, KM_CommonTypes;


{ TKMSaveInfo }
constructor TKMSaveInfo.Create(const aPath, aFilename: String);
begin
  inherited Create;
  fPath := aPath;
  fFilename := aFilename;
  fInfo := TKMGameInfo.Create;

  //We could postpone this step till info is actually required
  //but we do need title and TickCount right away, so it's better just to scan it ASAP
  ScanSave;
end;


destructor TKMSaveInfo.Destroy;
begin
  fInfo.Free;
  inherited;
end;


procedure TKMSaveInfo.ScanSave;
var
  LoadStream:TKMemoryStream;
begin
  if not FileExists(fPath + fFilename + '.sav') then
  begin
    fSaveError := 'File not exists';
    Exit;
  end;

  fCRC := Adler32CRC(fPath + fFilename + '.sav');

  LoadStream := TKMemoryStream.Create; //Read data from file into stream
  LoadStream.LoadFromFile(fPath + fFilename + '.sav');

  fInfo.Load(LoadStream);
  fSaveError := fInfo.ParseError;

  if fSaveError <> '' then
    fInfo.Title := fSaveError;

  LoadStream.Free;
end;


function TKMSaveInfo.IsValid:boolean;
begin
  Result := FileExists(fPath + fFilename + '.sav') and (fSaveError = '') and fInfo.IsValid;
end;


{ TKMapsCollection }
destructor TKMSavesCollection.Destroy;
begin
  Clear;
  inherited;
end;


procedure TKMSavesCollection.AutoSave;
var i: integer;
begin
  fGame.Save('autosave'); //Temp file

  DeleteFile(fGame.SaveName('autosave'+int2fix(AUTOSAVE_COUNT,2), 'sav'));
  DeleteFile(fGame.SaveName('autosave'+int2fix(AUTOSAVE_COUNT,2), 'rpl'));
  DeleteFile(fGame.SaveName('autosave'+int2fix(AUTOSAVE_COUNT,2), 'bas'));
  for i:=AUTOSAVE_COUNT downto 2 do //03 to 01
  begin
    RenameFile(fGame.SaveName('autosave'+int2fix(i-1,2), 'sav'), fGame.SaveName('autosave'+int2fix(i,2), 'sav'));
    RenameFile(fGame.SaveName('autosave'+int2fix(i-1,2), 'rpl'), fGame.SaveName('autosave'+int2fix(i,2), 'rpl'));
    RenameFile(fGame.SaveName('autosave'+int2fix(i-1,2), 'bas'), fGame.SaveName('autosave'+int2fix(i,2), 'bas'));
  end;

  RenameFile(fGame.SaveName('autosave', 'sav'), fGame.SaveName('autosave01', 'sav'));
  RenameFile(fGame.SaveName('autosave', 'rpl'), fGame.SaveName('autosave01', 'rpl'));
  RenameFile(fGame.SaveName('autosave', 'bas'), fGame.SaveName('autosave01', 'bas'));
end;


procedure TKMSavesCollection.BaseSave;
begin
  fGame.Save('basesave'); //Temp file

  //In Linux CopyFile does not overwrite
  if FileExists(fGame.SaveName('basesave', 'bas')) then DeleteFile(fGame.SaveName('basesave','bas'));
  CopyFile(PChar(fGame.SaveName('basesave','sav')), PChar(fGame.SaveName('basesave','bas')), false);
end;


procedure TKMSavesCollection.Clear;
var i:integer;
begin
  for i:=0 to fCount-1 do
    fSaves[i].Free;
  fCount := 0;
end;


function TKMSavesCollection.GetSave(Index:integer):TKMSaveInfo;
begin
  Assert(InRange(Index, 0, fCount-1));
  Result := fSaves[Index];
end;


procedure TKMSavesCollection.DeleteSave(Index:integer);
var i:integer;
begin
  Assert(InRange(Index, 0, fCount-1));
  DeleteFile(fSaves[Index].Path + fSaves[Index].fFilename + '.sav');
  DeleteFile(fSaves[Index].Path + fSaves[Index].fFilename + '.rpl');
  DeleteFile(fSaves[Index].Path + fSaves[Index].fFilename + '.bas');
  fSaves[Index].Free;
  for i:=Index to fCount-2 do
    fSaves[i] := fSaves[i+1]; //Move them down
  dec(fCount);
  SetLength(fSaves,fCount);
end;


//Scan either single or multiplayer saves
function TKMSavesCollection.SavesList: string;
var i:integer;
begin
  Result := '';
  for i:=0 to fCount-1 do
    Result := Result + fSaves[i].Filename + eol;
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
    then
    begin
      inc(fCount);
      SetLength(fSaves, fCount);
      fSaves[fCount-1] := TKMSaveInfo.Create(PathToSaves, TruncateExt(SearchRec.Name));
    end;
  until (FindNext(SearchRec)<>0);
  FindClose(SearchRec);
end;


end.

