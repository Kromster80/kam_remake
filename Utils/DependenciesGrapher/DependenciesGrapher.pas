unit DependenciesGrapher;
interface
uses Winapi.Windows, System.SysUtils, System.Classes, System.StrUtils,
  XMLDoc, XMLIntf,
  Generics.Collections, Generics.Defaults;

type
  TUnit = record
    UnitName: string;
    UnitPath: string;
  end;

  TUseSection = (usInterface, usImplementation);

  TUse = record
    Master, Slave: Integer;
    Section: TUseSection;
  end;

  TDependenciesGrapher = class
  private
    fRootPath: string;
    fSearchPaths: TStringList;
    fUnits: TList<TUnit>;
    fUses: TList<TUse>;

    fileOfText : string;
    fileOfTextPos : integer;

    function ExpandPath(aName, aPath: string): string;
    procedure ExpandPaths;
    procedure BuildEdges;
    procedure ExtractDependencies(aIndex: Integer; aSection: TUseSection);

    procedure ScanDpr(filename: string);
    function ReadWord: string;
    procedure SkipToUses;
    procedure RefactorFileText(var aText: string);
    procedure LoadFile(filename: string);
    function CheckEOF: Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure BuildGraph(pathToDpr: string);
    procedure ExportAsCsv(path: string);
  end;


implementation


function MakeUnit(aName, aPath: string): TUnit;
begin
  Result.UnitName := aName;
  Result.UnitPath := aPath;
end;


function MakeUse(aId1, aId2: Integer; aSect: TUseSection): TUse;
begin
  Result.Master := aId1;
  Result.Slave := aId2;
  Result.Section := aSect;
end;


constructor TDependenciesGrapher.Create;
begin
  inherited Create;

  fSearchPaths := TStringList.Create;
  fUnits := TList<TUnit>.Create;
  fUses := TList<TUse>.Create;
end;


destructor TDependenciesGrapher.Destroy;
begin
  fSearchPaths.Free;
  fUnits.Free;
  fUses.Free;

  inherited;
end;


procedure TDependenciesGrapher.BuildGraph(pathToDpr: string);
var
  xml: IXMLDocument;
  N: IXMLNode;
begin
  fRootPath := ExtractFilePath(pathToDpr);

  //Acquire search paths
  {xml := TXMLDocument.Create(nil);
  xml.Active := True;
  xml.LoadFromFile(pathToDpr);

  N := xml.ChildNodes['Root'];
  N := N.ChildNodes['Info'];}
  fSearchPaths.Append('');

  //Get list of files from DPR
  ScanDpr(ChangeFileExt(pathToDpr, '.dpr'));

  ExpandPaths;
  BuildEdges;
end;


procedure TDependenciesGrapher.ScanDpr(filename: string);
var
  str: string;
  strUnit, strPath: string;
begin
  LoadFile(filename);
  SkipToUses;

  repeat
    //Unit name
    str := ReadWord;
    strUnit := str;

    // ',' ';' or in
    str := ReadWord;
    Assert(SameText(str, 'in') or (str = ',') or (str = ';'));

    //Unit path follows
    if SameText(str, 'in') then
    begin
      str := ReadWord;
      Assert((str[1] = #39));
      strPath := StringReplace(str, '''', '', [rfReplaceAll]);
      str := ReadWord; //, or ;
    end;

    fUnits.Add(MakeUnit(strUnit, strPath));
    strUnit := '';
    strPath := '';

  until SameText(str, ';');
end;


//Guess files location
function TDependenciesGrapher.ExpandPath(aName, aPath: string): string;
begin
  //todo: include Search paths
  if ((aPath = '') and FileExists(fRootPath + aName + '.pas')) then
    Result := fRootPath + aName + '.pas'
  else
  if ((aPath <> '') and FileExists(fRootPath + aPath)) then
    Result := fRootPath + aPath
  else
    Result := '';
end;


procedure TDependenciesGrapher.ExpandPaths;
var
  I: Integer;
begin
  for I := 0 to fUnits.Count - 1 do
    fUnits[I] := MakeUnit(fUnits[I].UnitName, ExpandPath(fUnits[I].UnitName, fUnits[I].UnitPath));
end;


procedure TDependenciesGrapher.BuildEdges;
var
  I: Integer;
begin
  I := -1;
  while I < fUnits.Count - 1 do
  begin
    Inc(I);

    if fUnits[I].UnitPath = '' then Continue;

    LoadFile(fUnits[I].UnitPath);

    SkipToUses;

    if CheckEOF then
      Continue;

    ExtractDependencies(I, usInterface);

    SkipToUses;

    if CheckEOF then
      Continue;

    ExtractDependencies(I, usImplementation);
  end;
end;


procedure TDependenciesGrapher.ExtractDependencies(aIndex: Integer; aSection: TUseSection);
var
  str: string;
  I, K: Integer;
begin
  repeat
    str := ReadWord;

    K := -1;
    for I := 0 to fUnits.Count - 1 do
    if SameText(fUnits[I].UnitName, str) then
    begin
      K := I;
      Break;
    end;

    //Append new file to the list
    if K = -1 then
    begin
      fUnits.Add(MakeUnit(str, ExpandPath(str, '')));
      K := fUnits.Count - 1;
    end;

    fUses.Add(MakeUse(aIndex, K, aSection));

    str := ReadWord;
    Assert((str = ',') or (str = ';'));
  until (str = ';');
end;


procedure TDependenciesGrapher.LoadFile(filename: string);
var
  S: TStringList;
begin
  S := TStringList.Create;
  try
    S.LoadFromFile(filename);

    fileOfTextPos := 1;
    fileOfText := S.Text;

    RefactorFileText(fileOfText);
  finally
    S.Free;
  end;
end;

procedure TDependenciesGrapher.RefactorFileText(var aText: string);
var
  i, j: Integer;
begin
  // Inserting whitespaces around ',' and ';'
  aText := StringReplace(aText, ',', ' , ', [rfReplaceAll]);
  aText := StringReplace(aText, ';', ' ; ', [rfReplaceAll]);

  // Delete {} comments
  i := 0;
  repeat
    i := PosEx('{', aText, i + 1);
    if i > 0 then
    begin
      j := PosEx('}', aText, i + 1);
      if j > 0 then
        delete(aText, i, j - i + 1);
    end;
  until i = 0;

  // Delete (* *) comments
  i:= 0;
  repeat
    i:= PosEx('(*', aText, i + 1);
    if i > 0 then
    begin
      j:= PosEx('*)', aText, i + 1);
      if j > 0 then
        delete(aText, i, j - i + 2);
    end;
  until i = 0;

  // Delete // comments
  repeat
    i:= PosEx('//', aText);
    if i > 0 then
    begin
      j:= PosEx(#13#10, aText, i);
      if j > 0 then
        delete(aText, i, j - i + 2)
      else
      begin
        j:= PosEx(#10, aText, i);
        if j > 0 then
          delete(aText, i, j - i + 1);
      end;
    end;
  until i = 0;

  //Remove eol symbols (irregardless of EOL-style)
  aText:= StringReplace(aText, #10, ' ', [rfReplaceAll, rfIgnoreCase]);
  aText:= StringReplace(aText, #13, ' ', [rfReplaceAll, rfIgnoreCase]);

  // Delete extra whitespaces
  i:= 1;
  while i < Length(aText)  do
  begin
    if (aText[i] = ' ') then
    begin
      j:= i;
      while aText[j] = ' ' do
        inc(j);
      delete(aText, i, j - i - 1);
    end;
    inc(i);
  end;

  // File cannot start with a whitespace
  if aText[1] = ' ' then
    delete(aText, 1, 1);
end;

procedure TDependenciesGrapher.SkipToUses;
var str: string;
begin
  repeat
    str:= ReadWord;
  until SameText(str, 'uses') or CheckEOF;
end;

function TDependenciesGrapher.ReadWord: string;
var
  str: string;
begin
  str := '';
  if CheckEOF then
  begin
    ReadWord := '';
    Exit;
  end;

  while not (FileOfText[fileOfTextPos] = ' ') do
  begin
    str := str + FileOfText[fileOfTextPos];
    Inc(fileOfTextPos);
  end;

  Inc(fileOfTextPos);

  ReadWord := str;
end;

function TDependenciesGrapher.CheckEOF: Boolean;
begin
  Result:= false;
  if fileOfTextPos >= Length(FileOfText) then
    Result:= True;
end;

procedure TDependenciesGrapher.ExportAsCsv(path: string);
var
  I: Integer;
  K: TUse;
  S: TStringList;
begin
  S := TStringList.Create;

  S.Append('id;name');
  for I := 0 to fUnits.Count - 1 do
    S.Append(IntToStr(I) + ';' + fUnits[I].UnitName);

  S.Append('');
  S.Append('id1;id2;type');
  for K in fUses do
    S.Append(IntToStr(K.Master) + ';' + IntToStr(K.Slave) + ';' + IntToStr(Byte(K.Section) + 1));

  S.SaveToFile(path);
end;



end.
