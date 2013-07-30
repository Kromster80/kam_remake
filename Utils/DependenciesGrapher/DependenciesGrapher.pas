unit DependenciesGrapher;

interface

uses Winapi.Windows, System.SysUtils, System.Classes, System.StrUtils;

type
  TDependenciesGrapher = class
  private
    unitForAnalyse : TStringList;
    unitAnalysed : TStringList;
    nonSystemUnit : TStringList;
    nonSystemUnitFile : TStringList;
    graph : array of array of integer;
    fileOfTextStrList : TStringList;
    fileOfText : string;
    fileOfTextPos : integer;
    analyseProgress : integer;
    unitNumberToLoad : integer;
    unitNumberLoaded : integer;
    procedure ScanDpr( filename : string );
    function ReadWord() : string;
    procedure ScanForAllUnits( path : string; quickscan : boolean );
    procedure ScanForUnitsInDir( path : string; quickscan : boolean );
    procedure ScanUnitName( filename : string );
    procedure Analyse();
    procedure ScanForDep( path : string; id : integer );
    function GetUnitId( name : string ) : integer;
    function IsUnitInAnalyseList( name : string ) : boolean;
    function IsUnitAnalysed( name : string ) : boolean;
    procedure SkipToUses();
    procedure ReadDepAndFillGraph( id : integer; dep_type : integer );
    procedure RefactorFileText();
    procedure LoadFile( filename : string );
    function CheckEOF() : boolean;
    procedure CutSymbol( s : string; var str : string );
  public
    constructor Create();
    destructor Free();
    procedure BuildGraph( pathToDpr : string );
    procedure PrintOutput( path : string );
    function GetAnalyseProgress() : integer;
  end;

implementation

constructor TDependenciesGrapher.Create();
begin
  inherited Create;
  unitForAnalyse := TStringList.Create();
  unitAnalysed := TStringList.Create();
  nonSystemUnit := TStringList.Create();
  nonSystemUnitFile := TStringList.Create();
  fileOfTextStrList := TStringList.Create();
end;

destructor TDependenciesGrapher.Free();
begin
  unitForAnalyse.Free();
  unitAnalysed.Free();
  nonSystemUnit.Free();
  nonSystemUnitFile.Free();
  fileOfTextStrList.Free();
  //inherited Free;
end;

function TDependenciesGrapher.GetAnalyseProgress() : integer;
begin
  Result := analyseProgress;
end;

procedure TDependenciesGrapher.BuildGraph( pathToDpr : string );
var path : string;
    i : integer;
begin
  path := ExtractFilePath( pathToDpr );
  ScanForAllUnits( path, true );
  unitNumberToLoad := unitNumberToLoad * 2; // Units in project are loaded twice
  ScanForAllUnits( path, false );
  ScanDpr( pathToDpr );
  Analyse();
  analyseProgress := 100;
end;

procedure TDependenciesGrapher.ScanDpr( filename : string );
var
    str, path : string;
    lastUnitId, del_pos : integer;
    i: integer;
begin
  path := ExtractFilePath( filename );
  LoadFile( filename );
  SkipToUses();

  repeat
    str := ReadWord;

    if( not ( SameText( str , 'in' ) ) ) then
    begin
      if ( str[1] <> #39 ) and ( str[length(str)] <> ',' ) then
      begin
        unitForAnalyse.Add( str );
      end;
      if ( str[1] = #39 ) then
      begin
        CutSymbol( #39, str );
        CutSymbol( #39, str );
        CutSymbol( ',', str );
        CutSymbol( ';', str );
        for I := 0 to nonSystemUnit.Count - 1   do
        begin
          if nonSystemUnit[i] = unitForAnalyse[ unitForAnalyse.Count - 1] then
            nonSystemUnitFile[i] := path + str;
        end;
      end;
    end;
  until SameText(str, 'begin');
  unitForAnalyse.Delete( unitForAnalyse.Count - 1 );
end;

procedure TDependenciesGrapher.CutSymbol( s : string; var str : string );
var delPos : integer;
begin
  delPos := PosEx( s, str );
  if delPos > 0 then
    Delete( str, delPos, 1 );
end;

procedure TDependenciesGrapher.LoadFile( filename : string );
begin
  fileOfTextStrList.LoadFromFile( filename );
  RefactorFileText();
end;

procedure TDependenciesGrapher.RefactorFileText();
var
  i, j: Integer;
begin
  fileOfText := fileOfTextStrList.Text;
  fileOfTextPos := 1;

  // Inserting whitespaces after all ',' and ';'
  i := 1;
  while i < Length( fileOfText )  do
  begin
    if ( fileOfText[i] = ',' ) or ( fileOfText[i] = ';' ) then
      insert( ' ', fileOfText, i + 1 );
    inc(i);
  end;

  // Deleting {} comments
  i := 0;
  repeat
    i := PosEx( '{', fileOfText, i + 1 );
    if i > 0 then
    begin
      j := PosEx( '}', fileOfText, i + 1 );
      if j > 0 then
        delete( fileOfText, i, j - i + 1 );
    end;
  until i = 0;
  // Deleting (* *) comments
  i := 0;
  repeat
    i := PosEx( '(*', fileOfText, i + 1 );
    if i > 0 then
    begin
      j := PosEx( '*)', fileOfText, i + 1 );
      if j > 0 then
        delete( fileOfText, i, j - i + 2 );
    end;
  until i = 0;
  // Deleting // comments
  i := 0;
  repeat
    i := PosEx( '//', fileOfText );
    if i > 0 then
    begin
      j := PosEx( #13#10, fileOfText, i );
      if j > 0 then
        delete( fileOfText, i, j - i + 2 )
      else
      begin
        j := PosEx( #10, fileOfText, i );
        if j > 0 then
          delete( fileOfText, i, j - i + 1 );
      end;
    end;
  until i = 0;

  //Remove eol symbols (irregardless of EOL-style)
  // doesnt work yet, assertion in ScanUnitName fails
  {StringReplace(fileOfText, #10, '', [rfReplaceAll, rfIgnoreCase]);
  StringReplace(fileOfText, #13, '', [rfReplaceAll, rfIgnoreCase]);}

  // Deleting eol symbols
  i := 1;
  while i < Length( fileOfText )  do
  begin
    if ( fileOfText[i] = #13 ) then
    begin
      delete( fileOfText, i, 2 );
      insert( ' ', fileOfText, i );
    end;
    inc(i);
  end;

  // Deleting extra whitespaces
  i := 1;
  while i < Length( fileOfText )  do
  begin
    if ( fileOfText[i] = ' ' ) then
    begin
      j := i;
      while fileOfText[j] = ' ' do
        inc(j);
      delete( fileOfText, i, j - i - 1 );
    end;
    inc(i);
  end;
  // File cannot start with a whitespace
  if fileOfText[1] = ' ' then
    delete( fileOfText, 1, 1 );
end;

procedure TDependenciesGrapher.SkipToUses();
var str : string;
begin
  repeat
    str := ReadWord;
  until SameText(str, 'uses') or CheckEOF();
end;

function TDependenciesGrapher.ReadWord() : string;
var
    str : string;
begin
  str := '';
  if CheckEOF then
  begin
    ReadWord := '';
    exit;
  end;

  while not ( FileOfText[fileOfTextPos] = ' ' ) do
  begin
    str := str + FileOfText[fileOfTextPos];
    inc(fileOfTextPos);
  end;

  inc(fileOfTextPos);

  ReadWord := str;
end;

function TDependenciesGrapher.CheckEOF() : boolean;
begin
  Result := false;
  if fileOfTextPos >= Length( FileOfText ) then
    Result := true;
end;

procedure TDependenciesGrapher.ScanForAllUnits( path : string; quickscan : boolean );
var
    searchRec : TSearchRec;
begin
  ScanForUnitsInDir( path, quickscan );

  searchRec.Name := '';
  FindFirst( path + '*', faDirectory, searchRec );  // searchRec.Name = '.'
  FindNext( searchRec );  // searchRec.Name = '..'
  while FindNext( searchRec ) = 0 do
  begin
    ScanForUnitsInDir( path + searchRec.Name + '\', quickscan  );
  end;
end;

procedure TDependenciesGrapher.ScanForUnitsInDir( path : string; quickscan : boolean );
var fileData : TWIN32FindData;
    seachInfo : WinAPI.Windows.THandle;
begin
  fileData.cFileName := '';
  seachInfo := FindFirstFile( pchar(path + '*.pas'), fileData );
  if seachInfo = INVALID_HANDLE_VALUE then
    exit;
  inc(unitNumberToLoad);
  if not quickscan then
    ScanUnitName( path + fileData.cFileName );

  while FindNextFile( seachInfo, fileData )  do
  begin
    inc(unitNumberToLoad);
    if not quickscan then
      ScanUnitName( path + fileData.cFileName );
  end;
end;

procedure TDependenciesGrapher.ScanUnitName( filename : string );
var str : string;
    i : integer;
begin
  LoadFile( filename );
  inc(unitNumberLoaded);
  analyseProgress := ( unitNumberLoaded * 100 ) div unitNumberToLoad;

  str := ReadWord;
  assert( SameText( str, 'unit' ) );
  str := ReadWord;
  i := length(str);
  while str[i] <> ';' do
    dec(i);
  delete( str, i, length(str) - i + 1 );  // delete ';'
  nonSystemUnit.Add( str );
  nonSystemUnitFile.Add( filename );

end;

procedure TDependenciesGrapher.Analyse();
var
    nonSysUnId : integer;
    i, j: Integer;
begin

  SetLength( graph, nonSystemUnit.Count, nonSystemUnit.Count );

  repeat
    unitAnalysed.Add( unitForAnalyse[unitForAnalyse.Count-1] );
    unitForAnalyse.Delete( unitForAnalyse.Count-1 );

    nonSysUnId := GetUnitId( unitAnalysed[unitAnalysed.Count-1] );

    ScanForDep( nonSystemUnitFile[nonSysUnId], nonSysUnId );
  until unitForAnalyse.Count = 0;
end;

procedure TDependenciesGrapher.ScanForDep( path : string; id : integer );
var
    str : string;
    stop : boolean;
begin
  assert( id >= 0 );
  LoadFile( path );

  SkipToUses();

  if CheckEOF() then
    exit;

  ReadDepAndFillGraph( id, 1 );  // interface type

  SkipToUses();

  if CheckEOF() then
    exit;

  ReadDepAndFillGraph( id, 2 );  // implementation type

end;

procedure TDependenciesGrapher.ReadDepAndFillGraph( id : integer; dep_type : integer );
var stop : boolean;
    str : string;
    dep_id : integer;
begin
  stop := false;
  repeat
    str := ReadWord;

    if (str[length(str)] = ';') or (str[length(str)] = ',') then
    begin
      if ( pos( ';', str ) > 0 ) then
        stop := true;
      delete( str, length(str), 1 );  // delete ';' or ','
    end;

    if ( length(str) > 0 ) then
    begin
      dep_id := GetUnitId( str );

      if dep_id >= 0 then
      begin
        graph[id][dep_id] := dep_type;
        if ( not IsUnitInAnalyseList( str ) ) and ( not IsUnitAnalysed( str ) ) then
        begin
            unitForAnalyse.Add( str );
        end;
      end;
    end;
  until stop;
end;

function TDependenciesGrapher.GetUnitId( name : string ) : integer;
var res, i : integer;
begin
  res := -1;
  for i := 0 to nonSystemUnit.Count - 1 do
  begin
    if SameText( nonSystemUnit[i], name ) then
      res := i;
  end;
  GetUnitId := res;
end;

function TDependenciesGrapher.IsUnitInAnalyseList( name : string ) : boolean;
var i : integer;
    res : boolean;
begin
  res := false;
  for i := 0 to unitForAnalyse.Count - 1 do
  begin
    if SameText( unitForAnalyse[i], name ) then
      res := true;
  end;
  IsUnitInAnalyseList := res;
end;

function TDependenciesGrapher.IsUnitAnalysed( name : string ) : boolean;
var i : integer;
    res : boolean;
begin
  res := false;
  for i := 0 to unitAnalysed.Count - 1 do
  begin
    if SameText( unitAnalysed[i], name ) then
      res := true;
  end;
  IsUnitAnalysed := res;
end;

procedure TDependenciesGrapher.PrintOutput( path : string );
var i, j : integer;
    id : integer;
    f : text;
begin
  assignFile( f, path );
  rewrite( f );

  writeln( f, 'id;name' );
  for i := 0 to nonSystemUnit.Count - 1 do
    writeln( f, i, ';' ,nonSystemUnit[i] );

  writeln(f);
  writeln( f, 'id1;id2;type' );
  for i := 0 to unitAnalysed.Count - 1 do
  begin
    id := GetUnitId( unitAnalysed[i] );
    for j := 0 to nonSystemUnit.Count - 1 do
    begin
      if graph[id][j] > 0 then
        writeln( f, id, ';', j, ';', graph[id][j] );
    end;
  end;

  close( f );
end;


end.
