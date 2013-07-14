unit DependenciesGrapher;

interface

uses Winapi.Windows, System.SysUtils;

const
  MAX_UNITS_NUM = 1000;

type
  TDependenciesGrapher = class
  private
    dprFileName : string;
    unitForAnalyse : array[0..MAX_UNITS_NUM] of string;
    unitAnalysed : array[0..MAX_UNITS_NUM] of string;
    unitStackPos : integer;
    unitAnalysedCount : integer;
    nonSystemUnit : array[0..MAX_UNITS_NUM] of string;
    nonSystemUnitFile : array[0..MAX_UNITS_NUM] of string;
    numNonSysUnit : integer;
    graph : array[0..MAX_UNITS_NUM,0..MAX_UNITS_NUM] of integer;
    f : TextFile;
    procedure ScanDpr( filename : string );
    function ReadWord() : string;
    procedure ScanForUnits( path : string );
    procedure ScanUnitName( filename : string; numUnit : integer );
    procedure Analyse();
    procedure ScanForDep( path : string; id : integer );
    function GetUnitId( name : string ) : integer;
    function IsUnitInAnalyseList( name : string ) : boolean;
    function IsUnitAnalysed( name : string ) : boolean;
    procedure SkipToUses();
    procedure ReadDepAndFillGraph( id : integer; dep_type : integer );
  public
    procedure Init();
    function CheckIfFileDpr( filename : string ) : boolean;
    procedure SetDprPath( path : string );
    procedure BuildGraph;
    procedure PrintOutput( path : string );
  end;

implementation

procedure TDependenciesGrapher.Init();
var
  i,j: Integer;
begin
  unitStackPos := 0;
  unitAnalysedCount := 0;
  for i := 0 to MAX_UNITS_NUM do
    for j := 0 to MAX_UNITS_NUM do
      graph[i][j] := 0;
end;

procedure TDependenciesGrapher.SetDprPath( path : string );
begin
  dprFileName := path;
end;

procedure TDependenciesGrapher.BuildGraph;
var path : string;
    i : integer;
begin
  ScanDpr( dprFileName );
  path := dprFileName;
  i := length( dprFileName );
  while( path[i] <> '\' ) do
    dec(i);
  delete( path, i + 1, length( dprFileName ) - i ); // Cut projectname.dpr from the path
  ScanForUnits( path );
  Analyse();
end;

function TDependenciesGrapher.CheckIfFileDpr( filename : string ) : boolean;
begin
  if filename[length(filename)] <> 'r' then
  begin
    CheckIfFileDpr := false;
    exit;
  end;
  if filename[length(filename)-1] <> 'p' then
  begin
    CheckIfFileDpr := false;
    exit;
  end;
  if filename[length(filename)-2] <> 'd' then
  begin
    CheckIfFileDpr := false;
    exit;
  end;
  CheckIfFileDpr := true;
end;

procedure TDependenciesGrapher.ScanDpr( filename : string );
var
    str : string;
begin
  AssignFile( f, filename );
  reset( f );

  SkipToUses();

  repeat
    str := ReadWord;

    if ( str[1] = '{' ) or ( str[1] = '/' ) then
      readln( f )
    else
      if( not ( SameText( str , 'in' ) ) ) and ( str[1] <> #39 ) then
      begin
        if str[length(str)] <> ',' then
        begin
          unitForAnalyse[unitStackPos] := str;
          inc(unitStackPos);
        end;
      end;

  until SameText(str, 'begin');
  dec(unitStackPos);
  close( f );
end;

procedure TDependenciesGrapher.SkipToUses();
var str : string;
begin
  repeat
    str := ReadWord;
  until SameText(str, 'uses') or EOF( f );
end;

function TDependenciesGrapher.ReadWord() : string;
var c : char;
    str : string;
begin
    while( eoln( f ) ) and (not EOF(f)) do
      readln( f );
    c := ' ';
    while( c = ' ' ) and (not EOF(f)) do
    begin
      Read(f, c);
    end;

    while ( c <> ' ' ) and ( not eoln(f) ) and (not EOF(f)) do
    begin
      str := str + c;
      Read(f, c);
    end;

    if c <> ' ' then
      str := str + c;

  ReadWord := str;
end;

procedure TDependenciesGrapher.ScanForUnits( path : string );
var fileData : TWIN32FindData;
    str : string;
    numUnit : integer;
    seachInfo : WinAPI.Windows.THandle;
begin
  fileData.cFileName := '';
  seachInfo := FindFirstFile( pchar(path + '*.pas'), fileData );
  if seachInfo = ERROR_NO_MORE_FILES then
    exit;
  numUnit := 0;
  str := string(fileData.cFileName);
  str := string(fileData.cAlternateFileName);
  ScanUnitName( path + fileData.cFileName, numUnit );
  inc( numUnit );

  while FindNextFile( seachInfo, fileData )  do
  begin
    ScanUnitName( path + fileData.cFileName, numUnit );
    inc( numUnit );
  end;

  numNonSysUnit := numUnit;

  { TODO: Add seach in subdirectories }
end;

procedure TDependenciesGrapher.ScanUnitName( filename : string; numUnit : integer );
var str : string;
    i : integer;
begin
  AssignFile( f, filename );
  Reset( f );

  str := ReadWord;
  assert( SameText( str, 'unit' ) );
  str := ReadWord;
  i := length(str);
  while str[i] <> ';' do
    dec(i);
  delete( str, i, length(str) - i + 1 );  // delete ';'
  nonSystemUnit[numUnit] := str;
  nonSystemUnitFile[numUnit] := filename;

  Close( f );
end;

procedure TDependenciesGrapher.Analyse();
var
    nonSysUnId : integer;
begin
  repeat
    unitAnalysed[unitAnalysedCount] := unitForAnalyse[unitStackPos-1];
    inc(unitAnalysedCount);
    dec(unitStackPos);

    nonSysUnId := GetUnitId(unitAnalysed[unitAnalysedCount-1]);

    ScanForDep(nonSystemUnitFile[nonSysUnId], nonSysUnId );
  until unitStackPos = 0;
end;

procedure TDependenciesGrapher.ScanForDep( path : string; id : integer );
var
    str : string;
    stop : boolean;
begin
  assert( id >= 0 );
  AssignFile( f, path );
  Reset( f );

  SkipToUses();

  if EOF(f) then
    exit;

  ReadDepAndFillGraph( id, 1 );  // interface type

  SkipToUses();

  if EOF(f) then
    exit;

  ReadDepAndFillGraph( id, 2 );  // implementation type

  Close( f );
end;

procedure TDependenciesGrapher.ReadDepAndFillGraph( id : integer; dep_type : integer );
var stop : boolean;
    str : string;
    dep_id : integer;
begin
  stop := false;
  repeat
    str := ReadWord;
    if (str[1] = '{') then
      readln( f )
    else
    begin
      if (str[length(str)] = ';') or (str[length(str)] = ',') then
      begin
        if (str[length(str)] = ';') then
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
            unitForAnalyse[unitStackPos] := str;
            inc(unitStackPos);
          end;
        end;
      end;
    end;
  until stop;
end;

function TDependenciesGrapher.GetUnitId( name : string ) : integer;
var res, i : integer;
begin
  res := -1;
  for i := 0 to numNonSysUnit-1 do
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
  for i := 0 to unitStackPos - 1 do
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
  for i := 0 to unitAnalysedCount - 1 do
  begin
    if SameText( unitAnalysed[i], name ) then
      res := true;
  end;
  IsUnitAnalysed := res;
end;

procedure TDependenciesGrapher.PrintOutput( path : string );
var i, j : integer;
    id : integer;
begin
  assignFile( f, path );
  rewrite( f );

  writeln( f, 'id;name' );
  for i := 0 to numNonSysUnit - 1 do
    writeln( f, i, ';' ,nonSystemUnit[i] );

  writeln(f);
  writeln( f, 'id1;id2;type' );
  for i := 0 to unitAnalysedCount - 1 do
  begin
    id := GetUnitId( unitAnalysed[i] );
    for j := 0 to MAX_UNITS_NUM - 1 do
    begin
      if graph[id][j] > 0 then
        writeln( f, id, ';', j, ';', graph[id][j] );
    end;
  end;

  close( f );
end;


end.
