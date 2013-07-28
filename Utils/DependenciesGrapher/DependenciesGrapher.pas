unit DependenciesGrapher;

interface

uses Winapi.Windows, System.SysUtils, System.Classes;

type
  TDependenciesGrapher = class
  private
    unitForAnalyse : TStringList;
    unitAnalysed : TStringList;
    nonSystemUnit : TStringList;
    nonSystemUnitArrayLength : integer;
    nonSystemUnitFile : TStringList;
    graph : array of array of integer;
    f : TextFile;
    procedure ScanDpr( filename : string );
    function ReadWord() : string;
    procedure ScanForAllUnits( path : string );
    procedure ScanForUnitsInDir( path : string );
    procedure ScanUnitName( filename : string );
    procedure Analyse();
    procedure ScanForDep( path : string; id : integer );
    function GetUnitId( name : string ) : integer;
    function IsUnitInAnalyseList( name : string ) : boolean;
    function IsUnitAnalysed( name : string ) : boolean;
    procedure SkipToUses();
    procedure ReadDepAndFillGraph( id : integer; dep_type : integer );
  public
    constructor Create();
    procedure BuildGraph( pathToDpr : string );
    procedure PrintOutput( path : string );
  end;

implementation


constructor TDependenciesGrapher.Create();
begin
  inherited Create;
  unitForAnalyse := TStringList.Create();
  unitAnalysed := TStringList.Create();
  nonSystemUnit := TStringList.Create();
  nonSystemUnitFile := TStringList.Create();
end;

procedure TDependenciesGrapher.BuildGraph( pathToDpr : string );
var path : string;
    i : integer;
begin
  ScanDpr( pathToDpr );
  path := ExtractFilePath( pathToDpr );
  ScanForAllUnits( path );
  Analyse();
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
          unitForAnalyse.Add( str );
        end;
      end;

  until SameText(str, 'begin');
  unitForAnalyse.Delete( unitForAnalyse.Count - 1 );
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
var c, temp_c : char;
    str : string;
    temp : integer;
begin
    if EOF(f) then
    begin
      ReadWord := '';
      exit;
    end;
    while( eoln( f ) ) and (not EOF(f)) do
      readln( f );
    c := ' ';
    while( c = ' ' ) and (not EOF(f)) do
    begin
      Read(f, c);
    end;

    if EOF( f ) then
    begin
      ReadWord := '';
      exit;
    end;

    temp := integer(c);
    if temp < 30 then
    begin
      ReadWord := ReadWord;
      exit;
    end;

    if ( c <> ' ' ) and ( eoln(f) ) then
    begin
      str := str + c;
    end
    else
    begin
      while ( c <> ' ' ) and ( not eoln(f) ) and (not EOF(f)) and ( c <> #10 ) do
      begin
        str := str + c;
        Read(f, c);
        temp := integer(c);
      end;
      if (c <> ' ' )and ( temp <> 10 ) then
        str := str + c;
    end;

    if str[1] = '{' then
    begin
      if pos( '}', str ) = 0 then
        repeat
          Read(f, c);
        until c = '}';
      ReadWord := ReadWord;
      exit;
    end;

    if length( str ) > 1 then
      if ( str[1] = '/' ) and ( str[2] = '/' ) then
      begin
        repeat
          Read(f, c);
        until (c = #10) or eoln(f);
        ReadWord := ReadWord;
        exit;
      end;

    if length( str ) > 1 then
      if ( str[1] = '(' ) and ( str[2] = '*' ) then
      begin
        repeat
          temp_c := c;
          Read(f, c);
        until ( temp_c = '*' ) and ( c = ')' );
        ReadWord := ReadWord;
        exit;
      end;

  ReadWord := str;
end;

procedure TDependenciesGrapher.ScanForAllUnits( path : string );
var
    searchRec : TSearchRec;
begin
  ScanForUnitsInDir( path );

  searchRec.Name := '';
  FindFirst( path + '*', faDirectory, searchRec );  // searchRec.Name = '.'
  FindNext( searchRec );  // searchRec.Name = '..'
  while FindNext( searchRec ) = 0 do
  begin
    ScanForUnitsInDir( path + searchRec.Name + '\' );
  end;
end;

procedure TDependenciesGrapher.ScanForUnitsInDir( path : string );
var fileData : TWIN32FindData;
    seachInfo : WinAPI.Windows.THandle;
begin
  fileData.cFileName := '';
  seachInfo := FindFirstFile( pchar(path + '*.pas'), fileData );
  if seachInfo = INVALID_HANDLE_VALUE then
    exit;
  ScanUnitName( path + fileData.cFileName );

  while FindNextFile( seachInfo, fileData )  do
  begin
    ScanUnitName( path + fileData.cFileName );
  end;
end;

procedure TDependenciesGrapher.ScanUnitName( filename : string );
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
  nonSystemUnit.Add( str );
  nonSystemUnitFile.Add( filename );

  Close( f );
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
