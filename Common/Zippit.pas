{
  Extended by George Mizzi - 7.15.05

    I Fixed+Extended Patrik Spanel's TZipFile
    into one small-footprint and easy-to-use
    zipping class.


  Modified by Lewin Hodgman - 7th July 2011

    I added the PathOnZip parameter to AddFile(s) which is blank by default.
    Added example of usage below

  ----------------
  EXAMPLE OF USAGE
  ----------------
  MyZip := TZippit.Create;
  MyZip.AddFile('C:\folder\myfile.ext');
  MyZip.AddFiles('C:\folder2\*.pas');
  MyZip.SaveToFile('C:\folder3\test.zip');
  MyZip.Free;
}

unit Zippit;

interface

uses
  Classes, SysUtils, SciZipFile;

type
  TZippit = class(TZipFile)
  private
  public
    procedure AddFiles(const Pattern: String; const PathOnZip:string=''; Recurse: Boolean = True);
    procedure AddFile(const Name: String; PathOnZip:string=''); overload;
  end;

implementation

{ Utils }

procedure ListPaths(Path: String; Paths: TStrings; Recurse: Boolean);
var
  SR: TSearchRec;
begin
  if FindFirst(Path + '*.*', faDirectory, SR) <> 0 then
    Exit;

  repeat
    if (SR.Attr and faDirectory <> 0) and
      (SR.Name <> '.') and
      (SR.Name <> '..') then
    begin
      Paths.Add(Path + SR.Name + '\');
      if Recurse then ListPaths(Path + SR.Name + '\', Paths, Recurse);
    end;
  until FindNext(SR) <> 0;

  FindClose(SR);
end;

procedure ListFiles(Pattern: String; Files: TStrings; Recurse: Boolean);

  procedure List(Pattern: String; Files: TStrings);
  var
    SR: TSearchRec;
  begin
    if FindFirst(Pattern, faAnyFile, SR) <> 0 then
      Exit;

    repeat
      if (SR.Attr and faDirectory = 0) and
        (SR.Name <> '.') and
        (SR.Name <> '..') then
        Files.Add(ExtractFilePath(Pattern) + SR.Name);
    until FindNext(SR) <> 0;

    FindClose(SR);
  end;

var
  Paths: TStringList;
  FileName: String;
  I: Integer;
begin
  Paths := TStringList.Create;
  FileName := ExtractFileName(Pattern);
  try
    List(Pattern, Files);
    if Recurse then
    begin
      ListPaths(ExtractFilePath(Pattern), Paths, Recurse);
      for I := 0 to Paths.Count - 1 do
        List(Paths[I] + FileName, Files);
    end;
  finally
    Paths.Free;
  end;
end;

{ TZippit }

procedure TZippit.AddFile(const Name: String; PathOnZip:string='');
var
  F: TFileStream;
  B: AnsiString;
begin
  if not FileExists(Name) then exit;
  if PathOnZip <> '' then PathOnZip := SysUtils.IncludeTrailingBackslash(PathOnZip);
  F := TFileStream.Create(Name, fmOpenRead);
  try
    SetLength(B, F.Size);
    F.ReadBuffer(B[1], F.Size);
    inherited AddFile(PathOnZip + ExtractFileName(Name), 0);
    Data[Count-1] := B;
  finally
    F.Free;
  end;
end;

procedure TZippit.AddFiles(const Pattern: String; const PathOnZip:string=''; Recurse: Boolean = True);
var
  L: TStringList;
  I: Integer;
begin
  L := TStringList.Create;
  try
  	ListFiles(Pattern, L, Recurse);
    for I := 0 to L.Count - 1 do
	  	AddFile(L[I],PathOnZip);
  finally
    L.Free;
  end;
end;

end.
