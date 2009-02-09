unit KM_LoadLib;
interface
uses
  Windows, Classes, SysUtils, KromUtils;

const
  MaxStrings = 999;
  //Here are some indexes into the files for different items. (e.g. unit names) si is for "String Index"
  siHouseNames = 0;
  siResourceNames = 39;
  siUnitNames = 69;
  siUnitDescriptions = 99;

type
  TTextLibrary = class(TObject)
  private     { Private declarations }
    TextStrings: array[0..MaxStrings] of string;
    SetupStrings: array[0..MaxStrings] of string;  
    procedure LoadLIBFile(FilePath:string; var AArray:array of string);
    procedure ExportTextLibrary(var ALibrary: array of string; AFileName:string);
  public      { Public declarations } 
    constructor Create(ALibPath: string);
    function GetTextString(AIndex:integer):string;
    function GetSetupString(AIndex:integer):string;
    procedure ExportTextLibraries;
end;

var
  fTextLibrary: TTextLibrary;

implementation
uses KM_Defaults;

constructor TTextLibrary.Create(ALibPath: string);
begin
  inherited Create;
  LoadLIBFile(ALibPath+'text.lib',TextStrings);
  LoadLIBFile(ALibPath+'setup.lib',SetupStrings);
end;

procedure TTextLibrary.LoadLIBFile(FilePath:string; var AArray:array of string);
var
  LIBFile: file of char;
  i, i2, i3, StrCount, Byte1, Byte2, LastStrLen, LastFirstFFIndex, StrLen, TheIndex, ExtraCount: integer;
  FileData: array[0..100000] of byte;
  TheString: string;
  LastWasFF: boolean;
  c: char;
  b: byte;
begin
  {
  By reading this code you will probably think that I'm crazy. But all the weird stuff
  with the so called "FF byte pars" is actually needed. If I just load everything in
  order then stuff won't be correct and it WILL cause us problems later on. Just trust
  me on this one, I spent a long time making a tool to edit these files so I DO know
  what I'm talking about. ;)
  }
  if not CheckFileExists(FilePath) then exit;
  AssignFile(LIBFile, FilePath);
  Reset(LIBFile);
  //First load entire file into an array 
  i2 := 0;
  while not EOF(LIBFile) do
  begin
    Read(LIBFile, c);  b := ord(c);
    FileData[i2] := b; 
    i2 := i2 + 1;
  end;

  Reset(LIBFile); //Reset the file so we can start reading from the start again
  //Load string count from first two bytes
  Read(LIBFile, c); i := ord(c);
  Read(LIBFile, c);
  StrCount := i + (ord(c) * 256);
  //Load the length of the last string which is stored here
  Read(LIBFile, c); LastStrLen := ord(c);
  //Skip 5 other header bytes which are not needed for reading only
  Read(LIBFile, c);
  Read(LIBFile, c);
  Read(LIBFile, c);
  Read(LIBFile, c);
  Read(LIBFile, c);
  
  //Now starts the indexes, set some defaults then run a loop
  ExtraCount := 1;
  LastWasFF := false;
  LastFirstFFIndex := 1;
  for i3 := 1 to StrCount do
  begin
    //Load index bytes for this string
    Read(LIBFile, c);  Byte1 := ord(c);
    Read(LIBFile, c);  Byte2 := ord(c);
    //Check for FF byte pars
    if (Byte1 = 255) and (Byte2 = 255) then
    begin
      //This string is unused, meaning we must store it as blank, but also for some extreamly annoying reason they also change the order of bytes around them (don't ask...)
      AArray[i3] := ''; //Make it blank
      if LastWasFF = false then
        LastFirstFFIndex := i3;
      LastWasFF := true;
    end
    else
    begin
      StrLen := Byte1 + (Byte2 * 256);
      if i3 = StrCount then //For the last string we must get the length from the header
        StrLen := LastStrLen;

      TheString := '';
      for i2 := ExtraCount to StrLen - 1 do //Extract the string from the main section of the file
      begin
        TheString := TheString + char(FileData[(StrCount * 2) + 5 + i2]);
      end;
      ExtraCount := StrLen + 1;   
      if LastWasFF then  TheIndex := LastFirstFFIndex
      else TheIndex := i3;
      AArray[TheIndex-1] := TheString;
      LastWasFF := false;
    end;
  end;
            
  CloseFile(LIBFile);
end;
           
function TTextLibrary.GetTextString(AIndex:integer):string;
begin
  if AIndex < MaxStrings then Result := TextStrings[AIndex];
end;

function TTextLibrary.GetSetupString(AIndex:integer):string;
begin
  if AIndex < MaxStrings then Result := SetupStrings[AIndex];
end;

procedure TTextLibrary.ExportTextLibrary(var ALibrary: array of string; AFileName:string);
var
  i: integer;
  FileData: TStringList;
begin
  //Here we will export all of the text to text.txt and setup.txt
  FileData := TStringList.Create;
  if FileExists(AFileName) then DeleteFile(AFileName);
  for i:= 0 to MaxStrings do
    FileData.Add(IntToStr(i)+': '+ALibrary[i]);
  FileData.SaveToFile(AFileName);
  FileData.Free;
end;

procedure TTextLibrary.ExportTextLibraries;
begin
  //Here we will export all of the text to text.txt and setup.txt
  ExportTextLibrary(SetupStrings,ExeDir+'LIB_Setup.txt');
  ExportTextLibrary(TextStrings,ExeDir+'LIB_Text.txt');
end;

end.
