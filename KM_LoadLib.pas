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
  fTextLibrary:TTextLibrary;


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
  f:file; NumRead:integer;
  LIBFile: file of char;
  i, i2, i3, StrCount, Byte1, Byte2, LastStrLen, LastFirstFFIndex, StrLen, TheIndex, ExtraCount: integer;
  FileData: array[0..100000] of byte;
  TheString: string;
  LastWasFF: boolean;
  c: char;
begin
  {
  By reading this code you will probably think that I'm crazy. But all the weird stuff
  with the so called "FF byte pairs" is actually needed. If I just load everything in
  order then stuff won't be correct and it WILL cause us problems later on. Just trust
  me on this one, I spent a long time making a tool to edit these files so I DO know
  what I'm talking about. ;)
  }
  if not CheckFileExists(FilePath) then exit;
  //@Lewin: Replaced your code. Saved us 200ms on loading :)
  //Reading files byte-by-byte is the slowest thing ever.
  //Always try to read whole thing in one chunk into memory first and then process it
  //NumRead holds Count read from file, it's generally unused, but allows to avoid GetFileSize thing

  //@Krom: Thanks for that, I think I understand BlockRead/Write now. You're right, it's far more effecient
  //than doing it one byte at a time, but that was all I knew. I have cleaned up the code below so that it
  //no longer reopens the file. It just loads stuff out of FileData. LIB loading is now down to 50ms. :) To be deleted...
  assignfile(f,FilePath); reset(f,1);
  blockread(f,FileData,100000,NumRead);
  closefile(f);

  //Load string count from first two bytes
  StrCount := FileData[0] + (FileData[1] * 256);
  //Load the length of the last string which is stored here
  LastStrLen := FileData[2];
  
  //Now starts the indexes, set some defaults then run a loop
  ExtraCount := 1;
  LastWasFF := false;
  LastFirstFFIndex := 1;
  for i3 := 1 to StrCount do
  begin
    //Load index bytes for this string
    Byte1 := FileData[8+((i3-1)*2)];
    Byte2 := FileData[9+((i3-1)*2)];
    //Check for FF byte pars
    if (Byte1 = 255) and (Byte2 = 255) then
    begin
      //This string is unused, meaning we must store it as blank, but also for some extreamly
      //annoying reason they also change the order of bytes around them (don't ask...)
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

  //@Lewin: Here I suggest you organise a temp list of new strings
  //TextStrings[1025]:='Activity';
  //TextStrings[1026]:='Building';
  //TextStrings[1027]:='Digging';
  //TextStrings[1028]:='Delivering';
  //etc..
  //Once we settle the design and test it ingame we could either export it to lib files
  //or make an addon lib file especially for Remake (I like that better) - addon.lib
  //Your ideas?

  //@Krom: Temp list of strings is a good idea, but only use it for things that are likely
  //to become perminate. (not just debugging stuff)
  //I too like the addon.lib idea better. Once we have a more complete list then I can organise that
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
  //Here we will export all of the text to a file
  FileData := TStringList.Create;
  if FileExists(AFileName) then DeleteFile(AFileName);
  for i:= 0 to MaxStrings do
    FileData.Add(IntToStr(i)+': '+ALibrary[i]);
  FileData.SaveToFile(AFileName);
  FileData.Free;
end;

procedure TTextLibrary.ExportTextLibraries;
begin
  //Here we will export all of the text to LIB_Setup.txt and LIB_Text.txt
  ExportTextLibrary(SetupStrings,ExeDir+'LIB_Setup.txt');
  ExportTextLibrary(TextStrings,ExeDir+'LIB_Text.txt');
end;

end.
