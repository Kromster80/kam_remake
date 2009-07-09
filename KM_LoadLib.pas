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
  siTrackNames = 570;

type
  TTextLibrary = class(TObject)
  private     { Private declarations }
    TextStrings: array[0..MaxStrings] of string;
    SetupStrings: array[0..MaxStrings] of string;  
    procedure LoadLIBFile(FilePath:string; var aArray:array of string);
    procedure ExportTextLibrary(var aLibrary: array of string; aFileName:string);
  public      { Public declarations } 
    constructor Create(aLibPath: string);
    function GetTextString(aIndex:integer):string;
    function GetSetupString(aIndex:integer):string;
    procedure ExportTextLibraries;
end;

var
  fTextLibrary:TTextLibrary;


implementation
uses KM_Defaults, KM_Settings;

constructor TTextLibrary.Create(aLibPath: string);
begin
  inherited Create;
  LoadLIBFile(aLibPath+'text.'+fGameSettings.GetLocale+'.lib',TextStrings);
  LoadLIBFile(aLibPath+'setup.'+fGameSettings.GetLocale+'.lib',SetupStrings);
end;

procedure TTextLibrary.LoadLIBFile(FilePath:string; var aArray:array of string);
var             
  f:file; NumRead:integer;
  i2, i3, StrCount, Byte1, Byte2, LastStrLen, LastFirstFFIndex, StrLen, TheIndex, ExtraCount: integer;
  FileData: array[0..100000] of byte;
  TheString: string;
  LastWasFF: boolean;
begin
  {
  By reading this code you will probably think that I'm crazy. But all the weird stuff
  with the so called "FF byte pairs" is actually needed. If I just load everything in
  order then stuff won't be correct and it WILL cause us problems later on. Just trust
  me on this one, I spent a long time making a tool to edit these files so I DO know
  what I'm talking about. ;)
  }
  if not CheckFileExists(FilePath) then exit;

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
      aArray[i3] := ''; //Make it blank
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
      aArray[TheIndex-1] := TheString;
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

  //@Lewin: Okay, I guess it will be hardcoded until we export it to LIB.
  //fTextLibrary.GetRemakeString(*)
  // RemakeStrings[1]:='Activity';
  // etc.. I will look into this when we have more menu options..
end;
           
function TTextLibrary.GetTextString(aIndex:integer):string;
begin
  if aIndex < MaxStrings then Result := TextStrings[aIndex];
end;

function TTextLibrary.GetSetupString(aIndex:integer):string;
begin
  if aIndex < MaxStrings then Result := SetupStrings[aIndex];
end;

procedure TTextLibrary.ExportTextLibrary(var aLibrary: array of string; aFileName:string);
var
  i: integer;
  FileData: TStringList;
begin
  //Here we will export all of the text to a file
  FileData := TStringList.Create;
  if FileExists(aFileName) then DeleteFile(aFileName);
  for i:= 0 to MaxStrings do
    FileData.Add(IntToStr(i)+': '+aLibrary[i]);
  FileData.SaveToFile(aFileName);
  FileData.Free;
end;

procedure TTextLibrary.ExportTextLibraries;
begin
  //Here we will export all of the text to LIB_Setup.txt and LIB_Text.txt
  CreateDir(ExeDir+'Export\');
  ExportTextLibrary(SetupStrings,ExeDir+'Export\LIB_Setup.txt');
  ExportTextLibrary(TextStrings,ExeDir+'Export\LIB_Text.txt');
end;

end.
