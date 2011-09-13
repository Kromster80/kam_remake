; Knights and Merchants: Remake
; Installer Script
#define MyAppName 'KaM Remake'
#define MyAppExeName 'KaM_Remake.exe';
#define Website 'http://krom.reveur.de/index.php?t=kam_remake'

;http://stfx-wow.googlecode.com/svn-history/r418/trunk/NetFxIS/setup.iss
;http://tdmaker.googlecode.com/svn/trunk/Setup/tdmaker-anycpu.iss
;http://tdmaker.googlecode.com/svn/trunk/Setup/scripts/products.iss

[Setup]
AppId={{FDE049C8-E4B2-4EB5-A534-CF5C581F5D32}
AppName={#MyAppName}
AppVerName={#MyAppName} {#InstallType} {#Revision}
AppPublisherURL={#Website}
AppSupportURL={#Website}
AppUpdatesURL={#Website}
DefaultDirName={pf}\{#MyAppName}
LicenseFile=License.en.txt
DisableProgramGroupPage=yes
OutputDir=Output
OutputBaseFilename={#OutputEXE}_{#Revision}
Compression=lzma
SolidCompression=yes
Uninstallable=yes
;WizardImageFile=D:\Accessories\Inno Setup 5\Projects\1.60 Patch\Beta 7\ENG\Images\WizImage.bmp
;WizardSmallImageFile=D:\Accessories\Inno Setup 5\Projects\1.60 Patch\Beta 7\ENG\Images\WizSmallImage.bmp
  
[Languages]  
Name: "eng"; MessagesFile: "compiler:Default.isl";
Name: "cze"; MessagesFile: "compiler:Languages\Czech.isl";
Name: "dut"; MessagesFile: "compiler:Languages\Dutch.isl";
Name: "fre"; MessagesFile: "compiler:Languages\French.isl";
Name: "ger"; MessagesFile: "compiler:Languages\German.isl"; LicenseFile: "License.de.txt"
Name: "hun"; MessagesFile: "compiler:Languages\Hungarian.isl";
Name: "pol"; MessagesFile: "compiler:Languages\Polish.isl";
Name: "rus"; MessagesFile: "compiler:Languages\Russian.isl";
Name: "svk"; MessagesFile: "compiler:Languages\Slovak.isl";
Name: "spa"; MessagesFile: "compiler:Languages\Spanish.isl";
Name: "swe"; MessagesFile: "ExtraLanguages\Swedish.isl";

[CustomMessages]  
#include "Translations.iss"


[Registry]
Root: HKLM; Subkey: "SOFTWARE\JOYMANIA Entertainment\KnightsandMerchants TPR"; ValueType: string; ValueName: "RemakeVersion"; ValueData: {#Revision}; Flags:uninsdeletevalue;

[Run]
Filename: "{code:GetReadmeLang}";  Description: {cm:ViewReadme};  Flags: postinstall shellexec skipifsilent
Filename: "{app}\{#MyAppExeName}"; Description: {cm:LaunchProgram,{#MyAppName}}; Flags: postinstall nowait skipifsilent unchecked

[Code]
#include "CheckKaM.iss"

//Executed before the wizard appears, allows us to check that they have KaM installed
function InitializeSetup(): Boolean;
begin
  if CheckKaM() then
  begin
    if CanInstall() then
    begin
      Result := True
    end
    else  
    begin
      Result := False;
      MsgBox(ExpandConstant('{cm:CantUpdate}'), mbInformation, MB_OK);
    end;
  end
  else
  begin
    Result := False;
    MsgBox(ExpandConstant('{cm:NoKaM}'), mbInformation, MB_OK);
  end;
end;

//This event is executed right after installing, use this time to install OpenAL
function NeedRestart(): Boolean;
var ResultCode: Integer;
begin
  Result := false; //We never require a restart, this is just a handy event for post install
  if FileExists(ExpandConstant('{sys}')+'\OpenAL32.dll') then exit; //User already has OpenAL installed
  if MsgBox(ExpandConstant('{cm:OpenAL}'), mbConfirmation, MB_YESNO) = idYes then
  begin
    Exec(ExpandConstant('{app}\oalinst.exe'), '/S', '', SW_SHOW, ewWaitUntilTerminated, ResultCode);
  end;
end;

function GetReadmeLang(Param: String): string;
begin
  Result := ExpandConstant('{app}\Readme_{language}.html'); //Use the user's language if possible
  if not FileExists(Result) then
    Result := ExpandConstant('{app}\Readme_eng.html'); //Otherwise use English
end;

[Files]
Source: "{#BuildFolder}\*"; DestDir: "{app}"; Excludes: "*.svn,*.svn\*"; Flags: ignoreversion recursesubdirs createallsubdirs
Source: "oalinst.exe"; DestDir: "{app}"; Flags: ignoreversion

[Tasks]
Name: programgroup; Description: {cm:CreateStartShortcut};
Name: desktopicon; Description: {cm:CreateDesktopIcon}; Flags:Unchecked

[Icons]
Name: "{commonprograms}\{#MyAppName}\{#MyAppName}"; Filename: "{app}\{#MyAppExeName}"; Tasks: programgroup
Name: "{commonprograms}\{#MyAppName}\{cm:UninstallProgram,{#MyAppName}}"; Filename: "{uninstallexe}"; Tasks: programgroup; Flags: excludefromshowinnewinstall
Name: "{commonprograms}\{#MyAppName}\{cm:ViewReadme}"; Filename: "{code:GetReadmeLang}"; Tasks: programgroup; Flags: excludefromshowinnewinstall
Name: "{commondesktop}\{#MyAppName}"; Filename: "{app}\{#MyAppExeName}"; Tasks: desktopicon
