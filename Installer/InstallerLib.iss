; Knights and Merchants: Remake
; Installer Script
#define MyAppName 'KaM Remake'
#define MyAppExeName 'KaM_Remake.exe';
#define Website 'http://www.kamremake.com/'

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
DefaultDirName={sd}\{#MyAppName}
LicenseFile=License.eng.txt
DisableProgramGroupPage=yes
OutputDir=Output
OutputBaseFilename={#OutputEXE}_{#Revision}
Compression=lzma
SolidCompression=no
Uninstallable=yes
;WizardImageFile=D:\Accessories\Inno Setup 5\Projects\1.60 Patch\Beta 7\ENG\Images\WizImage.bmp
;WizardSmallImageFile=D:\Accessories\Inno Setup 5\Projects\1.60 Patch\Beta 7\ENG\Images\WizSmallImage.bmp
  
[Languages]  
Name: "eng"; MessagesFile: "compiler:Default.isl";
Name: "cze"; MessagesFile: "compiler:Languages\Czech.isl"; LicenseFile: "License.cze.txt"
Name: "dut"; MessagesFile: "compiler:Languages\Dutch.isl"; LicenseFile: "License.dut.txt"
Name: "fre"; MessagesFile: "compiler:Languages\French.isl"; LicenseFile: "License.fre.txt"
Name: "ger"; MessagesFile: "compiler:Languages\German.isl"; LicenseFile: "License.ger.txt"
Name: "hun"; MessagesFile: "compiler:Languages\Hungarian.isl"; LicenseFile: "License.hun.txt"
Name: "pol"; MessagesFile: "compiler:Languages\Polish.isl"; LicenseFile: "License.pol.txt"
Name: "rus"; MessagesFile: "compiler:Languages\Russian.isl"; LicenseFile: "License.rus.txt"
Name: "ita"; MessagesFile: "compiler:Languages\Italian.isl"; LicenseFile: "License.ita.txt"
Name: "svk"; MessagesFile: "compiler:Languages\Slovak.isl";
Name: "spa"; MessagesFile: "compiler:Languages\Spanish.isl"; LicenseFile: "License.spa.txt"
Name: "swe"; MessagesFile: "ExtraLanguages\Swedish.isl"; LicenseFile: "License.swe.txt"
Name: "ptb"; MessagesFile: "ExtraLanguages\BrazilianPortuguese.isl"; LicenseFile: "License.ptb.txt"
Name: "bul"; MessagesFile: "ExtraLanguages\Bulgarian.isl"; LicenseFile: "License.bul.txt"
Name: "est"; MessagesFile: "ExtraLanguages\Estonian.isl"; LicenseFile: "License.est.txt"

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
var Warnings:string;
begin
  Warnings := '';
  if not CheckKaM() then
    Warnings := ExpandConstant('{cm:NoKaM}');
  
  if not CanInstall() then
  begin
    if Warnings <> '' then
      Warnings := Warnings + '' + #13#10#13#10; //Two EOLs between messages
    Warnings := Warnings + ExpandConstant('{cm:CantUpdate}')
  end;
  
  if Warnings = '' then
    Result := True
  else
  begin
    Result := False;
    MsgBox(Warnings, mbInformation, MB_OK);
  end;
end;

//This event is executed right after installing, use this time to install OpenAL
function NeedRestart(): Boolean;
var ResultCode: Integer; MyText:String;
begin
  Result := false; //We never require a restart, this is just a handy event for post install

  //First create the ini file with the right language selected
  MyText := '[Game]'+#13+#10+'Locale='+ExpandConstant('{language}');
  SaveStringToFile(ExpandConstant('{app}\KaM_Remake_Settings.ini'), MyText, False);
  
  //Now install OpenAL, if needed
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
